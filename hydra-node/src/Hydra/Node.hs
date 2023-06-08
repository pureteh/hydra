{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/core-concepts/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  newTVarIO,
  stateTVar,
 )
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey))
import Hydra.Chain (Chain (..), ChainStateType, HeadParameters (..), IsChainState, PostTxError)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  ClosedState (..),
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (IdleState),
  InitialState (..),
  OpenState (..),
  Outcome (..),
  defaultTTL,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node.EventQueue (EventQueue (..), Queued (..))
import Hydra.Options (ChainConfig (..), ParamMismatch (..), ParamMismatchError (..), RunOptions (..))
import Hydra.Party (Party (..), deriveParty)
import Hydra.Persistence (Persistence (..))

-- * Environment Handling

initEnvironment :: RunOptions -> IO Environment
initEnvironment RunOptions{hydraSigningKey, hydraVerificationKeys, chainConfig = DirectChainConfig{contestationPeriod}} = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      , contestationPeriod
      }
 where
  loadParty p =
    Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p
-- ** Create and run a hydra node

-- | Main handle of a hydra node where all layers are tied together.
data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: Network m (Message tx)
  , nodeState :: NodeState tx m
  , oc :: Chain tx m
  , server :: Server tx m
  , ledger :: Ledger tx
  , env :: Environment
  , persistence :: Persistence (HeadState tx) m
  }

data HydraNodeLog tx
  = BeginEvent {by :: Party, eventId :: Word64, event :: Event tx}
  | EndEvent {by :: Party, eventId :: Word64}
  | BeginEffect {by :: Party, eventId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, eventId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  | CreatedState
  | LoadedState
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => Show (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (HydraNodeLog tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (HydraNodeLog tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary

runHydraNode ::
  ( MonadThrow m
  , MonadCatch m
  , MonadAsync m
  , IsChainState tx
  , MonadLabelledSTM m
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node@HydraNode{env, persistence} = do
  loadedState <- loadPersistentState tracer env persistence
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ stepHydraNode tracer (maybe node (\nodeState -> node{nodeState}) loadedState)

loadPersistentState ::
  ( MonadThrow m
  , MonadCatch m
  , MonadAsync m
  , IsChainState tx
  , MonadLabelledSTM m
  ) =>
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  Persistence (HeadState tx) m ->
  m (Maybe (NodeState tx m))
loadPersistentState tracer env persistence = do
  load persistence >>= \case
    Nothing -> pure Nothing
    Just headState -> do
      traceWith tracer LoadedState
      let paramsMismatch = checkParamsAgainstExistingState headState env
      unless (null paramsMismatch) $ do
        traceWith tracer (Misconfiguration paramsMismatch)
        throwIO $ ParamMismatchError paramsMismatch
      Just <$> createNodeState headState

-- check if hydra-node parameters are matching with the hydra-node state.
checkParamsAgainstExistingState :: HeadState tx -> Environment -> [ParamMismatch]
checkParamsAgainstExistingState hs env =
  -- TODO: test me (See end2endTest)
  case hs of
    Idle _ -> []
    Initial InitialState{parameters} -> validateParameters parameters
    Open OpenState{parameters} -> validateParameters parameters
    Closed ClosedState{parameters} -> validateParameters parameters
 where
  validateParameters HeadParameters{contestationPeriod = loadedCp, parties} =
    flip execState [] $ do
      when (loadedCp /= configuredCp) $
        modify (<> [ContestationPeriodMismatch{loadedCp, configuredCp}])
      when (loadedParties /= configuredParties) $
        modify (<> [PartiesMismatch{loadedParties, configuredParties}])
   where
    loadedParties = sort parties

  Environment{contestationPeriod = configuredCp, otherParties, party} = env
  configuredParties = sort (party : otherParties)

stepHydraNode ::
  ( MonadThrow m
  , MonadCatch m
  , MonadAsync m
  , IsChainState tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node = do
  e@Queued{eventId, queuedEvent} <- nextEvent eq
  traceWith tracer $ BeginEvent{by = party, eventId, event = queuedEvent}
  outcome <- atomically (processNextEvent node queuedEvent)
  traceWith tracer (LogicOutcome party outcome)
  effs <- case outcome of
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error _ -> do
      pure []
    Wait _reason -> do
      putEventAfter eq waitDelay (decreaseTTL e)
      pure []
    NewState s effs -> do
      save s
      pure effs
    OnlyEffects effs -> do
      pure effs
  mapM_ (uncurry $ flip $ processEffect node tracer) $ zip effs (map (eventId,) [0 ..])
  traceWith tracer EndEvent{by = party, eventId}
 where
  decreaseTTL =
    \case
      -- XXX: this is smelly, handle wait re-enqueing differently
      Queued{eventId, queuedEvent = NetworkEvent ttl msg}
        | ttl > 0 -> Queued{eventId, queuedEvent = NetworkEvent (ttl - 1) msg}
      e -> e

  Environment{party} = env

  Persistence{save} = persistence

  HydraNode{persistence, eq, env} = node

-- | The time to wait between re-enqueuing a 'Wait' outcome from 'HeadLogic'.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  (IsChainState tx) =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    case Logic.update env ledger s e of
      OnlyEffects effects -> (OnlyEffects effects, s)
      NewState s' effects -> (NewState s' effects, s')
      Error err -> (Error err, s)
      Wait reason -> (Wait reason, s)
 where
  NodeState{modifyHeadState} = nodeState

processEffect ::
  ( MonadAsync m
  , MonadCatch m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  (Word64, Word32) ->
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc = Chain{postTx}, server, eq, env = Environment{party}} tracer (eventId, effectId) e = do
  traceWith tracer $ BeginEffect party eventId effectId e
  case e of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL msg)
    OnChainEffect{postChainTx} ->
      postTx postChainTx
        `catch` \(postTxError :: PostTxError tx) ->
          putEvent eq $ PostTxError{postChainTx, postTxError}
  traceWith tracer $ EndEffect party eventId effectId

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  }

-- | Initialize a new 'NodeState'.
createNodeState :: (MonadSTM m, MonadLabelledSTM m) => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  labelTVarIO tv "node-state"
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }
