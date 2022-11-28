{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- == Node Architecture
--
-- The following [diagram (click for a full-width
-- version)](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/images/hydra-architecture-direct.jpg)
-- represents the internal structure of the Hydra Node and the interactions
-- between its components.
--
-- ![Hydra Architecture](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/images/hydra-architecture-direct_800x.jpg)
--
-- __Legend__:
--
--     * Grayed boxes represent components which are not developed yet
--     * Black boxes represent components which are expected to be used as _black box_, eg. without any knowledge of their inner workings.
--     * Arrows depict the flow of data (Requests, messages, responses...)
--     * We represent some components that are not part of the Hydra node proper for legibility's sake
module Hydra.Node where

import Hydra.Prelude

import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (
  isEmptyTQueue,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey))
import Hydra.Chain (Chain (..), ChainStateType, IsChainState, PostTxError)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  LogicError (..),
  Outcome (..),
  defaultTTL,
  emitSnapshot,
 )
import qualified Hydra.HeadLogic as Logic
import Hydra.Ledger (IsTx, Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Options (RunOptions (..))
import Hydra.Party (Party (..), deriveParty)
import Hydra.Persistence (Persistence (..))

-- * Environment Handling

initEnvironment :: RunOptions -> IO Environment
initEnvironment RunOptions{hydraSigningKey, hydraVerificationKeys} = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
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
  = ErrorHandlingEvent {by :: Party, event :: Event tx, reason :: LogicError tx}
  | BeginEvent {by :: Party, event :: Event tx}
  | EndEvent {by :: Party, event :: Event tx}
  | BeginEffect {by :: Party, effect :: Effect tx}
  | EndEffect {by :: Party, effect :: Effect tx}
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
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node =
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ stepHydraNode tracer node

-- On process les évènements les uns après les autres.
-- Soit on sauvegarde les évènements en entrée avant de faire quoique ce soit
-- Soit on sauvegarde après
-- qu'est-ce que ça veut dire OnlyEffect ?
-- c'est quoi un effect ?
-- C'est quoi la différence avec NewState ?
-- Un effect semble être un effet de bord qu'on souhaite avoir sur le reste du monde
-- Autrement dit, on n'a sans doute pas envie de rejouer un effect lorsqu'on rejoue
-- les évènement précédents.
-- C'est sans doute pour cela qu'on a sauvegardé uniquement le chainstate ici.
-- Il y a trois catégorie d'effect:

-- * ClientEffect -> on va envoyer un message aux clients

-- * NetworkEffect -> on va poster un message sur le réseau

-- * ChainEffect -> on va poster une transaction on-chain

-- j'ai dans l'idée qu'on ne veut vraiment refaire aucun de ces effets, en fait
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
  event <- nextEvent eq
  traceWith tracer $ BeginEvent party event
  atomically (processNextEvent node event) >>= \case
    -- TODO(SN): Handling of 'Left' is untested, i.e. the fact that it only
    -- does trace and not throw!
    Error err -> traceWith tracer (ErrorHandlingEvent party event err)
    -- Que fait wait exactement ?
    Wait _reason -> putEventAfter eq 0.1 (decreaseTTL event) >> traceWith tracer (EndEvent party event)
    -- Super louche : ça peut être NewState et Effects ou OnlyEffects
    -- Du coup on a une duplication de code ici pour l'application des effets
    NewState s effs -> do
      save s
      forM_ effs (processEffect node tracer)
      traceWith tracer (EndEvent party event)
    OnlyEffects effs ->
      forM_ effs (processEffect node tracer) >> traceWith tracer (EndEvent party event)
 where
  decreaseTTL =
    \case
      NetworkEvent ttl msg -> NetworkEvent (ttl - 1) msg
      e -> e

  Environment{party} = env

  Persistence{save} = persistence

  HydraNode{persistence, eq, env} = node

-- | Monadic interface around 'Hydra.Logic.update'.
-- Ce truc va émettre un output en fonction du résultat de Logic.update
-- qu'est-ce qu'on en fait ensuite de cet output ?
-- est-ce qu'il faut sauvegarder l'output ?
-- est-ce qu'il faut sauvegarder l'event ?'
processNextEvent ::
  (IsChainState tx) =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    case Logic.update env ledger s e of
      OnlyEffects effects -> (OnlyEffects effects, s)
      NewState s' effects ->
        let (s'', effects') = emitSnapshot env effects s'
         in (NewState s'' effects', s'')
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
  Effect tx ->
  m ()
processEffect HydraNode{hn, oc = Chain{postTx}, server, eq, env = Environment{party}} tracer effect = do
  traceWith tracer $ BeginEffect party effect
  case effect of
    ClientEffect i -> sendOutput server i
    NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL msg)
    OnChainEffect{chainState, postChainTx} ->
      postTx chainState postChainTx
        `catch` \(postTxError :: PostTxError tx) ->
          putEvent eq $ PostTxError{postChainTx, postTxError}
  traceWith tracer $ EndEffect party effect
-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
-- C'est là qu'on va tout ranger
-- on pourrait en faire une event queue persistente et ce serait cool
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , putEventAfter :: NominalDiffTime -> e -> m ()
  , nextEvent :: m e
  , isEmpty :: m Bool
  }

createEventQueue :: (MonadSTM m, MonadDelay m, MonadAsync m) => m (EventQueue m e)
createEventQueue = do
  numThreads <- newTVarIO (0 :: Integer)
  q <- atomically newTQueue
  pure
    EventQueue
      { putEvent = push q
      , putEventAfter = \delay e -> do
          atomically $ modifyTVar' numThreads succ
          void . async $ do
            threadDelay $ realToFrac delay
            atomically $ do
              modifyTVar' numThreads pred
            push q e
      , nextEvent =
          pop q
      , isEmpty = do
          atomically $ do
            n <- readTVar numThreads
            isEmpty' <- isEmptyTQueue q
            pure (isEmpty' && n == 0)
      }
 where
  pop q = atomically $ readTQueue q
  push q = atomically . writeTQueue q

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  }

-- | Initialize a new 'NodeState'.
createNodeState :: MonadSTM m => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }
