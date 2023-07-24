{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Implements the Head Protocol's /state machine/ as a /pure function/.
--
-- The protocol is described in two parts in the [Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/)
--
--     * One part detailing how the Head deals with /client input/.
--     * Another part describing how the Head reacts to /network messages/ from peers.
--     * A third part detailing the /On-Chain Verification (OCV)/ protocol, i.e. the abstract "smart contracts" that are need to provide on-chain security.
--
-- This module is about the first two parts, while the "Hydra.Contract.Head" module in 'hydra-plutus' covers the third part.
module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (
  ChainEvent (..),
  ChainStateType,
  HeadId,
  HeadParameters (..),
  IsChainState (chainStateSlot),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError,
 )
import Hydra.ContestationPeriod
import Hydra.Crypto (
  HydraKey,
  Signature,
  SigningKey,
  VerificationKey,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyMultiSignature,
 )
import Hydra.Ledger (
  ChainSlot,
  IsTx,
  Ledger (..),
  TxIdType,
  UTxOType,
  ValidationError,
  applyTransactions,
  txId,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party (vkey))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

-- * Types

-- TODO: Move logic up and types down or re-organize using explicit exports

-- | The different events which are processed by the head logic (the "core").
-- Corresponding to each of the "shell" layers, we distinguish between events
-- from the client, the network and the chain.
data Event tx
  = -- | Event received from clients via the "Hydra.API".
    ClientEvent {clientInput :: ClientInput tx}
  | -- | Event received from peers via a "Hydra.Network".
    --
    --  * `ttl` is a simple counter that's decreased every time the event is
    --    reenqueued due to a wait. It's default value is `defaultTTL`
    NetworkEvent {ttl :: TTL, party :: Party, message :: Message tx}
  | -- | Event received from the chain via a "Hydra.Chain".
    OnChainEvent {chainEvent :: ChainEvent tx}
  | -- | Event to re-ingest errors from 'postTx' for further processing.
    PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (Event tx)
deriving instance (IsChainState tx) => Show (Event tx)
deriving instance (IsChainState tx) => ToJSON (Event tx)
deriving instance (IsChainState tx) => FromJSON (Event tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Event tx)
  where
  arbitrary = genericArbitrary

-- | Analogous to events, the pure head logic "core" can have effects emited to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ServerOutput'.
    ClientEffect {serverOutput :: ServerOutput tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (Effect tx)
deriving instance (IsChainState tx) => Show (Effect tx)
deriving instance (IsChainState tx) => ToJSON (Effect tx)
deriving instance (IsChainState tx) => FromJSON (Effect tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Effect tx)
  where
  arbitrary = genericArbitrary

-- | The main state of the Hydra protocol state machine. It holds both, the
-- overall protocol state, but also the off-chain 'CoordinatedHeadState'.
--
-- Each of the sub-types (InitialState, OpenState, etc.) contain black-box
-- 'chainState' corresponding to 'OnChainEvent' that has been observed leading
-- to the state.
--
-- Note that rollbacks are currently not fully handled in the head logic and
-- only this internal chain state gets replaced with the "rolled back to"
-- version.
data HeadState tx
  = Idle (IdleState tx)
  | Initial (InitialState tx)
  | Open (OpenState tx)
  | Closed (ClosedState tx)
  deriving stock (Generic)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (HeadState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (HeadState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (HeadState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (HeadState tx)

-- | Get the chain state in any 'HeadState'.
getChainState :: HeadState tx -> ChainStateType tx
getChainState = \case
  Idle IdleState{chainState} -> chainState
  Initial InitialState{chainState} -> chainState
  Open OpenState{chainState} -> chainState
  Closed ClosedState{chainState} -> chainState

-- | Update the chain state in any 'HeadState'.
setChainState :: ChainStateType tx -> HeadState tx -> HeadState tx
setChainState chainState = \case
  Idle st -> Idle st{chainState}
  Initial st -> Initial st{chainState}
  Open st -> Open st{chainState}
  Closed st -> Closed st{chainState}

-- ** Idle

-- | An 'Idle' head only having a chain state with things seen on chain so far.
newtype IdleState tx = IdleState {chainState :: ChainStateType tx}
  deriving (Generic)

deriving instance Eq (ChainStateType tx) => Eq (IdleState tx)
deriving instance Show (ChainStateType tx) => Show (IdleState tx)
deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (IdleState tx)
deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (IdleState tx)

instance (Arbitrary (ChainStateType tx)) => Arbitrary (IdleState tx) where
  arbitrary = genericArbitrary

-- ** Initial

-- | An 'Initial' head which already has an identity and is collecting commits.
data InitialState tx = InitialState
  { parameters :: HeadParameters
  , pendingCommits :: PendingCommits
  , committed :: Committed tx
  , chainState :: ChainStateType tx
  , headId :: HeadId
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (InitialState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (InitialState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (InitialState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (InitialState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (InitialState tx) where
  arbitrary = do
    InitialState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

type PendingCommits = Set Party

type Committed tx = Map Party (UTxOType tx)

-- ** Open

-- | An 'Open' head with a 'CoordinatedHeadState' tracking off-chain
-- transactions.
data OpenState tx = OpenState
  { parameters :: HeadParameters
  , coordinatedHeadState :: CoordinatedHeadState tx
  , chainState :: ChainStateType tx
  , headId :: HeadId
  , currentSlot :: ChainSlot
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (OpenState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (OpenState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (OpenState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (OpenState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (OpenState tx) where
  arbitrary =
    OpenState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Off-chain state of the Coordinated Head protocol.
data CoordinatedHeadState tx = CoordinatedHeadState
  { localUTxO :: UTxOType tx
  -- ^ The latest UTxO resulting from applying 'localTxs' to
  -- 'confirmedSnapshot'. Spec: L̂
  , localTxs :: [tx]
  -- ^ List of transactions applied locally and pending inclusion in a snapshot. Spec: T̂
  , allTxs :: Map.Map (TxIdType tx) tx
  -- ^ Map containing all the transactions ever seen by this node and not yet
  -- included in a snapshot. Spec: Tall
  , confirmedSnapshot :: ConfirmedSnapshot tx
  -- ^ The latest confirmed snapshot. Spec: U̅, s̅ and σ̅
  , seenSnapshot :: SeenSnapshot tx
  -- ^ Last seen snapshot and signatures accumulator. Spec: Û, ŝ and Σ̂
  }
  deriving stock (Generic)

deriving instance IsTx tx => Eq (CoordinatedHeadState tx)
deriving instance IsTx tx => Show (CoordinatedHeadState tx)
deriving instance IsTx tx => ToJSON (CoordinatedHeadState tx)
deriving instance IsTx tx => FromJSON (CoordinatedHeadState tx)

instance IsTx tx => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

-- | Data structure to help in tracking whether we have seen or requested a
-- ReqSn already and if seen, the signatures we collected already.
data SeenSnapshot tx
  = -- | Never saw a ReqSn.
    NoSeenSnapshot
  | -- | No snapshot in flight with last seen snapshot number as given.
    LastSeenSnapshot {lastSeen :: SnapshotNumber}
  | -- | ReqSn was sent out and it should be considered already in flight.
    RequestedSnapshot
      { lastSeen :: SnapshotNumber
      , requested :: SnapshotNumber
      }
  | -- | ReqSn for given snapshot was received.
    SeenSnapshot
      { snapshot :: Snapshot tx
      , signatories :: Map Party (Signature (Snapshot tx))
      -- ^ Collected signatures and so far.
      }
  deriving stock (Generic)

instance IsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (SeenSnapshot tx)
deriving instance IsTx tx => Show (SeenSnapshot tx)
deriving instance IsTx tx => ToJSON (SeenSnapshot tx)
deriving instance IsTx tx => FromJSON (SeenSnapshot tx)

-- | Get the last seen snapshot number given a 'SeenSnapshot'.
seenSnapshotNumber :: SeenSnapshot tx -> SnapshotNumber
seenSnapshotNumber = \case
  NoSeenSnapshot -> 0
  LastSeenSnapshot{lastSeen} -> lastSeen
  RequestedSnapshot{lastSeen} -> lastSeen
  SeenSnapshot{snapshot = Snapshot{number}} -> number

-- ** Closed

-- | An 'Closed' head with an current candidate 'ConfirmedSnapshot', which may
-- be contested before the 'contestationDeadline'.
data ClosedState tx = ClosedState
  { parameters :: HeadParameters
  , confirmedSnapshot :: ConfirmedSnapshot tx
  , contestationDeadline :: UTCTime
  , readyToFanoutSent :: Bool
  -- ^ Tracks whether we have informed clients already about being
  -- 'ReadyToFanout'.
  , chainState :: ChainStateType tx
  , headId :: HeadId
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ClosedState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (ClosedState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ClosedState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ClosedState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ClosedState tx) where
  arbitrary =
    ClosedState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- ** Other types

type TTL = Natural

defaultTTL :: TTL
defaultTTL = 5

-- | Preliminary type for collecting errors occurring during 'update'.
-- TODO: Try to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | InvalidSnapshot {expected :: SnapshotNumber, actual :: SnapshotNumber}
  | LedgerError ValidationError
  | RequireFailed (RequirementFailure tx)
  | NotOurHead {ourHeadId :: HeadId, otherHeadId :: HeadId}
  deriving stock (Generic)

instance (Typeable tx, Show (Event tx), Show (HeadState tx), Show (RequirementFailure tx)) => Exception (LogicError tx)

instance (Arbitrary (Event tx), Arbitrary (HeadState tx), Arbitrary (RequirementFailure tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance (Eq (HeadState tx), Eq (Event tx), Eq (RequirementFailure tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx), Show (RequirementFailure tx)) => Show (LogicError tx)
deriving instance (ToJSON (HeadState tx), ToJSON (Event tx), ToJSON (RequirementFailure tx)) => ToJSON (LogicError tx)
deriving instance (FromJSON (HeadState tx), FromJSON (Event tx), FromJSON (RequirementFailure tx)) => FromJSON (LogicError tx)

data RequirementFailure tx
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SnapshotDoesNotApply {requestedSn :: SnapshotNumber, txid :: TxIdType tx, error :: ValidationError}
  deriving stock (Generic)

deriving instance (Eq (TxIdType tx)) => Eq (RequirementFailure tx)
deriving instance (Show (TxIdType tx)) => Show (RequirementFailure tx)
deriving instance (ToJSON (TxIdType tx)) => ToJSON (RequirementFailure tx)
deriving instance (FromJSON (TxIdType tx)) => FromJSON (RequirementFailure tx)

instance Arbitrary (TxIdType tx) => Arbitrary (RequirementFailure tx) where
  arbitrary = genericArbitrary

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = HeadInitialized
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      }
  | CommittedUTxO
      { party :: Party
      , committedUTxO :: UTxOType tx
      , chainState :: ChainStateType tx
      }
  | TransactionAppliedToLocalUTxO
      { tx :: tx
      , utxo :: UTxOType tx
      }
  | StateReplaced (HeadState tx)
  deriving stock (Generic)

instance (Arbitrary tx, Arbitrary (HeadState tx), Arbitrary (ChainStateType tx), Arbitrary (UTxOType tx)) => Arbitrary (StateChanged tx) where
  arbitrary = genericArbitrary

deriving instance (Eq tx, Eq (HeadState tx), Eq (UTxOType tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving instance (Show tx, Show (HeadState tx), Show (UTxOType tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving instance (ToJSON tx, ToJSON (HeadState tx), ToJSON (UTxOType tx), ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving instance (FromJSON tx, FromJSON (HeadState tx), FromJSON (UTxOType tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

data Outcome tx
  = NoOutcome
  | Effects {effects :: [Effect tx]}
  | StateChanged (StateChanged tx)
  | Wait {reason :: WaitReason tx}
  | Error {error :: LogicError tx}
  | Combined {left :: Outcome tx, right :: Outcome tx}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (Outcome tx)
deriving instance (IsChainState tx) => Show (Outcome tx)
deriving instance (IsChainState tx) => ToJSON (Outcome tx)
deriving instance (IsChainState tx) => FromJSON (Outcome tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (Outcome tx) where
  arbitrary = genericArbitrary

data WaitReason tx
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingFor :: SnapshotNumber}
  | WaitOnSeenSnapshot
  | WaitOnTxs {waitingForTxIds :: [TxIdType tx]}
  | WaitOnContestationDeadline
  deriving stock (Generic)

deriving instance (IsTx tx) => Eq (WaitReason tx)
deriving instance (IsTx tx) => Show (WaitReason tx)
deriving instance (IsTx tx) => ToJSON (WaitReason tx)
deriving instance (IsTx tx) => FromJSON (WaitReason tx)

instance IsTx tx => Arbitrary (WaitReason tx) where
  arbitrary = genericArbitrary

data Environment = Environment
  { party :: Party
  -- ^ This is the p_i from the paper
  , -- NOTE(MB): In the long run we would not want to keep the signing key in
    -- memory, i.e. have an 'Effect' for signing or so.
    signingKey :: SigningKey HydraKey
  , otherParties :: [Party]
  , contestationPeriod :: ContestationPeriod
  }

collectEffects :: Outcome tx -> [Effect tx]
collectEffects = \case
  NoOutcome -> []
  Error _ -> []
  Wait _ -> []
  StateChanged _ -> []
  Effects effs -> effs
  Combined l r -> collectEffects l <> collectEffects r

collectWaits :: Outcome tx -> [WaitReason tx]
collectWaits = \case
  NoOutcome -> []
  Error _ -> []
  Wait w -> [w]
  StateChanged _ -> []
  Effects _ -> []
  Combined l r -> collectWaits l <> collectWaits r

-- FIXME: This is only used in tests
collectState :: HasCallStack => Outcome tx -> [HeadState tx]
collectState = \case
  NoOutcome -> []
  Error _ -> []
  Wait _ -> []
  StateChanged s ->
    case s of
      HeadInitialized{} -> undefined
      CommittedUTxO{} -> undefined
      TransactionAppliedToLocalUTxO{} -> undefined
      StateReplaced sc -> [sc]
  Effects _ -> []
  Combined l r -> collectState l <> collectState r

-- * The Coordinated Head protocol

-- ** Opening the Head

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  Outcome tx
onIdleClientInit env =
  Effects [OnChainEffect{postChainTx = InitTx parameters}]
 where
  parameters =
    HeadParameters
      { contestationPeriod
      , parties = party : otherParties
      }

  Environment{party, otherParties, contestationPeriod} = env

-- | Observe an init transaction, initialize parameters in an 'InitialState' and
-- notify clients that they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
onIdleChainInitTx ::
  -- | New chain state.
  ChainStateType tx ->
  [Party] ->
  ContestationPeriod ->
  HeadId ->
  Outcome tx
onIdleChainInitTx newChainState parties contestationPeriod headId =
  StateChanged
    ( HeadInitialized
        { parameters = HeadParameters{contestationPeriod, parties}
        , chainState = newChainState
        , headId
        }
    )
    `Combined` Effects [ClientEffect $ HeadIsInitializing headId (fromList parties)]

-- | Client request to commit a UTxO entry to the head. Provided the client
-- hasn't committed yet, this leads to a commit transaction on-chain containing
-- that UTxO entry.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientCommit ::
  Environment ->
  InitialState tx ->
  ClientInput tx ->
  Outcome tx
onInitialClientCommit env st clientInput =
  case clientInput of
    (Commit utxo)
      -- REVIEW: Is 'canCommit' something we want to handle here or have the OCV
      -- deal with it?
      | canCommit -> Effects [OnChainEffect{postChainTx = CommitTx party utxo}]
    _ -> Effects [ClientEffect $ CommandFailed clientInput]
 where
  canCommit = party `Set.member` pendingCommits

  InitialState{pendingCommits} = st

  Environment{party} = env

-- | Observe a commit transaction and record the committed UTxO in the state.
-- Also, if this is the last commit to be observed, post a collect-com
-- transaction on-chain.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialChainCommitTx ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | Comitting party
  Party ->
  -- | Committed UTxO
  UTxOType tx ->
  Outcome tx
onInitialChainCommitTx st newChainState pt utxo =
  StateChanged
    (CommittedUTxO{party = pt, committedUTxO = utxo, chainState = newChainState})
    `Combined` Effects
      ( notifyClient
          : [postCollectCom | canCollectCom]
      )
 where
  newCommitted = Map.insert pt utxo committed

  notifyClient = ClientEffect $ Committed headId pt utxo

  postCollectCom =
    OnChainEffect
      { postChainTx = CollectComTx $ fold newCommitted
      }

  canCollectCom = null remainingParties

  remainingParties = Set.delete pt pendingCommits

  InitialState{pendingCommits, committed, headId} = st

-- | Client request to abort the head. This leads to an abort transaction on
-- chain, reimbursing already committed UTxOs.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientAbort ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  Outcome tx
onInitialClientAbort st =
  Effects [OnChainEffect{postChainTx = AbortTx{utxo = fold committed}}]
 where
  InitialState{committed} = st

-- | Observe an abort transaction by switching the state and notifying clients
-- about it.
--
-- __Transition__: 'InitialState' → 'IdleState'
onInitialChainAbortTx ::
  Monoid (UTxOType tx) =>
  -- | New chain state
  ChainStateType tx ->
  Committed tx ->
  HeadId ->
  Outcome tx
onInitialChainAbortTx newChainState committed headId =
  StateChanged
    (StateReplaced $ Idle IdleState{chainState = newChainState})
    `Combined` Effects [ClientEffect $ HeadIsAborted{headId, utxo = fold committed}]

-- | Observe a collectCom transaction. We initialize the 'OpenState' using the
-- head parameters from 'IdleState' and construct an 'InitialSnapshot' holding
-- @u0@ from the committed UTxOs.
--
-- __Transition__: 'InitialState' → 'OpenState'
onInitialChainCollectTx ::
  (IsChainState tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onInitialChainCollectTx st newChainState =
  StateChanged
    ( StateReplaced $
        Open
          OpenState
            { parameters
            , coordinatedHeadState =
                CoordinatedHeadState
                  { localUTxO = u0
                  , allTxs = mempty
                  , localTxs = mempty
                  , confirmedSnapshot
                  , seenSnapshot = NoSeenSnapshot
                  }
            , chainState = newChainState
            , headId
            , currentSlot = chainStateSlot newChainState
            }
    )
    `Combined` Effects [ClientEffect $ HeadIsOpen{headId, utxo = u0}]
 where
  u0 = fold committed

  confirmedSnapshot = InitialSnapshot u0

  -- TODO: Do we want to check whether this even matches our local state? For
  -- example, we do expect `null remainingParties` but what happens if it's
  -- untrue?
  InitialState{parameters, committed, headId} = st

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx tx =
  Effects [NetworkEffect $ ReqTx tx]

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  OpenState tx ->
  TTL ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger st ttl tx = do
  -- Spec: Tall ← ̂Tall ∪ { (hash(tx), tx) }
  let chs' = coordinatedHeadState{allTxs = allTxs <> fromList [(txId tx, tx)]}
  -- Spec: wait L̂ ◦ tx ≠ ⊥ combined with L̂ ← L̂ ◦ tx
  waitApplyTx chs' $ \utxo' -> do
    let chs'' =
          chs'
            { -- Spec: L̂ ← L̂ ◦ tx
              localUTxO = utxo'
            , -- Spec: T̂ ← T̂ ∪ {tx}
              localTxs = localTxs'
            }
    (Effects [ClientEffect $ TxValid headId tx] `Combined`) $
      -- Spec: if ŝ = s̄ ∧ leader(s̄ + 1) = i
      if not snapshotInFlight && isLeader parameters party nextSn
        then
          StateChanged
            ( StateReplaced $
                Open
                  st
                    { coordinatedHeadState =
                        chs''
                          { seenSnapshot =
                              -- XXX: This state update has no equivalence in the
                              -- spec. Do we really need to store that we have
                              -- requested a snapshot? If yes, should update spec.
                              RequestedSnapshot
                                { lastSeen = seenSnapshotNumber seenSnapshot
                                , requested = nextSn
                                }
                          }
                    }
            )
            `Combined` Effects [NetworkEffect (ReqSn nextSn (txId <$> localTxs'))]
        else StateChanged (TransactionAppliedToLocalUTxO{tx = tx, utxo = utxo'})
 where
  waitApplyTx chs cont =
    case applyTransactions currentSlot localUTxO [tx] of
      Right utxo' -> cont utxo'
      Left (_, err)
        | ttl > 0 ->
            StateChanged (StateReplaced $ Open st{coordinatedHeadState = chs})
              `Combined` Wait (WaitOnNotApplicableTx err)
        | otherwise ->
            -- XXX: We might want to remove invalid txs from allTxs here to
            -- prevent them piling up infintely. However, this is not really
            -- covered by the spec and this could be problematic in case of
            -- conflicting transactions paired with network latency and/or
            -- message resubmission. For example: Assume tx2 depends on tx1, but
            -- only tx2 is seen by a participant and eventually times out
            -- because of network latency when receiving tx1. The leader,
            -- however, saw both as valid and requests a snapshot including
            -- both. This is a valid request and if we would have removed tx2
            -- from allTxs, we would make the head stuck.
            Effects [ClientEffect $ TxInvalid headId localUTxO tx err]

  Ledger{applyTransactions} = ledger

  Environment{party} = env

  CoordinatedHeadState{allTxs, localTxs, localUTxO, confirmedSnapshot, seenSnapshot} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, headId, currentSlot, parameters} = st

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  nextSn = confirmedSn + 1

  localTxs' = localTxs <> [tx]

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  OpenState tx ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  Outcome tx
onOpenNetworkReqSn env ledger st otherParty sn requestedTxIds =
  -- TODO: Verify the request is signed by (?) / comes from the leader
  -- (Can we prove a message comes from a given peer, without signature?)

  -- Spec: require s = ŝ + 1 and leader(s) = j
  requireReqSn $
    -- Spec: wait s̅ = ŝ
    waitNoSnapshotInFlight $
      -- Spec: wait ∀h ∈ Treq : (h, ·) ∈ Tall
      waitResolvableTxs $ do
        -- Spec: Treq ← {Tall [h] | h ∈ Treq#}
        let requestedTxs = mapMaybe (`Map.lookup` allTxs) requestedTxIds
        -- Spec: require U̅ ◦ Treq /= ⊥ combined with Û ← Ū̅ ◦ Treq
        requireApplyTxs requestedTxs $ \u -> do
          -- NOTE: confSn == seenSn == sn here
          let nextSnapshot = Snapshot (confSn + 1) u requestedTxIds
          -- Spec: σᵢ
          let snapshotSignature = sign signingKey nextSnapshot
          (Effects [NetworkEffect $ AckSn snapshotSignature sn] `Combined`) $ do
            -- Spec: for loop which updates T̂ and L̂
            let (localTxs', localUTxO') = pruneTransactions u
            -- Spec: Tall ← {tx | ∀tx ∈ Tall : tx ∉ Treq }
            let allTxs' = foldr Map.delete allTxs requestedTxIds
            StateChanged
              ( StateReplaced $
                  Open
                    st
                      { coordinatedHeadState =
                          coordinatedHeadState
                            { seenSnapshot = SeenSnapshot nextSnapshot mempty
                            , localTxs = localTxs'
                            , localUTxO = localUTxO'
                            , allTxs = allTxs'
                            }
                      }
              )
 where
  requireReqSn continue
    | sn /= seenSn + 1 =
        Error $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    | not (isLeader parameters otherParty sn) =
        Error $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    | otherwise =
        continue

  waitNoSnapshotInFlight continue
    | confSn == seenSn =
        continue
    | otherwise =
        Wait $ WaitOnSnapshotNumber seenSn

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue
      unseen -> Wait $ WaitOnTxs unseen

  -- NOTE: at this point we know those transactions apply on the localUTxO because they
  -- are part of the localTxs. The snapshot can contain less transactions than the ones
  -- we have seen at this stage, but they all _must_ apply correctly to the latest
  -- snapshot's UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  requireApplyTxs requestedTxs cont =
    case applyTransactions ledger currentSlot confirmedUTxO requestedTxs of
      Left (tx, err) ->
        Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
      Right u -> cont u

  pruneTransactions utxo = do
    foldr go ([], utxo) localTxs
   where
    go tx (txs, u) =
      -- XXX: We prune transactions on any error, while only some of them are
      -- actually expected.
      -- For example: `OutsideValidityIntervalUTxO` ledger errors are expected
      -- here when a tx becomes invalid.
      case applyTransactions ledger currentSlot u [tx] of
        Left (_, _) -> (txs, u)
        Right u' -> (txs <> [tx], u')

  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{initialUTxO} -> initialUTxO
    ConfirmedSnapshot{snapshot = Snapshot{utxo}} -> utxo

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, localTxs, allTxs} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, currentSlot} = st

  Environment{signingKey} = env

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- We do require that the is from the last seen or next expected snapshot, and
-- potentially wait wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsTx tx =>
  Environment ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn Environment{party} openState otherParty snapshotSignature sn =
  -- TODO: verify authenticity of message and whether otherParty is part of the head
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: (j,.) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        let sigs' = Map.insert otherParty snapshotSignature sigs
        ifAllMembersHaveSigned snapshot sigs' $ do
          -- Spec: σ̃ ← MS-ASig(k_H, ̂Σ̂)
          let multisig = aggregateInOrder sigs' parties
          requireVerifiedMultisignature multisig snapshot $ do
            let nextSn = sn + 1
            Effects [ClientEffect $ SnapshotConfirmed headId snapshot multisig]
              `Combined` if isLeader parameters party nextSn && not (null localTxs)
                then
                  StateChanged
                    ( StateReplaced $
                        onlyUpdateCoordinatedHeadState $
                          coordinatedHeadState
                            { confirmedSnapshot =
                                ConfirmedSnapshot
                                  { snapshot
                                  , signatures = multisig
                                  }
                            , seenSnapshot =
                                RequestedSnapshot
                                  { lastSeen = sn
                                  , requested = nextSn
                                  }
                            }
                    )
                    `Combined` Effects [NetworkEffect (ReqSn nextSn (txId <$> localTxs))]
                else
                  StateChanged
                    ( StateReplaced $
                        onlyUpdateCoordinatedHeadState $
                          coordinatedHeadState
                            { confirmedSnapshot =
                                ConfirmedSnapshot
                                  { snapshot
                                  , signatures = multisig
                                  }
                            , seenSnapshot = LastSeenSnapshot sn
                            }
                    )
 where
  seenSn = seenSnapshotNumber seenSnapshot

  requireValidAckSn continue =
    if sn `elem` [seenSn, seenSn + 1]
      then continue
      else Error $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      SeenSnapshot snapshot sigs
        | seenSn == sn -> continue snapshot sigs
      _ -> Wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  ifAllMembersHaveSigned snapshot sigs' cont =
    if Map.keysSet sigs' == Set.fromList parties
      then cont
      else
        StateChanged
          ( StateReplaced $
              onlyUpdateCoordinatedHeadState $
                coordinatedHeadState
                  { seenSnapshot = SeenSnapshot snapshot sigs'
                  }
          )

  requireVerifiedMultisignature multisig msg cont =
    case verifyMultiSignature vkeys multisig msg of
      Verified -> cont
      FailedKeys failures ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys = failures}
      KeyNumberMismatch ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys}

  vkeys = vkey <$> parties

  -- XXX: Data structures become unwieldy -> helper functions or lenses
  onlyUpdateCoordinatedHeadState chs' =
    Open openState{coordinatedHeadState = chs'}

  OpenState
    { parameters
    , coordinatedHeadState
    , headId
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs} = coordinatedHeadState

  HeadParameters{parties} = parameters

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  Effects [OnChainEffect{postChainTx = CloseTx confirmedSnapshot}]
 where
  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{coordinatedHeadState} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  StateChanged (StateReplaced closedState)
    `Combined` Effects
      ( notifyClient
          : [ OnChainEffect
              { postChainTx = ContestTx{confirmedSnapshot}
              }
            | doContest
            ]
      )
 where
  doContest =
    number (getSnapshot confirmedSnapshot) > closedSnapshotNumber

  closedState =
    Closed
      ClosedState
        { parameters
        , confirmedSnapshot
        , contestationDeadline
        , readyToFanoutSent = False
        , chainState = newChainState
        , headId
        }

  notifyClient =
    ClientEffect $
      HeadIsClosed
        { headId
        , snapshotNumber = closedSnapshotNumber
        , contestationDeadline
        }

  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{parameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  ClosedState tx ->
  SnapshotNumber ->
  Outcome tx
onClosedChainContestTx closedState snapshotNumber
  | snapshotNumber < number (getSnapshot confirmedSnapshot) =
      Effects
        [ ClientEffect HeadIsContested{snapshotNumber, headId}
        , OnChainEffect{postChainTx = ContestTx{confirmedSnapshot}}
        ]
  | snapshotNumber > number (getSnapshot confirmedSnapshot) =
      -- TODO: A more recent snapshot number was succesfully contested, we will
      -- not be able to fanout! We might want to communicate that to the client!
      Effects [ClientEffect HeadIsContested{snapshotNumber, headId}]
  | otherwise =
      Effects [ClientEffect HeadIsContested{snapshotNumber, headId}]
 where
  ClosedState{confirmedSnapshot, headId} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  Effects
    [ OnChainEffect
        { postChainTx =
            FanoutTx{utxo, contestationDeadline}
        }
    ]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, contestationDeadline} = closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState =
  StateChanged
    (StateReplaced $ Idle IdleState{chainState = newChainState})
    `Combined` Effects [ClientEffect $ HeadIsFinalized{headId, utxo}]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, headId} = closedState

-- | The "pure core" of the Hydra node, which handles the 'Event' against a
-- current 'HeadState'. Resulting new 'HeadState's are retained and 'Effect'
-- outcomes handled by the "Hydra.Node".
update ::
  (IsChainState tx) =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update env ledger st ev = case (st, ev) of
  (Idle _, ClientEvent Init) ->
    onIdleClientInit env
  (Idle _, OnChainEvent Observation{observedTx = OnInitTx{headId, contestationPeriod, parties}, newChainState}) ->
    onIdleChainInitTx newChainState parties contestationPeriod headId
  -- Initial
  (Initial idleState, ClientEvent clientInput@(Commit _)) ->
    onInitialClientCommit env idleState clientInput
  (Initial initialState, OnChainEvent Observation{observedTx = OnCommitTx{party = pt, committed = utxo}, newChainState}) ->
    onInitialChainCommitTx initialState newChainState pt utxo
  (Initial initialState, ClientEvent Abort) ->
    onInitialClientAbort initialState
  (Initial initialState, OnChainEvent Observation{observedTx = OnCollectComTx{}, newChainState}) ->
    onInitialChainCollectTx initialState newChainState
  (Initial InitialState{headId, committed}, OnChainEvent Observation{observedTx = OnAbortTx{}, newChainState}) ->
    onInitialChainAbortTx newChainState committed headId
  (Initial InitialState{committed, headId}, ClientEvent GetUTxO) ->
    Effects [ClientEffect . GetUTxOResponse headId $ fold committed]
  -- Open
  (Open openState, ClientEvent Close) ->
    onOpenClientClose openState
  (Open{}, ClientEvent (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState, NetworkEvent ttl _ (ReqTx tx)) ->
    onOpenNetworkReqTx env ledger openState ttl tx
  (Open openState, NetworkEvent _ otherParty (ReqSn sn txIds)) ->
    -- XXX: ttl == 0 not handled for ReqSn
    onOpenNetworkReqSn env ledger openState otherParty sn txIds
  (Open openState, NetworkEvent _ otherParty (AckSn snapshotSignature sn)) ->
    -- XXX: ttl == 0 not handled for AckSn
    onOpenNetworkAckSn env openState otherParty snapshotSignature sn
  ( Open openState@OpenState{headId = ourHeadId}
    , OnChainEvent Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}, headId}, ClientEvent GetUTxO) ->
    -- TODO: Is it really intuitive that we respond from the confirmed ledger if
    -- transactions are validated against the seen ledger?
    Effects [ClientEffect . GetUTxOResponse headId $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  -- Closed
  (Closed closedState, OnChainEvent Observation{observedTx = OnContestTx{snapshotNumber}}) ->
    onClosedChainContestTx closedState snapshotNumber
  (Closed cst@ClosedState{contestationDeadline, readyToFanoutSent, headId}, OnChainEvent Tick{chainTime})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        StateChanged
          (StateReplaced $ Closed cst{readyToFanoutSent = True})
          `Combined` Effects [ClientEffect $ ReadyToFanout headId]
  (Closed closedState, ClientEvent Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, OnChainEvent Observation{observedTx = OnFanoutTx{}, newChainState}) ->
    onClosedChainFanoutTx closedState newChainState
  -- General
  (currentState, OnChainEvent Rollback{rolledBackChainState}) ->
    StateChanged (StateReplaced $ setChainState rolledBackChainState currentState)
  (Open ost@OpenState{}, OnChainEvent Tick{chainSlot}) ->
    StateChanged (StateReplaced $ Open ost{currentSlot = chainSlot})
  (_, OnChainEvent Tick{}) ->
    NoOutcome
  (_, PostTxError{postChainTx, postTxError}) ->
    Effects [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]
  (_, ClientEvent{clientInput}) ->
    Effects [ClientEffect $ CommandFailed clientInput]
  _ ->
    Error $ InvalidEvent ev st

-- * Snapshot helper functions

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- * HeadState aggregate

-- | Reflect 'StateChanged' events onto the 'HeadState' aggregate.
aggregate :: IsTx tx => HeadState tx -> StateChanged tx -> HeadState tx
aggregate st = \case
  HeadInitialized{parameters = parameters@HeadParameters{parties}, headId, chainState} ->
    Initial
      InitialState
        { parameters = parameters
        , pendingCommits = Set.fromList parties
        , committed = mempty
        , chainState
        , headId
        }
  CommittedUTxO{committedUTxO, chainState, party} ->
    case st of
      Initial InitialState{parameters, pendingCommits, committed, headId} ->
        Initial
          InitialState
            { parameters
            , pendingCommits = remainingParties
            , committed = newCommitted
            , chainState
            , headId
            }
       where
        newCommitted = Map.insert party committedUTxO committed
        remainingParties = Set.delete party pendingCommits
      _otherState -> st
  TransactionAppliedToLocalUTxO{tx, utxo} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { allTxs = allTxs <> fromList [(txId tx, tx)]
                  , localUTxO = utxo
                  , localTxs = localTxs <> [tx]
                  }
            }
       where
        CoordinatedHeadState{allTxs, localTxs} = coordinatedHeadState
      _otherState -> st
  StateReplaced newState -> newState
