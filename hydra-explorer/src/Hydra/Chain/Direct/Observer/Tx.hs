{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic observation predicates to identify various Head transactions.
module Hydra.Chain.Direct.Observer.Tx where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.SafeHash (originalBytes)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import Hydra.Cardano.Api.Prelude (Hash (PaymentKeyHash))
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.Tx (
  UTxOHash (UTxOHash),
  convertTxOut,
  findFirst,
  findHeadAssetId,
  findStateToken,
  mkHeadId,
 )
import Hydra.ContestationPeriod (ContestationPeriod, fromChain)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Party (Party, partyFromChain)
import Hydra.Snapshot (SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (fromBuiltin, fromData)
import qualified Plutus.V2.Ledger.Api as Plutus

data HeadInitObservation = HeadInitObservation
  { headId :: HeadId
  , parties :: [Party]
  , contestationPeriod :: ContestationPeriod
  , cardanoKeyHashes :: [Hash PaymentKey]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- orphans?
instance ToJSON (Hash PaymentKey) where
  toJSON (PaymentKeyHash (Ledger.KeyHash vkh)) =
    Aeson.String . decodeUtf8 . Base16.encode $ originalBytes vkh

instance FromJSON (Hash PaymentKey) where
  parseJSON = Aeson.withText "Hash PaymentKey" $ \text ->
    case Base16.decode $ encodeUtf8 text of
      Left e -> fail e
      Right bs -> pure $ unsafePaymentKeyHashFromBytes bs

-- | Observes a newly initialised Head.
-- This observation is based on the structure of the transaction's outputs:
--
-- * It has an output paying to the `Head` script with a `Head.Initial` datum
-- * It has $n$ outputs that pay to the `Initial` script containing participation tokens
-- * Those PTs and the corresponding ST are minted by the transaction
observeHeadInitTx ::
  NetworkId ->
  Tx ->
  Maybe HeadInitObservation
observeHeadInitTx networkId tx = do
  -- FIXME: This is affected by "same structure datum attacks", we should be
  -- using the Head script address instead.
  (_, headOut, _, Head.Initial cp ps) <- findFirst headOutput indexedOutputs
  parties <- mapM partyFromChain ps
  let contestationPeriod = fromChain cp
  (headTokenPolicyId, headAssetName) <- findHeadAssetId headOut
  cardanoKeyHashes <- traverse fromAssetName $ assetNames headAssetName
  -- additional sanity check to ensure transaction is consistent
  guard (length initials == length cardanoKeyHashes)
  -- ensures this is a minting tx
  _ <- findScriptMinting tx headTokenPolicyId
  pure
    HeadInitObservation
      { contestationPeriod
      , headId = mkHeadId headTokenPolicyId
      , parties
      , cardanoKeyHashes
      }
 where
  headOutput = \case
    (ix, out@(TxOut _ _ (TxOutDatumInTx d) _)) ->
      (ix,out,toLedgerData d,) <$> fromData (toPlutusData d)
    _ -> Nothing

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    mapMaybe
      ( \(i, o) -> do
          dat <- getScriptData o
          pure (mkTxIn tx i, toCtxUTxOTxOut o, dat)
      )
      initialOutputs

  isInitial (TxOut addr _ _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @PlutusScriptV2 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

  assetNames headAssetName =
    [ assetName
    | (AssetId _ assetName, _) <- txMintAssets tx
    , assetName /= headAssetName
    ]

  fromAssetName :: AssetName -> Maybe (Hash PaymentKey)
  fromAssetName (AssetName bs) = deserialiseFromRawBytes (AsHash AsPaymentKey) bs

-- * Commit

data HeadCommitObservation = HeadCommitObservation
  { committed :: [TxOut CtxUTxO]
  , party :: Party
  , headId :: HeadId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a commit tx by:
--
-- - Find which 'initial' tx input is being consumed,
-- - Find the redeemer corresponding to that 'initial', which contains the tx
--   input of the committed utxo,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  Tx ->
  Maybe HeadCommitObservation
observeCommitTx networkId tx = do
  (_, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- getScriptData commitOut
  (onChainParty, _, onChainCommit) <- fromData @Commit.DatumType $ toPlutusData dat
  party <- partyFromChain onChainParty
  headId <- mkHeadId <$> findHeadIdFromPT commitOut
  let mCommittedTxOut = convertTxOut onChainCommit

  committed <-
    -- TODO: we do now record the TxOutRef also in the 'onChainCommit'. This
    -- reduces the cases as we can either interpret the commit or not.
    case mCommittedTxOut of
      Nothing -> Just mempty
      Just o -> Just [o]

  pure
    HeadCommitObservation
      { party
      , committed
      , headId
      }
 where
  findHeadIdFromPT :: TxOut ctx -> Maybe PolicyId
  findHeadIdFromPT txOut =
    flip findFirst (valueToList $ txOutValue txOut) $ \case
      (AssetId pid _, q)
        | q == 1 ->
          Just pid
      _ ->
        Nothing

  commitAddress = mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

-- * CollectCom

data HeadCollectComObservation = HeadCollectComObservation
  { headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a collectCom tx by lookup up the output paying to Head script
observeHeadCollectComTx ::
  Tx ->
  Maybe HeadCollectComObservation
observeHeadCollectComTx tx = do
  (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
  headId <- findStateToken newHeadOutput
  newHeadDatum <- lookupScriptData tx newHeadOutput
  utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
  pure
    HeadCollectComObservation
      { headId
      , utxoHash
      }
 where
  headScript = fromPlutusScript Head.validatorScript
  decodeUtxoHash datum =
    case fromData $ toPlutusData datum of
      Just Head.Open{utxoHash} -> Just $ fromBuiltin utxoHash
      _ -> Nothing

-- * Close

data HeadCloseObservation = HeadCloseObservation
  { headId :: HeadId
  , closeContestationDeadline :: Plutus.POSIXTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a close tx by lookup up the output paying to the Head script.
observeCloseTx ::
  Tx ->
  Maybe HeadCloseObservation
observeCloseTx tx = do
  (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
  headId <- findStateToken newHeadOutput
  newHeadDatum <- lookupScriptData tx newHeadOutput
  closeContestationDeadline <- case fromData (toPlutusData newHeadDatum) of
    Just Head.Closed{contestationDeadline} -> pure contestationDeadline
    _ ->
      Nothing
  pure
    HeadCloseObservation{headId, closeContestationDeadline}
 where
  headScript = fromPlutusScript Head.validatorScript

-- * Contest

data HeadContestObservation = HeadContestObservation
  { headId :: HeadId
  , contestContestationDeadline :: Plutus.POSIXTime
  , snapshotNumber :: SnapshotNumber
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a contest tx
-- 1. look up the output paying to the Head script.
-- 2. find a redeemer for an input that matches a `Contest` redeemer
observeContestTx ::
  Tx ->
  Maybe HeadContestObservation
observeContestTx tx = do
  redeemer <- findContestRedeemer
  (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
  headId <- findStateToken newHeadOutput
  newState <- fromData . toPlutusData =<< lookupScriptData tx newHeadOutput
  case (redeemer, newState) of
    (Head.Contest{snapshotNumber}, Head.Closed{contestationDeadline}) ->
      pure HeadContestObservation{headId, contestContestationDeadline = contestationDeadline, snapshotNumber = fromIntegral snapshotNumber}
    _ ->
      Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

  findContestRedeemer = listToMaybe $ mapMaybe (findRedeemerSpending @Head.Input tx) $ toList $ txInputSet tx
