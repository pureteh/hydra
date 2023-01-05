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
import Hydra.Chain.Direct.Tx (findFirst, findHeadAssetId, mkHeadId)
import Hydra.ContestationPeriod (ContestationPeriod, fromChain)
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Party (Party, partyFromChain)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (fromData)

data HeadInitObservation = HeadInitObservation
  { headId :: HeadId
  , parties :: [Party]
  , contestationPeriod :: ContestationPeriod
  , cardanoKeyHashes :: [Hash PaymentKey]
  , headInitChainPoint :: Maybe ChainPoint
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
      , headInitChainPoint = Nothing
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
