{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances on ledger types used by
-- Hydra.Ledger.Cardano to have JSON representations for various types.
--
-- XXX: The ledger team notified that we should be using lenses going forward.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api hiding (Era)
import Hydra.Prelude

import Cardano.Binary (
  decodeFull',
  decodeFullDecoder',
  decodeListLenOf,
  decodeWord,
  encodeListLen,
  encodeWord,
  serialize',
 )
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Ledger
import qualified Cardano.Ledger.Alonzo.TxAuxData as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import Cardano.Ledger.Babbage.Core (auxDataHashTxBodyL, certsTxBodyL, collateralInputsTxBodyL, collateralReturnTxBodyL, feeTxBodyL, mintValueTxBodyF, networkIdTxBodyL, outputsTxBodyL, referenceInputsTxBodyL, reqSignerHashesTxBodyL, scriptIntegrityHashTxBodyL, totalCollateralTxBodyL, vldtTxBodyL, withdrawalsTxBodyL)
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (mkSized)
import qualified Cardano.Ledger.Binary as Ledger
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Core (eraProtVerLow, inputsTxBodyL)
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Codec.Binary.Bech32 as Bech32
import Control.Lens ((^.))
import Data.Aeson (
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSONKey,
  Value (String),
  object,
  toJSONKey,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (
  Pair,
  Parser,
  toJSONKeyText,
 )
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

--
-- Addr
--
-- NOTE: ToJSON instance defined in cardano-ledger-specs
-- NOTE: Not defining 'FromJSON' because of conflicts with cardano-ledger-specs

decodeAddress ::
  Crypto crypto =>
  Text ->
  Parser (Ledger.Addr crypto)
decodeAddress t =
  decodeBech32 <|> parseJSON (String t)
 where
  decodeBech32 =
    case Bech32.decodeLenient t of
      Left err ->
        fail $ "failed to decode from bech32: " <> show err
      Right (_prefix, dataPart) ->
        case Bech32.dataPartToBytes dataPart >>= Ledger.deserialiseAddr of
          Nothing -> fail "failed to deserialise addresse."
          Just addr -> pure addr

--
-- AuxiliaryData
--

instance
  ( Typeable era
  , ToCBOR (Ledger.AlonzoTxAuxData era)
  ) =>
  ToJSON (Ledger.AlonzoTxAuxData era)
  where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance
  ( Era era
  , FromCBOR (Ledger.AlonzoTxAuxData era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  ) =>
  FromJSON (Ledger.AlonzoTxAuxData era)
  where
  parseJSON = withText "AuxiliaryData" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

instance Crypto crypto => FromJSON (Ledger.AuxiliaryDataHash crypto) where
  parseJSON = fmap Ledger.AuxiliaryDataHash . parseJSON

--
-- Bootstrap Witness
--

instance Crypto crypto => ToJSON (Ledger.BootstrapWitness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromCBOR (Ledger.BootstrapWitness crypto) => FromJSON (Ledger.BootstrapWitness crypto) where
  parseJSON = withText "BootstrapWitness" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- IsValid
--

instance ToJSON Ledger.Babbage.IsValid where
  toJSON (Ledger.Babbage.IsValid b) = toJSON b

instance FromJSON Ledger.Babbage.IsValid where
  parseJSON = fmap Ledger.Babbage.IsValid . parseJSON

--
-- Redeemers
--
-- TODO: Provide maybe better instances for redeemers from which we can actually
-- view them as a map from pointers to data?

instance
  FromCBOR (Ledger.Redeemers era) =>
  FromJSON (Ledger.Redeemers era)
  where
  parseJSON = withText "Redeemers" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

instance ToCBOR (Ledger.Redeemers era) => ToJSON (Ledger.Redeemers era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

--
-- RewardAcnt
--

-- NOTE: The Ledge derive generic ToJSONKey from 'RewardAcnt', which by default
-- turn them into an array of elements.
rewardAcntToText :: Ledger.RewardAcnt crypto -> Text
rewardAcntToText = decodeUtf8 . Base16.encode . Ledger.serialiseRewardAcnt

rewardAcntFromText :: Crypto crypto => Text -> Maybe (Ledger.RewardAcnt crypto)
rewardAcntFromText t = do
  case Base16.decode (encodeUtf8 t) of
    Left{} -> Nothing
    Right bs -> Ledger.deserialiseRewardAcnt bs

--
-- SafeHash
--

instance Crypto crypto => ToJSONKey (Ledger.SafeHash crypto any) where
  toJSONKey = toJSONKeyText safeHashToText

safeHashToText ::
  Ledger.SafeHash crypto any ->
  Text
safeHashToText =
  decodeUtf8 . Base16.encode . Crypto.hashToBytes . Ledger.extractHash

instance Crypto crypto => FromJSONKey (Ledger.SafeHash crypto any) where
  fromJSONKey = FromJSONKeyTextParser safeHashFromText

safeHashFromText ::
  (Crypto crypto, MonadFail m) =>
  Text ->
  m (Ledger.SafeHash crypto any)
safeHashFromText t =
  case Crypto.hashFromTextAsHex t of
    Nothing -> fail "failed to decode from base16."
    Just h -> pure $ Ledger.unsafeMakeSafeHash h

--
-- Script
--

instance
  ( Crypto (Ledger.EraCrypto era)
  , Typeable era
  , Ledger.Era era
  , FromCBOR (Ledger.AlonzoScript era)
  ) =>
  FromJSON (Ledger.AlonzoScript era)
  where
  parseJSON = withText "Script" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- Timelock
--

instance ToJSON (Ledger.Timelock StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromCBOR (Ledger.Timelock StandardCrypto) => FromJSON (Ledger.Timelock StandardCrypto) where
  parseJSON = withText "Timelock" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxBody
--

instance ToJSON (Ledger.Babbage.BabbageTxBody LedgerEra) where
  toJSON b =
    object $
      mconcat
        [ onlyIf (const True) "inputs" (Set.map fromLedgerTxIn $ b ^. inputsTxBodyL)
        , onlyIf (not . null) "collateral" (Set.map fromLedgerTxIn $ b ^. collateralInputsTxBodyL)
        , onlyIf (not . null) "referenceInputs" (Set.map fromLedgerTxIn $ b ^. referenceInputsTxBodyL)
        , onlyIf (const True) "outputs" (fromLedgerTxOut <$> b ^. outputsTxBodyL)
        , onlyIf isSJust "collateralReturn" (fromLedgerTxOut <$> b ^. collateralReturnTxBodyL)
        , onlyIf isSJust "totalCollateral" (b ^. totalCollateralTxBodyL)
        , onlyIf (not . null) "certificates" (encodeDCert <$> b ^. certsTxBodyL)
        , onlyIf (not . null . Ledger.unWithdrawals) "withdrawals" (b ^. withdrawalsTxBodyL)
        , onlyIf (const True) "fees" (b ^. feeTxBodyL)
        , onlyIf (not . isOpenInterval) "validity" (b ^. vldtTxBodyL)
        , onlyIf (not . null) "requiredSignatures" (b ^. reqSignerHashesTxBodyL)
        , onlyIf (/= mempty) "mint" (fromLedgerValue (b ^. mintValueTxBodyF))
        , onlyIf isSJust "scriptIntegrityHash" (b ^. scriptIntegrityHashTxBodyL)
        , onlyIf isSJust "auxiliaryDataHash" (b ^. auxDataHashTxBodyL)
        , onlyIf isSJust "networkId" (b ^. networkIdTxBodyL)
        ]
   where
    version = eraProtVerLow @LedgerEra

    -- TODO: Delegation certificates can actually be represented as plain JSON
    -- objects (it's a sum type), so we may want to revisit this interface later?
    encodeDCert = String . decodeUtf8 . Base16.encode . Ledger.serialize' version

instance FromJSON (Ledger.Babbage.BabbageTxBody LedgerEra) where
  parseJSON = withObject "TxBody" $ \o -> do
    Ledger.Babbage.BabbageTxBody
      <$> (o .: "inputs")
      <*> (o .:? "collateral" .!= mempty)
      <*> (o .:? "referenceInputs" .!= mempty)
      <*> (fmap (mkSized version . toLedgerTxOut) <$> o .: "outputs")
      <*> (fmap (mkSized version . toLedgerTxOut) <$> o .:? "collateralReturn" .!= SNothing)
      <*> (o .:? "totalCollateral" .!= SNothing)
      <*> (o .:? "certificates" >>= mapM decodeDCert) .!= mempty
      <*> (o .:? "withdrawals" .!= Ledger.Withdrawals mempty)
      <*> (o .:? "fees" .!= mempty)
      <*> (o .:? "validity" .!= Ledger.ValidityInterval SNothing SNothing)
      <*> pure SNothing -- TODO: Protocol Updates? Likely irrelevant to the L2.
      <*> (o .:? "requiredSignatures" .!= mempty)
      <*> (o .:? "mint" <&> fmap valueToMultiAsset) .!= mempty
      <*> (o .:? "scriptIntegrityHash" .!= SNothing)
      <*> (o .:? "auxiliaryDataHash" .!= SNothing)
      <*> (o .:? "networkId" .!= SNothing)
   where
    version = eraProtVerLow @LedgerEra

    valueToMultiAsset v =
      let (Ledger.MaryValue _ ma) = toLedgerValue v
       in ma

    decodeDCert = withText "DCert" $ \t ->
      case Base16.decode $ encodeUtf8 t of
        Left err -> fail $ "failed to decode from base16: " <> show err
        Right bs' -> case Ledger.decodeFull' version bs' of
          Left err -> fail $ show err
          Right v -> pure v

--
-- TxDats
--

instance
  ( Typeable era
  , Crypto (Ledger.EraCrypto era)
  , Ledger.Era era
  ) =>
  ToJSON (Ledger.TxDats era)
  where
  toJSON (Ledger.TxDats datums) = toJSON datums

instance
  ( Typeable era
  , Crypto (Ledger.EraCrypto era)
  , Ledger.Era era
  , FromCBOR (Ledger.Data era)
  ) =>
  FromJSON (Ledger.TxDats era)
  where
  parseJSON = fmap Ledger.TxDats . parseJSON

instance
  ( Typeable era
  ) =>
  ToJSON (Ledger.Data era)
  where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance
  ( Typeable era
  , Ledger.Era era
  , FromCBOR (Ledger.Data era)
  ) =>
  FromJSON (Ledger.Data era)
  where
  parseJSON = withText "Data" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxId
--

txIdToText :: Ledger.TxId crypto -> Text
txIdToText (Ledger.TxId h) = safeHashToText h

txIdFromText :: (Crypto crypto, MonadFail m) => Text -> m (Ledger.TxId crypto)
txIdFromText = fmap Ledger.TxId . safeHashFromText

--
-- TxIn
--

instance FromJSON (Ledger.TxIn StandardCrypto) where
  parseJSON = fmap toLedgerTxIn . parseJSON

--
-- TxOut
--

instance FromJSON (Ledger.Babbage.BabbageTxOut LedgerEra) where
  parseJSON = fmap toLedgerTxOut . parseJSON

--
-- TxWitness
--

instance
  ( ToJSON (Ledger.Script era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  , Era era
  ) =>
  ToJSON (Ledger.AlonzoTxWits era)
  where
  toJSON (Ledger.AlonzoTxWits vkeys boots scripts datums redeemers) =
    object $
      mconcat
        [ onlyIf (not . null) "keys" vkeys
        , onlyIf (not . null) "bootstrap" boots
        , onlyIf (not . null) "scripts" scripts
        , onlyIf (not . Ledger.nullDats) "datums" datums
        , onlyIf (not . Ledger.nullRedeemers) "redeemers" redeemers
        ]

instance
  ( FromJSON (Ledger.Script era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  , Era era
  , FromCBOR (Ledger.WitVKey 'Ledger.Witness (Ledger.EraCrypto era))
  , FromCBOR (Ledger.BootstrapWitness (Ledger.EraCrypto era))
  , FromCBOR (Ledger.Babbage.Data era)
  , FromCBOR (Ledger.Redeemers era)
  ) =>
  FromJSON (Ledger.AlonzoTxWits era)
  where
  parseJSON = withObject "TxWitness" $ \o ->
    Ledger.AlonzoTxWits
      <$> (o .:? "keys" .!= mempty)
      <*> (o .:? "bootstrap" .!= mempty)
      <*> (o .:? "scripts" .!= mempty)
      <*> (o .:? "datums" .!= Ledger.TxDats mempty)
      <*> (o .:? "redeemers" .!= Ledger.Redeemers mempty)

--
-- ValidatedTx
--

instance
  ( ToJSON (Ledger.TxWits era)
  , ToJSON (Ledger.TxBody era)
  , ToJSON (Ledger.TxAuxData era)
  , ToJSON (Ledger.Script era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  , Ledger.Era era
  , Ledger.EraTxBody era
  ) =>
  ToJSON (Ledger.Babbage.AlonzoTx era)
  where
  toJSON (Ledger.Babbage.AlonzoTx body witnesses isValid auxiliaryData) =
    object $
      mconcat
        [ ["id" .= txid body]
        , ["body" .= body]
        , ["witnesses" .= witnesses]
        , ["isValid" .= isValid]
        , onlyIf isSJust "auxiliaryData" auxiliaryData
        ]

instance
  ( FromJSON (Ledger.TxBody era)
  , FromJSON (Ledger.TxAuxData era)
  , FromJSON (Ledger.TxWits era)
  , FromJSON (Ledger.Script era)
  , FromCBOR (Ledger.TxBody era)
  , FromCBOR (Ledger.TxWits era)
  , FromCBOR (Ledger.Babbage.AlonzoTx era)
  , Era era
  , Ledger.Script era ~ Ledger.AlonzoScript era
  ) =>
  FromJSON (Ledger.Babbage.AlonzoTx era)
  where
  parseJSON value =
    -- We accepts transactions in three forms:
    --
    -- (a) As high-level JSON object, which full format is specified via a
    -- JSON-schema.
    --
    -- (b) As a JSON 'text-envelope', which is a format defined and produced by
    -- the cardano-cli, wrapping base16-encoded strings as JSON objects with
    -- tags.
    --
    -- (c) As base16 string representing a CBOR-serialized transaction, since
    -- this is the most common medium of exchange used for transactions.
    parseAsBase16CBOR value
      <|> parseAsEnvelopedBase16CBOR value
      <|> parseAsAdHocJSONObject value
   where
    parseAsBase16CBOR =
      withText "Tx" $ \t ->
        case Base16.decode $ encodeUtf8 t of
          Left base16Error ->
            fail $ show base16Error
          Right bytes ->
            case decodeFull' bytes of
              Left cborError -> fail $ show cborError
              Right tx -> pure tx

    parseAsEnvelopedBase16CBOR =
      withObject "Tx" $ \o -> do
        let TextEnvelopeType envelopeType = textEnvelopeType (proxyToAsType (Proxy @Tx))
        str <- o .: "cborHex"
        guard . (== envelopeType) =<< (o .: "type")
        parseAsBase16CBOR (String str)

    parseAsAdHocJSONObject =
      withObject "Tx" $ \o -> do
        Ledger.Babbage.AlonzoTx
          <$> (o .: "body")
          <*> (o .: "witnesses")
          <*> (o .:? "isValid" .!= Ledger.Babbage.IsValid True)
          <*> (o .:? "auxiliaryData" .!= SNothing)

--
-- ValidityInterval
--

instance ToJSON Ledger.ValidityInterval where
  toJSON (Ledger.ValidityInterval notBefore notAfter) =
    object
      [ "notBefore" .= notBefore
      , "notAfter" .= notAfter
      ]

instance FromJSON Ledger.ValidityInterval where
  parseJSON = withObject "ValidityInterval" $ \obj ->
    Ledger.ValidityInterval
      <$> obj
      .: "notBefore"
      <*> obj
      .: "notAfter"

--
-- Value
--

instance FromJSON (Ledger.MaryValue StandardCrypto) where
  parseJSON = fmap toLedgerValue . parseJSON

--
-- Wdrl
--

instance Crypto crypto => ToJSON (Ledger.Withdrawals crypto) where
  toJSON = toJSON . Map.mapKeys rewardAcntToText . Ledger.unWithdrawals

instance Crypto crypto => FromJSON (Ledger.Withdrawals crypto) where
  parseJSON json = do
    m <- Map.foldMapWithKey fn <$> parseJSON json
    maybe (fail "failed to parse withdrawal map.") (pure . Ledger.Withdrawals) m
   where
    fn k v = Map.singleton <$> rewardAcntFromText k <*> v

--
-- WitVKey
--

instance Crypto crypto => ToJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' . prefixWithTag
   where
    prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit

instance
  ( Crypto crypto
  , FromCBOR (Ledger.WitVKey 'Ledger.Witness crypto)
  ) =>
  FromJSON (Ledger.WitVKey 'Ledger.Witness crypto)
  where
  parseJSON = withText "VKeyWitness" $ \t ->
    -- TODO(AB): this is ugly
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeFullDecoder' "ShelleyKeyWitness" decoder bs' of
        Left err -> fail $ show err
        Right v -> pure v
   where
    decoder = do
      decodeListLenOf 2
      t <- decodeWord
      case t of
        0 -> fromCBOR
        _ -> fail $ "Invalid tag decoding key witness, only support 1: " <> show t

--
-- Helpers
--

onlyIf :: ToJSON a => (a -> Bool) -> Aeson.Key -> a -> [Pair]
onlyIf predicate k v =
  [(k, toJSON v) | predicate v]

isOpenInterval :: Ledger.ValidityInterval -> Bool
isOpenInterval = \case
  Ledger.ValidityInterval SNothing SNothing -> True
  _ -> False
