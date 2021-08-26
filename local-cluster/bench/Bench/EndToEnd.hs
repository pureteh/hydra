{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Lens ((^?))
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  modifyTVar,
  newTVarIO,
 )
import Data.Aeson (Result (Success), Value, encodeFile, fromJSON, (.=))
import Data.Aeson.Lens (key, _Array, _Number)
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Hydra.Ledger (Tx, TxId, Utxo, txId)
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  HydraClient,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraNode,
  withMockChain,
 )
import System.FilePath ((</>))

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

data Event = Event
  { submittedAt :: UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: FilePath -> Utxo CardanoTx -> [CardanoTx] -> IO ()
bench workDir initialUtxo txs = do
  registry <- newTVarIO mempty :: IO (TVar IO (Map.Map (TxId CardanoTx) Event))
  failAfter 300 $
    showLogsOnFailure $ \tracer ->
      withMockChain $ \chainPorts ->
        withHydraNode tracer workDir chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
          withHydraNode tracer workDir chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
            withHydraNode tracer workDir chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
              waitForNodesConnected tracer [n1, n2, n3]
              let contestationPeriod = 10 :: Natural
              send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
              waitFor tracer 3 [n1, n2, n3] $
                output "ReadyToCommit" ["parties" .= [int 10, 20, 30]]

              send n1 $ input "Commit" ["utxo" .= initialUtxo]
              send n2 $ input "Commit" ["utxo" .= noUtxos]
              send n3 $ input "Commit" ["utxo" .= noUtxos]

              waitFor tracer 3 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= initialUtxo]

              for_ txs (\tx -> newTx registry n1 tx >> threadDelay 0.001)
                `concurrently_` waitForAllConfirmations n1 registry txs

              send n1 $ input "Close" []
              waitMatch (contestationPeriod + 3) n1 $ \v ->
                guard (v ^? key "tag" == Just "HeadIsFinalized")

  res <- mapMaybe analyze . Map.toList <$> readTVarIO registry
  let resFile = workDir </> "results.json"
  putStrLn $ "Writing results to: " <> resFile
  encodeFile resFile res

--
-- Helpers
--

noUtxos :: Utxo CardanoTx
noUtxos = mempty

int :: Int -> Int
int = id

type TransactionId = Integer
type TransactionInput = Int
type TransactionOutput = Int

newTx ::
  Tx tx =>
  TVar IO (Map.Map (TxId tx) Event) ->
  HydraClient ->
  tx ->
  IO ()
newTx registry client tx = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.insert (txId tx) $
        Event
          { submittedAt = now
          , confirmedAt = Nothing
          }
  send client $ input "NewTx" ["transaction" .= tx]

waitForAllConfirmations :: HydraClient -> TVar IO (Map.Map (TxId CardanoTx) Event) -> [CardanoTx] -> IO ()
waitForAllConfirmations n1 registry txs =
  go allIds
 where
  allIds = Set.fromList $ map txId txs

  go remainingIds
    | Set.null remainingIds = pure ()
    | otherwise = do
      (confirmedTxs, confirmedSnapshotNumber) <- waitMatch 20 n1 $ \v -> do
        guard (v ^? key "tag" == Just "SnapshotConfirmed")
        snapshot <- v ^? key "snapshot"
        (,)
          <$> snapshot ^? key "confirmedTransactions" . _Array
          <*> snapshot ^? key "snapshotNumber" . _Number
      -- TODO(SN): use a tracer for this
      putTextLn $ "Snapshot confirmed: " <> show confirmedSnapshotNumber
      confirmedIds <- mapM (confirmTx registry) confirmedTxs
      go $ remainingIds \\ Set.fromList (toList confirmedIds)

confirmTx ::
  TVar IO (Map.Map (TxId CardanoTx) Event) ->
  Value ->
  IO (TxId CardanoTx)
confirmTx registry tx = do
  case fromJSON @(TxId CardanoTx) <$> tx ^? key "id" of
    Just (Success identifier) -> do
      now <- getCurrentTime
      atomically $
        modifyTVar registry $
          Map.adjust (\e -> e{confirmedAt = Just now}) identifier
      pure identifier
    _ -> error $ "incorrect Txid" <> show tx

analyze :: (TxId CardanoTx, Event) -> Maybe (UTCTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, confirmedAt = Just conf}) -> Just (submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing
