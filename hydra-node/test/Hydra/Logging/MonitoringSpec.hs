{-# LANGUAGE TypeApplications #-}

module Hydra.Logging.MonitoringSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.Text as Text
import Hydra.API.ServerOutput (ServerOutput (SnapshotConfirmed))
import Hydra.BehaviorSpec (testHeadId)
import Hydra.HeadLogic (
  Effect (ClientEffect),
  Event (NetworkEvent),
  defaultTTL,
 )
import Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Hydra.Logging (nullTracer, traceWith)
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Logging.Monitoring
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node (HydraNodeLog (BeginEffect, BeginEvent))
import Hydra.Snapshot (Snapshot (Snapshot))
import Network.HTTP.Req (GET (..), NoReqBody (..), bsResponse, defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Test.Hydra.Fixture (alice)
import Test.Network.Ports (randomUnusedTCPPorts)

spec :: Spec
spec =
  it "provides prometheus metrics from traces" $ do
    failAfter 3 $ do
      [p] <- randomUnusedTCPPorts 1
      withMonitoring (Just $ fromIntegral p) nullTracer $ \tracer -> do
        traceWith tracer (Node $ BeginEvent alice 0 (NetworkEvent defaultTTL alice (ReqTx (aValidTx 42))))
        traceWith tracer (Node $ BeginEvent alice 1 (NetworkEvent defaultTTL alice (ReqTx (aValidTx 43))))
        threadDelay 0.1
        traceWith tracer (Node $ BeginEffect alice 0 0 (ClientEffect (SnapshotConfirmed testHeadId (Snapshot 1 (utxoRefs [1]) [43, 42]) mempty)))

        metrics <-
          Text.lines . decodeUtf8 . responseBody
            <$> runReq @IO defaultHttpConfig (req GET (http "localhost" /: "metrics") NoReqBody bsResponse (port p))

        metrics `shouldContain` ["hydra_head_confirmed_tx  2"]
        metrics `shouldContain` ["hydra_head_tx_confirmation_time_ms_bucket{le=\"1000.0\"} 2.0"]
