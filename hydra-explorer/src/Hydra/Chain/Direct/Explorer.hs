{-# OPTIONS_GHC -Wwarn #-}

module Hydra.Chain.Direct.Explorer where

import Hydra.Prelude

import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Monad.Class.MonadAsync (wait)
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Hydra.Chain.Direct.Observer (ChainEvent, ObserverConfig (..), runChainObserver)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status200, status404, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Hydra.Chain.Direct.Observer.Tx (HeadCloseObservation,
                                       HeadCollectComObservation, HeadCommitObservation, HeadInitObservation (..), observeCloseTx, observeCommitTx, observeHeadCollectComTx, observeHeadInitTx)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import Network.WebSockets (withPingThread)
import qualified Network.WebSockets as WS

runServer :: Int -> ObserverConfig -> IO ()
runServer port config@ObserverConfig{dumpEvents} = do
  history <- newTVarIO mempty
  events <- newBroadcastTChanIO
  withAsync
    ( runChainObserver
        config
        ( \e -> do
            when dumpEvents $ dump e
            atomically $ do
              appendToHistory history e
              writeTChan events e
        )
    )
    $ \_ ->
      Wai.websocketsOr WS.defaultConnectionOptions (websocketApp history events) httpApp
        & Warp.runSettings settings
 where
  dump e = BS.putStr $ LBS.toStrict $ encode e <> "\n"

  appendToHistory h e = modifyTVar' h $ (e :)

  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

websocketApp :: TVar IO [ChainEvent] -> TChan ChainEvent -> WS.PendingConnection -> IO ()
websocketApp history chan pendingConnection = do
  cnx <- WS.acceptRequest pendingConnection
  putTextLn "accepted connection"
  withPingThread cnx 30 (pure ()) $
    void $ withAsync (sendEvents cnx) $ wait
 where
  sendEvents cnx = do
    putTextLn "started thread"
    (events, past) <- atomically $ do
      ch <- dupTChan chan
      h <- reverse <$> readTVar history
      pure (ch, h)
    WS.sendTextDatas cnx (encode <$> past)
    forever $ atomically (readTChan events) >>= WS.sendTextData cnx . encode

httpApp :: Application
httpApp req send =
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> do
      send $
        responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["index.html"]) -> send $ handleFile "index.html"
    ("GET", ["bundle.js"]) -> send $ handleFile "bundle.js"
    ("GET", ["style.css"]) -> send $ handleFile "style.css"
    ("GET", ["logo.png"]) -> send $ handleFile "logo.png"
    (_, _) ->
      send handleNotFound

handleError :: Response
handleError =
  responseLBS status500 corsHeaders "INVALID REQUEST"

handleNotFound :: Response
handleNotFound =
  responseLBS status404 corsHeaders "NOT FOUND"

handleFile :: FilePath -> Response
handleFile filepath = responseFile status200 corsHeaders filepath Nothing

corsHeaders :: [(HeaderName, ByteString)]
corsHeaders =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Methods", "*")
  , ("Access-Control-Allow-Headers", "*")
  ]
