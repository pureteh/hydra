{-# OPTIONS_GHC -Wwarn #-}

module Hydra.Chain.Direct.Observer.Server where

import Hydra.Prelude

import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Monad.Class.MonadAsync (wait)
import Data.Aeson (encode)
import Hydra.Chain.Direct.Observer (ChainEvent, ObserverConfig, runChainObserver)
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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import Network.WebSockets (withPingThread)
import qualified Network.WebSockets as WS

runServer :: Int -> ObserverConfig -> IO ()
runServer port config = do
  events <- newBroadcastTChanIO
  withAsync
    ( runChainObserver
        config
        ( \e -> do
            --            dump e
            atomically (writeTChan events e)
        )
    )
    $ \_ ->
      Wai.websocketsOr WS.defaultConnectionOptions (websocketApp events) httpApp
        & Warp.runSettings settings
 where
  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

websocketApp :: TChan ChainEvent -> WS.PendingConnection -> IO ()
websocketApp chan pendingConnection = do
  cnx <- WS.acceptRequest pendingConnection
  putTextLn "accepted connection"
  withPingThread cnx 30 (pure ()) $
    void $ withAsync (sendEvents cnx) $ wait
 where
  sendEvents cnx = do
    putTextLn "started thread"
    events <- atomically $ dupTChan chan
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
