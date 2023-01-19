module Hydra.Chain.Direct.Explorer where

import Hydra.Prelude

import Cardano.Ledger.Slot (SlotNo)
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
import qualified Data.List as List
import Data.Text (splitOn)
import Hydra.Cardano.Api (SlotNo (..))
import Hydra.Chain.Direct.Observer (ChainEvent, ObserverConfig (..), afterPoint, runChainObserver)
import Hydra.Network (PortNumber)
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
import Network.WebSockets (RequestHead (RequestHead, requestPath), withPingThread)
import qualified Network.WebSockets as WS

runServer :: PortNumber -> ObserverConfig -> IO ()
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
      & Warp.setPort (fromIntegral port)
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

websocketApp :: TVar IO [ChainEvent] -> TChan ChainEvent -> WS.PendingConnection -> IO ()
websocketApp history chan pendingConnection = do
  let RequestHead{requestPath} = WS.pendingRequest pendingConnection
      startingPoint = getStartingPoint requestPath
  cnx <- WS.acceptRequest pendingConnection
  withPingThread cnx 30 (pure ()) $
    void $ withAsync (sendEvents startingPoint cnx) $ wait
 where
  sendEvents startingPoint cnx = do
    (events, past) <- atomically $ do
      ch <- dupTChan chan
      h <- reverse . filter (afterPoint startingPoint) <$> readTVar history
      pure (ch, h)
    WS.sendTextDatas cnx (encode <$> past)
    forever $ atomically (readTChan events) >>= WS.sendTextData cnx . encode

getStartingPoint :: ByteString -> Maybe SlotNo
getStartingPoint rawPath =
  case (decodeUtf8' rawPath) of
    Left _ -> Nothing
    Right path ->
      case splitOn "/" path of
        ["", number] -> SlotNo <$> readMaybe (toString number)
        _ -> Nothing

httpApp :: Application
httpApp req send =
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> do
      send $
        responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    -- FIXME: do proper file serving, this is dangerous
    ("GET", path) -> send $ handleFile $ toString $ mconcat $ List.intersperse "/" ("." : path)
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
