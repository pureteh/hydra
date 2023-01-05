{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A simplified chain follower that tracks Head initialisation transactions over a
-- network.
module Hydra.Chain.Direct.Observer where

import Hydra.Prelude

import Control.Exception (IOException)
import Control.Tracer (nullTracer)
import Hydra.Cardano.Api (
  ChainPoint,
  ConsensusMode (CardanoMode),
  NetworkId,
  fromConsensusPointInMode,
  fromLedgerTx,
  getTxBody,
  toConsensusPointInMode,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Chain.CardanoClient (
  queryTip,
 )
import Hydra.Chain.Direct (
  ConnectException (..),
  IntersectionNotFoundException (..),
 )
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  DirectChainLog (..),
  getBabbageTxs,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.Observer.Tx (HeadInitObservation (..), observeHeadInitTx)
import Hydra.Chain.Direct.Util (
  Block,
  defaultCodecs,
  nullConnectTracers,
  versions,
 )
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Network.Block (Point (..), Tip, blockPoint, getTipPoint)
import Ouroboros.Network.Mux (
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient (
  LocalAddress,
  NodeToClientProtocols (..),
  NodeToClientVersion,
  connectTo,
  localSnocket,
  localStateQueryPeerNull,
  localTxMonitorPeerNull,
  localTxSubmissionPeerNull,
  nodeToClientProtocols,
  withIOManager,
 )
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
  chainSyncClientPeer,
 )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

data ObserverConfig = ObserverConfig
  { -- | Network identifer to which we expect to connect.
    networkId :: NetworkId
  , -- | Path to a domain socket used to connect to the server.
    nodeSocket :: FilePath
  , startChainFrom :: Maybe ChainPoint
  }

data ChainEvent
  = HeadInit {headInit :: HeadInitObservation}
  | Forward {point :: ChainPoint, tx :: Api.Tx}
  | Backward {point :: ChainPoint}

-- | A generic chain observer used to detect new heads.
runChainObserver ::
  ObserverConfig ->
  -- | A callback which is passed new heads
  (ChainEvent -> IO ()) ->
  IO ()
runChainObserver config callback = do
  chainPoint <- maybe (queryTip networkId nodeSocket) pure $ startChainFrom
  handle onIOException $ do
    let handler = mkChainSyncHandler callback networkId
    let intersection = toConsensusPointInMode CardanoMode chainPoint
    let client = ouroborosApplication intersection handler
    withIOManager $ \iocp ->
      connectTo
        (localSnocket iocp)
        nullConnectTracers
        (versions networkId client)
        nodeSocket
 where
  ObserverConfig{networkId, nodeSocket, startChainFrom} = config

  onIOException :: IOException -> IO ()
  onIOException ioException =
    throwIO $
      ConnectException
        { ioException
        , nodeSocket
        , networkId
        }

mkChainSyncHandler :: (ChainEvent -> IO ()) -> NetworkId -> ChainSyncHandler IO
mkChainSyncHandler callback networkId =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  -- TODO: do something with rollbacks?
  onRollBackward :: Point Block -> IO ()
  onRollBackward rollbackPoint = do
    let point = fromConsensusPointInMode CardanoMode rollbackPoint
    callback $ Backward{point}

  onRollForward :: Block -> IO ()
  onRollForward blk = do
    let point = fromConsensusPointInMode CardanoMode $ blockPoint blk
    let receivedTxs = map fromLedgerTx . toList $ getBabbageTxs blk

    forM_ receivedTxs $ \tx ->
      case observeHeadInitTx networkId tx of
        Just t -> callback $ HeadInit t{headInitChainPoint = Just point}
        Nothing -> callback $ Forward{point, tx}

ouroborosApplication ::
  (MonadST m, MonadTimer m, MonadThrow m) =>
  Point Block ->
  ChainSyncHandler m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
ouroborosApplication point handler nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClient handler point
                   in MuxPeer nullTracer cChainSyncCodec (chainSyncClientPeer peer)
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionPeerNull
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryPeerNull
                   in MuxPeer nullTracer cStateQueryCodec peer
            , localTxMonitorProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxMonitorPeerNull
                   in MuxPeer nullTracer cTxMonitorCodec peer
            }
    )
    nodeToClientV
 where
  Codecs
    { cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    , cTxMonitorCodec
    } = defaultCodecs nodeToClientV

chainSyncClient ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  ChainSyncHandler m ->
  Point Block ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient handler startingPoint =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect
        [startingPoint]
        ( clientStIntersect
            (\_ -> throwIO (IntersectionNotFound startingPoint))
        )
 where
  clientStIntersect ::
    (Point Block -> m (ClientStIdle Block (Point Block) (Tip Block) m ())) ->
    ClientStIntersect Block (Point Block) (Tip Block) m ()
  clientStIntersect onIntersectionNotFound =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure clientStIdle)
      , recvMsgIntersectNotFound = \(getTipPoint -> tip) ->
          ChainSyncClient $ onIntersectionNotFound tip
      }

  clientStIdle :: ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \block _tip -> ChainSyncClient $ do
          onRollForward handler block
          pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          onRollBackward handler point
          pure clientStIdle
      }
