{-# LANGUAGE TypeApplications #-}

module Hydra.Network.AuthenticateSpec where

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Data.ByteString (pack)
import Hydra.Crypto (sign)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (message), nullTracer, traceInTVar)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..), Signed (Signed), mkAuthLog, withAuthentication)
import Hydra.Network.HeartbeatSpec (noop)
import Hydra.Network.Message (Message (ReqTx))
import Hydra.NetworkSpec (prop_canRoundtripCBOREncoding)
import Hydra.Prelude
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Hydra.Prelude
import Test.QuickCheck (listOf)
import Test.QuickCheck.Gen (generate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  msg <- runIO $ generate @(Message SimpleTx) arbitrary
  it "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withAuthentication
            nullTracer
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Signed msg (sign bobSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob]

  it "drop message coming from unknown party" $ do
    unexpectedMessage <- ReqTx <$> generate arbitrary
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withAuthentication
            nullTracer
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Signed msg (sign bobSk msg) bob)
                incoming (Signed unexpectedMessage (sign aliceSk unexpectedMessage) alice)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob]

  it "drop message coming from party with wrong signature" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withAuthentication
            nullTracer
            aliceSk
            [bob, carol]
            ( \incoming _ -> do
                incoming (Signed msg (sign carolSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` []

  it "authenticate the message to broadcast" $ do
    let someMessage = Authenticated msg bob
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO []

          withAuthentication nullTracer bobSk [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Signed msg (sign bobSk msg) bob]

  it "logs dropped messages" $ do
    let signature = sign carolSk msg
    let signedMsg = Signed msg signature bob
    let traced = runSimOrThrow $ do
          traces <- newTVarIO []

          let tracer = traceInTVar traces
          withAuthentication tracer aliceSk [bob, carol] (\incoming _ -> incoming signedMsg) noop $ \_ ->
            threadDelay 1

          readTVarIO traces

    (message <$> traced) `shouldContain` [mkAuthLog msg signature bob]

  describe "Serialization" $ do
    prop "can roundtrip CBOR encoding/decoding of Signed Hydra Message" $
      prop_canRoundtripCBOREncoding @(Signed Msg)

newtype Msg = Msg ByteString
  deriving newtype (Eq, Show, ToCBOR, FromCBOR, SignableRepresentation)

instance Arbitrary Msg where
  arbitrary = Msg . pack <$> listOf arbitrary
