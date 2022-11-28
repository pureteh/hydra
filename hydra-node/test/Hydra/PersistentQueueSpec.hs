module Hydra.PersistentQueueSpec where

import Control.Monad.Class.MonadSTM (MonadSTM (isEmptyTQueue, newTQueue, readTQueue, writeTQueue))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Hydra.Prelude
import Test.Hydra.Prelude
import UnliftIO.IO.File (withBinaryFileDurable)

spec :: Spec
spec = do
  describe "PersistentQueue" $ do
    it "can handle empty files" $ do
      withTempDir "hydra-persistence" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        writeFileBS fp ""
        withPersistentQueue fp $ \PersistentQueue{isEmpty} ->
          isEmpty `shouldReturn` True

    it "can pop a pushed item" $ do
      withTempDir "persistentQueue" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        withPersistentQueue fp $ \PersistentQueue{push, pop} -> do
          push 12043
          pop `shouldReturn` 12043

    it "is not empty on restart" $ do
      withTempDir "persistentQueue" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        withPersistentQueue fp $ \PersistentQueue{push} ->
          push 12043

        withPersistentQueue fp $ \PersistentQueue{isEmpty} ->
          isEmpty `shouldReturn` False

    xit "can pop a pushed item after restart" $ do
      withTempDir "persistentQueue" $ \tmpDir -> do
        let fp = tmpDir <> "/data"
        withPersistentQueue fp $ \PersistentQueue{push} ->
          push 12043

        withPersistentQueue fp $ \PersistentQueue{pop} ->
          pop `shouldReturn` 12043

--     it "is consistent after multiple append calls in presence of new-lines" $
--       checkCoverage $
--         monadicIO $ do
--           items <- pick $ listOf genPersistenceItem
--           monitor (cover 1 (null items) "no items stored")
--           monitor (cover 10 (containsNewLine items) "some item contains a new line")
--
--           actualResult <- run $
--             withTempDir "hydra-persistence" $ \tmpDir -> do
--               PersistenceIncremental{loadAll, append} <- createPersistenceIncremental $ tmpDir <> "/data"
--               forM_ items append
--               loadAll
--           pure $ actualResult === items

data PersistentQueue a m = PersistentQueue
  { isEmpty :: m Bool
  , push :: Natural -> m ()
  , pop :: m Natural
  }

withPersistentQueue :: (MonadIO m, MonadThrow m) => FilePath -> (PersistentQueue a m -> Handle -> m b) -> m b
withPersistentQueue filePath action = do
  memoryQueue <- atomically newTQueue
  withBinaryFileDurable filePath AppendMode $
    action
      PersistentQueue
        { isEmpty = atomically $ isEmptyTQueue memoryQueue
        , push = atomically . writeTQueue memoryQueue
        , pop = atomically $ readTQueue memoryQueue
        }

appendToWriteAheadLog :: ToJSON a => Handle -> a -> IO ()
appendToWriteAheadLog fileHandle x = do
  let bytes = toStrict $ Aeson.encode x <> "\n"
  BS.hPut fileHandle bytes
