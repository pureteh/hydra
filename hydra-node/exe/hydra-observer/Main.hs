{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hydra.Prelude

import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Hydra.Chain.Direct.Observer (ObserverConfig (..), runChainObserver)
import Hydra.Options (networkIdParser, nodeSocketParser, startChainFromParser)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  progDesc,
 )

optionsParser :: Parser ObserverConfig
optionsParser =
  ObserverConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> optional startChainFromParser

toolsOptions :: ParserInfo ObserverConfig
toolsOptions =
  info
    ( optionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Head Observer"
        <> header "hydra-observer - A Tool to observe Heads from a chain"
    )

main :: IO ()
main = do
  config <- execParser toolsOptions
  runChainObserver config dump
 where
  dump e = BS.putStr $ LBS.toStrict $ encode e <> "\n"
