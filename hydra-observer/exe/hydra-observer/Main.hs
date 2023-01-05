{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hydra.Prelude

import Hydra.Chain.Direct.Observer (ObserverConfig (..))
import Hydra.Chain.Direct.Observer.Server (runServer)
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
import Prelude (read)

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
  hSetBuffering stdout NoBuffering
  config <- execParser toolsOptions
  port <- maybe 8000 read <$> lookupEnv "HYDRA_OBSERVER_PORT"
  runServer port config
