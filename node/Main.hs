{-# language TemplateHaskell #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Data.Semigroup      ((<>))
import           Options.Applicative (execParser)

import Opts (Config (..), options)
import qualified Node as Node
import Msg

host = "127.0.0.1"
ports = [9500 .. 9504]

main :: IO ()
main = do
  opts <- execParser options
  let port = show $ _port opts
      seed = _seed opts
      send = _send opts
      wait = _wait opts
      searchPorts = show <$> filter (\p -> p /= _port opts) ports

  P2P.bootstrap
    host
    port
    (const (host, port))
    initRemoteTable
    (P2P.makeNodeId <$> ("127.0.0.1:" <>) <$> searchPorts)
    (Node.main seed send wait)
