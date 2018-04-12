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

main :: IO ()
main = do
  opts <- execParser options
  let port = show $ _port opts
      host = _host opts
      seed = _seed opts
      send = _send opts
      wait = _wait opts
      throttle = _throttle opts
      peers = _peers opts

  P2P.bootstrap
    host
    port
    (const (host, port))
    initRemoteTable
    (P2P.makeNodeId <$> peers)
    (Node.main seed send wait throttle)
