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

import Opts (Config, options)

host = "127.0.0.1"
port = "10501"

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling: " <> msg

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 100000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  execParser options >>= putStrLn . show

  P2P.bootstrap "gallium" "10501" (\port -> ("gallium", port)) myRemoteTable [P2P.makeNodeId "localhost:10501"] (forever $ do
    liftIO $ threadDelay 100000
    P2P.nsendPeers "myService" ("some", "message"))

    -- us <- getSelfNode
    -- _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
    -- pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
    -- liftIO $ threadDelay 200000

    -- echo <- spawnLocal $ forever $ do
    --   receiveWait [match logMessage, match replyBack]

    -- say "send some messages!"
    -- send echo "hello"

    -- self <- getSelfPid
    -- send echo (self, ("hello" :: String))

    -- m <- expectTimeout 100000 :: Process (Maybe String)
    -- case m of
    --   Nothing -> die "nothing came back!"
    --   Just s -> say $ "got: " <> s <> " back!"
