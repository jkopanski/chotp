module Node where

import Prelude hiding (init)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVar, readTVar, writeTVar)
import           Control.Monad.Reader
import           Control.Distributed.Process.Lifted (Process, exit, expect, register, say, spawnLocal)
import qualified Control.Distributed.Backend.P2P as P2P
import           Data.Semigroup ((<>))

import RNG
import Msg
import Time

broadcast :: Message -> m ()
broadcast = undefined

generateMsg :: (MonadTime m, MonadRand m) => m Message
generateMsg = do
  val <- next
  time <- getSeconds
  pure $ Message time val

data Env = Env
  { _chain :: !(TVar Chain)
  , _time :: !Time
  , _rand :: !Rand
  }

instance HasSeconds Env where
  seconds = seconds . _time

instance HasGen Env where
  gen = gen . _rand

appendMsg :: Message -> Chain -> Chain
appendMsg m c = m:c

runNode :: ReaderT Env Process () -> Env -> Process ()
runNode = runReaderT

-- TODO: Take Process to capabilities
send :: ReaderT Env Process ()
send = do
  msg <- generateMsg
  liftIO $ threadDelay 1000000
  lift P2P.getPeers >>= (lift . say . show)

  lift $ P2P.nsendPeers "iohk" msg
  say $ "sent a message: " <> (show msg)
  pure ()

receive :: ReaderT Env Process ()
receive = do
  msg <- lift (expect :: Process Message)
  (say . show . value) msg
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain (appendMsg msg)

main :: Int -> Int -> Int -> Process ()
main seed sendTime gracePeriod = do
  rand <- Rand <$> init seed
  chain <- liftIO $ atomically $ newTVar emptyChain
  let env = Env chain defaultTime rand
  receiver <- spawnLocal (runNode (forever receive) env)
  register "iohk" receiver
  sender <- spawnLocal (runNode (forever send) env)
  -- register "iohk" sender
  liftIO $ threadDelay (sendTime * 1000000)
  exit sender "timeout"
  liftIO $ threadDelay (gracePeriod * 1000000)
  exit receiver "grace period"
  liftIO . print . show =<< (liftIO $ readTVarIO (_chain env))
  -- calculate result and exit
  pure ()
