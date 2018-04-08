module Node where

import Prelude hiding (init)
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.STM             ( TVar, atomically
                                                    , modifyTVar', newTVar
                                                    , readTVarIO, writeTVar
                                                    )
import           Control.Monad.Reader
import           Control.Distributed.Process.Lifted ( Process, exit, expect
                                                    , matchIf
                                                    , receiveTimeout, register
                                                    , say, spawnLocal
                                                    , unregister
                                                    )
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
  , _send :: Bool
  }

instance HasSeconds Env where
  seconds = seconds . _time

instance HasGen Env where
  gen = gen . _rand

appendMsg :: MonadIO m => Message -> ReaderT Env m ()
appendMsg msg = do
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain ((:) msg)

longer :: Chain -> Chain -> Chain
longer a b = if length a < length b then b
                                    else a

replaceChain :: MonadIO m => Chain -> ReaderT Env m ()
replaceChain chain = do
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain (longer chain)

runNode :: ReaderT Env Process () -> Env -> Process ()
runNode = runReaderT

-- TODO: Take Process to capabilities
newMsg :: ReaderT Env Process ()
newMsg = do
  msg <- generateMsg
  liftIO $ threadDelay 1000000
  lift $ P2P.nsendPeers "iohk" (AppendMsg msg)

processMBox :: ReaderT Env Process (Maybe ())
processMBox = do
  env <- ask
  receiveTimeout 0
    [ matchIf isAppendMsg (\(AppendMsg msg) -> runNode (appendMsg msg) env)
    , matchIf isRespChain (\(RespChain chain) -> runNode (replaceChain chain) env)
    ]

-- | idea is as follows:
-- 1. check mailbox for new messages,
--    process then until it is empty
-- 2. send new message
node :: Bool -> ReaderT Env Process ()
node isSending = do
  let while :: Monad m => (a -> Bool) -> m a -> m ()
      while pred m = do
        m >>= \v -> case pred v of
          True -> while pred m
          False -> pure ()

      isJust :: Maybe a -> Bool
      isJust ma | (Just _) <- ma = True
                | otherwise = False

  -- process all pending messages
  while isJust processMBox
  -- generate and send new message
  when isSending newMsg

main :: Int -> Int -> Int -> Process ()
main seed sendTime gracePeriod = do
  rand <- Rand <$> init seed
  chain <- liftIO $ atomically $ newTVar emptyChain
  let env = Env chain defaultTime rand True

  -- send and receive
  txrx <- spawnLocal (runNode (forever $ node True) env)
  register "iohk" txrx
  liftIO $ threadDelay (sendTime * 1000000)
  exit txrx "timeout"
  unregister "iohk"

  -- just receive
  rx <- spawnLocal (runNode (forever $ node False) env)
  register "iohk" rx
  liftIO $ threadDelay (gracePeriod * 1000000)
  exit rx "grace period"
  unregister "iohk"

  -- show all
  liftIO . print . show =<< (liftIO $ readTVarIO (_chain env))
