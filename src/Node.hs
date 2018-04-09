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

getChain :: ReaderT Env Process Chain
getChain = liftIO . readTVarIO =<< asks _chain

generateMsg :: ReaderT Env Process Message
generateMsg = do
  val <- next
  time <- getSeconds
  chain <- getChain
  pure $ Message time val (scaledSum chain)

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

appendMsg :: Message -> ReaderT Env Process ()
appendMsg msg = do
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain (msg :)

processMsg :: Query -> ReaderT Env Process ()
processMsg (AppendMsg msg)= do
  chain <- getChain
  if canAppend chain msg then appendMsg msg
                         else pure () -- TODO: request chain update
processMsg _ = pure ()

replaceChain :: Chain -> ReaderT Env Process ()
replaceChain chain = do
  liftIO $ guard (isValidChain chain)
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain (longerChain chain)

runNode :: ReaderT Env Process () -> Env -> Process ()
runNode = runReaderT

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
