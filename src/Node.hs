module Node where

import Prelude hiding (init, pred)
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.STM             ( TVar, atomically
                                                    , modifyTVar', newTVar
                                                    , readTVarIO, writeTVar
                                                    )
import           Control.Monad.Reader
import           Control.Distributed.Process.Lifted ( Process
                                                    , exit
                                                    , getSelfPid
                                                    , matchAny, handleMessage_
                                                    , receiveTimeout, register
                                                    , newChan
                                                    , receiveChanTimeout
                                                    , say, send, sendChan, spawnLocal
                                                    , unregister
                                                    )
import           Control.Distributed.Process.Lifted.Class
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
  , _send :: !Bool
  }

instance HasSeconds Env where
  seconds = seconds . _time

instance HasGen Env where
  gen = gen . _rand

modifyChain :: (Chain -> Chain) -> ReaderT Env Process ()
modifyChain f = do
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain f

processMsg :: Query -> ReaderT Env Process ()
processMsg (AppendMsg msg pid)= do
  tchain <- asks _chain
  chain <- liftIO $ readTVarIO tchain
  say "processing msg..."
  case append msg chain of
    Just new -> do
      say "appending"
      liftIO $ atomically $ writeTVar tchain new
    Nothing -> do
      -- could really used an alternative
      if total msg < scaledSum chain
         -- if total msg < scaledSum chain
         -- the other node has a problem, we don't ;)
         then pure ()
         -- no sense in going forward,
         -- update the chain first
          else do
            (sendChain, recChain) <- newChan
            send pid (QueryChain sendChain)
            say "waiting for chain..."
            -- we limit the time so that in case of failure we keep progressing
            mchain <- receiveChanTimeout 1000000 recChain
            say "got new chain"
            process mchain

  where process :: Maybe Chain -> ReaderT Env Process ()
        process (Just c) = if isValidChain c then modifyChain (longerChain c)
                                             else pure ()
        process Nothing = pure ()

processMsg (QueryChain sendChain) = do
  chain <- getChain
  sendChan sendChain chain

newMsg :: Bool -> ReaderT Env Process ()
newMsg throttle = do
  msg <- generateMsg
  liftP $ P2P.nsendPeers "iohk" . AppendMsg msg =<< getSelfPid
  if throttle then liftIO $ threadDelay 1000000
              else pure ()

processMBox :: ReaderT Env Process (Maybe ())
processMBox = do
  env <- ask
  receiveTimeout 0 [ matchAny (\msg -> handleMessage_ msg (\m -> process env m)) ]
    where process :: Env -> Query -> Process ()
          process e q = runReaderT (processMsg q) e

-- | idea is as follows:
-- 1. check mailbox for new messages,
--    process them until mbox is empty
-- 2. send new message
node :: Bool -> Bool-> ReaderT Env Process ()
node isSending throttle = do
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
  when isSending (newMsg throttle)

main :: Int -> Int -> Int -> Bool -> Process ()
main seed sendTime gracePeriod throttle = do
  rand <- Rand <$> init seed
  tchain <- liftIO $ atomically $ newTVar [initialMsg]
  let env = Env tchain defaultTime rand True

  -- send and receive
  txrx <- spawnLocal (runReaderT (forever $ node True throttle) env)
  register "iohk" txrx
  liftIO $ threadDelay (sendTime * 1000000)
  exit txrx "timeout"
  unregister "iohk"

  -- just receive
  rx <- spawnLocal (runReaderT (forever $ node False throttle) env)
  register "iohk" rx
  liftIO $ threadDelay (gracePeriod * 1000000)
  exit rx "grace period"
  unregister "iohk"

  -- show all
  chain <- liftIO $ readTVarIO $ _chain env
  (liftIO . print . show . result) chain
