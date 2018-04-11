module Node where

import Prelude hiding (init, pred)
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.STM             ( TVar, atomically
                                                    , modifyTVar', newTVar
                                                    , readTVar, readTVarIO, writeTVar
                                                    )
import           Control.Monad.Reader
import           Control.Distributed.Process.Lifted ( Process, ReceivePort, SendPort
                                                    , exit, expect
                                                    , getSelfPid
                                                    , matchAny, matchIf, handleMessage_
                                                    , receiveTimeout, register
                                                    , newChan, nsend
                                                    , receiveChan
                                                    , say, send, sendChan, spawnLocal
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
      if total msg < scaledSum chain
         -- if total msg < scaledSum chain
         -- the other node has a problem, we don't ;)
         then pure ()
         -- no sense in going forward,
         -- update the chain first
          else do
            (sendChain, recChain) <- lift newChan
            lift $ send pid (QueryChain sendChain)
            say "waiting for chain..."
            newChain <- lift $ receiveChan recChain
            say "got new chain"
            if isValidChain newChain
               then modifyChain (longerChain newChain)
               else pure ()

processMsg (QueryChain sendChain) = do
  chain <- getChain
  lift $ sendChan sendChain chain

runNode :: ReaderT Env Process () -> Env -> Process ()
runNode = runReaderT

newMsg :: ReaderT Env Process ()
newMsg = do
  msg <- generateMsg
  lift . P2P.nsendPeers "iohk" . AppendMsg msg =<< lift getSelfPid
  liftIO $ threadDelay 1000000

appendableMsg :: Chain -> Query -> Bool
appendableMsg chain (AppendMsg msg _) = canAppend chain msg
appendableMsg _ _ = False

dropMsg :: Query -> Process ()
dropMsg msg = pure ()

processMBox :: ReaderT Env Process (Maybe ())
processMBox = do
  env <- ask
  chain <- getChain
  receiveTimeout 0
    [ matchAny (\msg -> handleMessage_ msg (\m -> process env m))
    --   matchIf (appendableMsg chain) (process env) -- | append all msgs first
    -- , matchIf isRespChain (process env)           -- | check if we have longer chains in queue
    -- , matchIf isQueryChain (process env)          -- | respond with our chain
    -- , matchAny (\msg -> handleMessage_ msg dropMsg) -- | rest
    ]

    where process :: Env -> Query -> Process ()
          process e q = runNode (processMsg q) e

-- | idea is as follows:
-- 1. check mailbox for new messages,
--    process them until mbox is empty
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
  tchain <- liftIO $ atomically $ newTVar [initialMsg]
  let env = Env tchain defaultTime rand True

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
  chain <- liftIO $ readTVarIO $ _chain env
  (liftIO . print . show) chain
  liftIO $ print $ "msgs: " <> (show $ length chain )
