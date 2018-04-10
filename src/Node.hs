module Node where

import Prelude hiding (init)
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.STM             ( TVar, atomically
                                                    , modifyTVar', newTVar
                                                    , readTVar, readTVarIO, writeTVar
                                                    )
import           Control.Monad.Reader
import           Control.Distributed.Process.Lifted ( Process, exit, expect
                                                    , matchAny, matchIf, handleMessage_
                                                    , receiveTimeout, register
                                                    , nsend
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

modifyChain :: (Chain -> Chain) -> ReaderT Env Process ()
modifyChain f = do
  tchain <- asks _chain
  liftIO $ atomically $ modifyTVar' tchain f

processMsg :: Query -> ReaderT Env Process ()
processMsg (AppendMsg msg)= do
  tchain <- asks _chain
  chain <- liftIO $ readTVarIO tchain
  case append msg chain of
    Just new -> liftIO $ atomically $ writeTVar tchain new
    Nothing -> do
      lift $ P2P.nsendPeers "iohk" QueryChain
      lift $ say "request chain"

processMsg (RespChain chain) = do
  liftIO $ guard (isValidChain chain)
  modifyChain (longerChain chain)

processMsg QueryChain = do
  chain <- getChain
  lift $ P2P.nsendPeers "iohk" (RespChain chain)

runNode :: ReaderT Env Process () -> Env -> Process ()
runNode = runReaderT

newMsg :: ReaderT Env Process ()
newMsg = do
  msg <- generateMsg
  liftIO $ threadDelay 1000000
  lift $ P2P.nsendPeers "iohk" (AppendMsg msg)
  -- chain <- getChain
  -- say ("chain: " <> show chain)
  -- say ("sending: " <> show msg)

appendableMsg :: Chain -> Query -> Bool
appendableMsg chain (AppendMsg msg) = canAppend chain msg
appendableMsg _ _ = False

dropMsg :: Query -> Process ()
dropMsg msg = do
  say ("useless: " <> show msg)
  P2P.nsendPeers "iohk" QueryChain

processMBox :: ReaderT Env Process (Maybe ())
processMBox = do
  env <- ask
  chain <- getChain
  receiveTimeout 0
    [ matchIf (appendableMsg chain) (process env) -- | append all msgs first
    , matchIf isRespChain (process env)           -- | check if we have longer chains in queue
    , matchIf isQueryChain (process env)          -- | respond with our chain
    , matchAny (\msg -> handleMessage_ msg dropMsg) -- | rest
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
  chain <- liftIO $ atomically $ newTVar [initialMsg]
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
  chain <- liftIO $ readTVarIO $ _chain env
  (liftIO . print . show) chain
  liftIO $ print $ "msgs: " <> (show $ length chain )
