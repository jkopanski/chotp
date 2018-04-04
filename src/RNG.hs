module RNG where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.Reader
import System.Random hiding (next)

data Rand = Rand
  { _gen  :: !(TVar StdGen)
  }

class HasRand env where
  rand :: env -> Rand
instance HasRand Rand where
  rand = id

class HasGen env where
  gen :: env -> TVar StdGen
instance HasGen (TVar StdGen) where
  gen = id
instance HasGen Rand where
  gen = _gen

class Monad m => MonadRand m where
  set  :: StdGen -> m ()
  next :: m Double

instance (HasGen env, MonadIO m) => MonadRand (ReaderT env m) where
  set g = do
    env <- ask
    liftIO $ atomically $ writeTVar (gen env) g

  next = do
    env <- ask
    stdgen <- liftIO $ atomically $ readTVar (gen env)
    let (r, newgen) = randomR (0, 1) stdgen
    set newgen
    if r /= 0 then pure r
              else next

init :: MonadIO m => Int -> m (TVar StdGen)
init seed = liftIO $ atomically $ newTVar (mkStdGen seed)
