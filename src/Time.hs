module Time where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time.Clock.POSIX (getPOSIXTime)

data Time = Time
  { _seconds :: IO Int
  }

defaultTime :: Time
defaultTime = Time
  { _seconds = round <$> getPOSIXTime
  }

class HasTime env where
  time :: env -> Time
instance HasTime Time where
  time = id

class HasSeconds env where
  seconds :: env -> IO Int
instance HasSeconds (IO Int) where
  seconds = id
instance HasSeconds Time where
  seconds = _seconds

class Monad m => MonadTime m where
  getSeconds :: m Int

instance (HasSeconds env, MonadIO m) => MonadTime (ReaderT env m) where
  getSeconds = liftIO . seconds =<< ask
