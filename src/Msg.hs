module Msg where

import Data.Binary     (Binary)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

type Timestamp = Integer
data Message = Message
  { timestamp :: Int
  , value     :: Double
  } deriving (Eq, Show, Generic, Typeable)

instance Binary Message

type Chain = [Message]

emptyChain :: Chain
emptyChain = []

