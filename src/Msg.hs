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
instance Ord Message where
  compare l r = timestamp l `compare` timestamp r

initialMsg :: Message
initialMsg = Message
  { timestamp = 0
  , value = 0
  }

type Chain = [Message]

data Query
  = QueryChain
  | RespChain Chain
  | AppendMsg Message
  deriving (Eq, Show, Generic, Typeable)
instance Binary Query

mac :: Double -> Int -> Double -> Double
mac acc multiplier x = acc + ((fromIntegral multiplier) * x)

scaledSum :: Chain -> Double
scaledSum xs = foldl (\g x !i -> mac (g (i + 1)) i (value x)) (const 0) xs 0

result :: Chain -> (Int, Double)
result xs = (length xs - 1, scaledSum xs)

isQueryChain, isRespChain, isAppendMsg :: Query -> Bool
isQueryChain x | QueryChain <- x = True
               | otherwise = False
isRespChain x | (RespChain x) <- x = True
              | otherwise = False
isAppendMsg x | (AppendMsg _) <- x = True
              | otherwise = False

emptyChain :: Chain
emptyChain = []

