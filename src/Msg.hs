module Msg where

import Data.Binary     (Binary)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

type Timestamp = Integer
data Message = Message
  { timestamp :: Int    -- ^ time of msg generation
  , value     :: Double -- ^ value to append
  , total     :: Double -- ^ scaled sum of all the previous values
                        --   monotonically increasing
  } deriving (Eq, Show, Generic, Typeable)

instance Binary Message
-- instance Ord Message where
--   compare l r = timestamp l `compare` timestamp r
instance Ord Message where
  compare l r = total l `compare` total r

initialMsg :: Message
initialMsg = Message
  { timestamp = 0
  , value = 0
  , total = 0
  }

-- | Chain definition and operations
type Chain = [Message]

emptyChain :: Chain
emptyChain = []

longerChain :: Chain -> Chain -> Chain
longerChain a b = if length a < length b then b
                                         else a

mac :: Double -> Int -> Double -> Double
mac acc multiplier x = acc + ((fromIntegral multiplier) * x)

scaledSum :: Chain -> Double
scaledSum xs = foldl (\g x !i -> mac (g (i + 1)) i (value x)) (const 0) xs 0

result :: Chain -> (Int, Double)
result xs = (length xs - 1, scaledSum xs)

canAppend :: Chain -> Message -> Bool
canAppend chain msg = total msg == scaledSum chain

append :: Message -> Chain -> Maybe Chain
append msg chain = let new = msg : chain
                   in if isValidChain new then Just new
                                          else Nothing

isValidChain :: Chain -> Bool
isValidChain [] = True
isValidChain (x:xs) = (total x == scaledSum xs) && isValidChain xs

-- | All messages that can be sent between nodes
data Query
  = QueryChain
  | RespChain Chain
  | AppendMsg Message
  deriving (Eq, Show, Generic, Typeable)
instance Binary Query

isQueryChain, isRespChain, isAppendMsg :: Query -> Bool
isQueryChain x | QueryChain <- x = True
               | otherwise = False
isRespChain x | (RespChain _) <- x = True
              | otherwise = False
isAppendMsg x | (AppendMsg _) <- x = True
              | otherwise = False

