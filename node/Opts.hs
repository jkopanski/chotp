module Opts where

import Data.Semigroup      ((<>))
import Options.Applicative

data Config = Config
  { _send :: Int
  , _wait :: Int
  , _seed :: Int
  , _port :: Int
  } deriving (Eq, Show)

config :: Parser Config
config = Config <$> send <*> wait <*> seed <*> port

send :: Parser Int
send = option auto
  ( long "send-for"
 <> short 'k'
 <> metavar "K"
 <> value 30
 <> help "Send messages for K seconds" )

wait :: Parser Int
wait = option auto
  ( long "wait-for"
 <> short 'l'
 <> metavar "L"
 <> value 30
 <> help "Wait K seconds for messages" )

seed :: Parser Int
seed = option auto
  ( long "with-seed"
 <> short 's'
 <> metavar "S"
 <> value 1234
 <> help "Use S as RNG seed" )

port :: Parser Int
port = option auto
  ( long "port"
 <> short 'p'
 <> metavar "P"
 <> value 9500
 <> help "Port number P" )

options = info (config <**> helper)
  ( fullDesc
 <> progDesc "IOHK test task"
 <> header "node - send and receive messages" )

