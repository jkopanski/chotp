module Opts where

import Data.Semigroup      ((<>))
import Options.Applicative

data Config = Config
  { _send     :: Int
  , _wait     :: Int
  , _seed     :: Int
  , _host     :: String
  , _port     :: Int
  , _throttle :: Bool
  , _peers    :: [String]
  } deriving (Eq, Show)

config :: Parser Config
config = Config <$> send
                <*> wait
                <*> seed
                <*> host
                <*> port
                <*> throttle
                <*> peers

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

throttle :: Parser Bool
throttle = switch
  ( long "throttle"
 <> short 't'
 <> help "Limit message sending to 1 per second" )

peer :: Parser String
peer = strArgument
  ( metavar "PEER"
 <> help "Address of peer node" )

peers :: Parser [String]
peers = some peer

host :: Parser String
host = strOption
  ( metavar "HOST"
 <> long "host"
 <> value "127.0.0.1"
 <> help "Bind to HOST" )

options = info (config <**> helper)
  ( fullDesc
 <> progDesc "IOHK test task"
 <> header "node - send and receive messages" )
