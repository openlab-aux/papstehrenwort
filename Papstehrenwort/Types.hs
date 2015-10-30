module Papstehrenwort.Types where

import System.Cron       (CronSchedule ())
import Network.Mail.Mime (Address ())

type Minute = Integer
data Task = Task {
    tTitle       :: String
  , tDescription :: String
  , tSchedule    :: CronSchedule
  , tOverdueTime :: Minute
  , tUsers       :: [User]
  } deriving (Show, Eq)

data User = User Address
  deriving (Show, Eq)
