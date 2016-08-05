module Papstehrenwort.Types where

import Protolude
import Data.Time.Calendar (Day)
import Network.URL

data Task = Task { tTitle :: Text
                 , tDescription :: Text
                 , tHow :: Maybe Text
                 , tUrl :: Maybe URL
                 , tRecur :: Integer
                 , tStart :: Day
                 } deriving (Show, Eq)

