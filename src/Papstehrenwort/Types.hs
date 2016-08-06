module Papstehrenwort.Types where

import Protolude
import Numeric.Natural
import Data.Time.Calendar (Day)
import Network.URL

data Task = Task { tTitle :: Text
                 , tDescription :: Text
                 , tHow :: Maybe Text
                 , tUrl :: Maybe URL
                 , tRecur :: Natural
                 , tStart :: Day
                 } deriving (Show, Eq)

