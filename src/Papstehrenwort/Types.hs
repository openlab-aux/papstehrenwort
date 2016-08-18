{-# LANGUAGE DeriveGeneric #-}
module Papstehrenwort.Types where

import Protolude
import Numeric.Natural
import Data.Time.Calendar (Day)
import qualified Network.URL as U
import Data.Aeson (ToJSON(..), FromJSON(..))


newtype URL = URL U.URL deriving (Eq, Show, Generic)
exportURL :: URL -> Text
exportURL (URL u) = toS $ U.exportURL u

data Task = Task { tTitle :: Text
                 , tDescription :: Text
                 , tHow :: Maybe Text
                 , tUrl :: Maybe URL
                 , tRecur :: Natural
                 , tStart :: Day
                 } deriving (Show, Eq, Generic)


instance ToJSON Task
instance ToJSON URL where
  toJSON = toJSON . exportURL
