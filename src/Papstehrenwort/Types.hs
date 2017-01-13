{-# LANGUAGE DeriveGeneric #-}
module Papstehrenwort.Types where

import Protolude
import Data.Time.Calendar (Day)
import qualified Network.URL as U
-- import Data.Aeson (ToJSON(..), FromJSON(..))


-- | we wrap the @U.URL@ type because it works with Text
--   and does not provide aeson instances
newtype URL = URL U.URL deriving (Eq, Show, Generic)
exportURL :: URL -> Text
exportURL (URL u) = toS $ U.exportURL u
importURL :: Text -> Maybe URL
importURL t = URL <$> U.importURL (toS t)

-- | A task that needs to be repeatet regularly
data Task = Task
  { tTitle :: Text
  , tDescription :: Text
  , tHow :: Maybe Text
  , tUrl :: Maybe URL
  , tRecur :: Int
  , tStart :: Day
  } deriving (Show, Eq, Generic)

-- | A splasher is a papstehrenwort user
data Splasher = Splasher
  { sNick :: Text
  , sMail :: Text
  } deriving (Show, Eq, Generic)


-- instance ToJSON Task
-- instance ToJSON URL where
--   toJSON = toJSON . exportURL
