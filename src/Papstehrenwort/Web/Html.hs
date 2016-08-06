{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Papstehrenwort.Web.Html where

import Protolude
import Data.Time.Calendar (Day)
import Network.URL (exportURL)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze (ToMarkup)

import qualified Papstehrenwort.Types as T
import qualified Papstehrenwort.Scheduler as S

data TaskList = TaskList { tlTasks :: [T.Task]
                         , tlToday :: Day }

instance ToMarkup TaskList where
  toMarkup TaskList{..} =
    table ! class_ "table" $ do
      thead $ do
        th "Title"
        th "Description"
        th "Url"
        th "Next"
      tbody $ do
        mconcat $ flip map tlTasks $ \T.Task{..} -> do
          dat tTitle
          dat tDescription
          dat $ maybe "" (toS.exportURL) tUrl
          dat . show $ S.nextOccurrence tStart tRecur tlToday
            where dat = td . toHtml
