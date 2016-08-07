{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Papstehrenwort.Web.Html where

import Protolude as P
import Data.Time.Calendar (Day)
import Network.URL (exportURL)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze (ToMarkup)

import qualified Papstehrenwort.Types as T
import qualified Papstehrenwort.Scheduler as S
import qualified Papstehrenwort.I18n as I


data Site = Site { sTasks :: TaskList }

newtype Translated a = Trans (I.UIMessages -> Html, a)

instance ToMarkup (Translated Site) where
  toMarkup (Trans (t, Site{..})) =
    docTypeHtml $ do
      H.head $ do
        H.title $ t I.Title
        H.link ! href "/static/screen.css" ! rel "stylesheet"
        script ! src "/static/jquery.min.js" $ mempty
        meta ! charset "utf-8"
      body $ do
        main $ do
          header $ do
            h1 $ t I.Title
            h2 . small $ t I.Tagline


data TaskList = TaskList { tlTasks :: [T.Task]
                         , tlToday :: Day }

instance ToMarkup (Translated TaskList) where
  toMarkup (Trans (t, TaskList{..})) =
    let desc = th . t
        dat  = td . toHtml in
    table ! class_ "table" $ do
      thead $ do
        desc I.TaskTitle
        desc I.TaskDescription
        desc I.TaskUrl
        desc I.TaskNextOccur
      tbody $ do
        mconcat $ flip P.map tlTasks $ \T.Task{..} -> do
          dat tTitle
          dat tDescription
          dat $ maybe "" (toS.exportURL) tUrl
          dat . show $ S.nextOccurrence tStart tRecur tlToday

