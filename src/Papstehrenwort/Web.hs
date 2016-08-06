{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}
module Papstehrenwort.Web where

import Protolude
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze (HTML)
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime, utctDay)
import Network.Wai (Application)

import Papstehrenwort.Types
import qualified Papstehrenwort.Web.Html as Html

type ApiType = Get '[HTML] Html.TaskList

tasks :: [Task]
tasks = [
  Task { tTitle = "test"
       , tDescription = "desc"
       , tHow = Nothing
       , tUrl = Nothing
       , tRecur = 5
       , tStart = ModifiedJulianDay 234 }
  ]

taskServer :: Server ApiType
taskServer = do
  d <- liftIO $ utctDay <$> getCurrentTime
  pure $ Html.TaskList { Html.tlTasks = tasks
                       , Html.tlToday = d }

userApi :: Proxy ApiType
userApi = Proxy

app :: Application
app = serve userApi taskServer

