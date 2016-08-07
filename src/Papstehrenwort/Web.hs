{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}
module Papstehrenwort.Web where

import Protolude
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze (HTML)
import Servant.Utils.StaticFiles (serveDirectory)
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime, utctDay)
import Network.Wai (Application)

import Papstehrenwort.Types
import qualified Papstehrenwort.Web.Html as H
import qualified Papstehrenwort.I18n as I

type ApiType = Get '[HTML] (H.Translated H.Site)
          :<|> "js"     :> Raw
          :<|> "static" :> Raw

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
taskServer =
  (do
      d <- liftIO $ utctDay <$> getCurrentTime
      return $ H.Trans ( I.fromMarkup <$> I.renderMessage I.Default I.EN
                       , H.Site $ H.TaskList { H.tlTasks = tasks
                                             , H.tlToday = d } ))
  -- TODO: don’t use relative paths
  :<|> serveDirectory "js"
  :<|> serveDirectory "static" 

userApi :: Proxy ApiType
userApi = Proxy

app :: Application
app = serve userApi taskServer

