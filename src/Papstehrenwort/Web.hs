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

type UI = Header "Accept-Language" I.Lang :> Get '[HTML] (H.Translated H.Site)
          :<|> "api"    :> API
          :<|> "js"     :> Raw
          :<|> "static" :> Raw

type API = APIcurrent
           :<|> "v0" :> APIv0
type APIcurrent = APIv0

type APIv0 = "tasks" :> Get '[JSON] [Task]

tasks :: [Task]
tasks = [
  Task { tTitle = "test"
       , tDescription = "desc"
       , tHow = Nothing
       , tUrl = Nothing
       , tRecur = 5
       , tStart = ModifiedJulianDay 234 }
  ]

taskServer :: Server UI
taskServer = site
             -- TODO: don’t use relative paths
             :<|> api
             :<|> serveDirectory "js"
             :<|> serveDirectory "static" 
  where
    site lang = do
      d <- liftIO $ utctDay <$> getCurrentTime
      return $ H.Trans (I.fromMarkup <$> I.renderMessage I.Default
                         (maybe I.EN identity lang))
                       $ H.Site $ H.TaskList { H.tlTasks = tasks
                                             , H.tlToday = d }
    api = apiv0 :<|> apiv0
    apiv0 = pure tasks

userApi :: Proxy UI
userApi = Proxy

app :: Application
app = serve userApi taskServer

