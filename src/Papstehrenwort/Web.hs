{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, DeriveGeneric #-}
module Papstehrenwort.Web where

import Protolude
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Network.Wai (Application)
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze (HTML)
import Servant.Utils.StaticFiles (serveDirectory)

import Papstehrenwort.Types
import qualified Papstehrenwort.Web.Html as H
import qualified Papstehrenwort.I18n as I

-- | type for servant routes
type Routes = Header "Accept-Language" I.Lang :> Get '[HTML] (H.Translated H.Site)
              :<|> "api"    :> API
              :<|> "js"     :> Raw
              :<|> "static" :> Raw

-- | The api is versioned. You can get the available ones on /api
type API = Get '[JSON] ApiVersions
           :<|> "v0" :> APIv0
type APIcurrent = APIv0
-- | constant, that should stay in sync with API type
apiVersions :: ApiVersions
apiVersions = ApiVersions
              { currentVersion = Semver 0 0 0
              , allVersions = [ Semver 0 0 0 ] }

type APIv0 = "tasks" :> Get '[JSON] [Task]


-- | Simple http://semver.org/ type
data Semver = Semver
  { major :: Integer
  , minor :: Integer
  , patch :: Integer
  } deriving (Show, Eq, Generic)

-- | API versions of the server
data ApiVersions = ApiVersions
  { currentVersion :: Semver
  , allVersions :: [Semver]
  } deriving (Show, Eq, Generic)

instance ToJSON ApiVersions
instance ToJSON Semver

-- | tmp test task
tasks :: [Task]
tasks = [
  Task { tTitle = "test"
       , tDescription = "desc"
       , tHow = Nothing
       , tUrl = Nothing
       , tRecur = 5
       , tStart = ModifiedJulianDay 234 }
  ]

-- | server implementation
taskServer :: Server Routes
taskServer = site
             -- TODO: don’t use relative paths
             :<|> api
             :<|> serveDirectory "js"
             :<|> serveDirectory "static" 
  where
    site lang = do
      d <- liftIO $ utctDay <$> getCurrentTime
      return $ H.Trans (
        -- render markup to HTML
        I.fromMarkup <$> I.renderMessage I.Default
                         -- translate according to browser settings
                         (maybe I.EN identity lang))
                       $ H.Site $ H.TaskList { H.tlTasks = tasks
                                             , H.tlToday = d }
    api = (pure apiVersions) :<|> apiv0
    apiv0 = pure tasks

-- | api proxy
userApi :: Proxy Routes
userApi = Proxy

-- | servant WAI application
app :: Application
app = serve userApi taskServer

