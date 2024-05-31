{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TogglAPI where

import           Control.Exception     (catch, throw)
import           Control.Lens          ((&), (?~), (^?))
import           Control.Monad         (when)
import           Data.Aeson            (FromJSON, ToJSON (toJSON),
                                        Value (Object), object, (.=))
import           Data.Aeson.Lens       (_JSON, _Null, _Number, key)
import           Data.Aeson.Types      (Pair)
import qualified Data.ByteString.Char8 as C
import           Data.HashMap.Strict   (fromList, union)
import           Data.Maybe            (fromJust)
import           Data.Text             as T
import           Data.Time             (ZonedTime, diffUTCTime,
                                        nominalDiffTimeToSeconds,
                                        zonedTimeToUTC)
import           GHC.Generics          (Generic)
import           Network.HTTP.Client   (HttpException (HttpExceptionRequest),
                                        HttpExceptionContent (StatusCodeException),
                                        Response (responseStatus))
import           Network.HTTP.Types    (Status (statusCode))
import           Network.Wreq          (auth, basicAuth, defaults, getWith,
                                        postWith, responseBody)
import           Text.Printf           (printf)
import           TimeTag               (parseTagTime)

data TimeEntry
  = TimeEntry
  { description :: String
  , project_id :: Int
  , start :: ZonedTime
  , stop :: ZonedTime
  } deriving (Show, Generic)

instance ToJSON TimeEntry

data Project
  = Project
  { id :: Pid
  , name :: String
  } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON   Project

type Pid = Int
type ProjectName = String
type WorkspaceId = Int
type ApiKey = String

class TogglAPI api where
  createProject :: api -> ApiKey -> WorkspaceId -> ProjectName -> IO Pid
  createTimeEntry :: api -> ApiKey -> WorkspaceId -> TimeEntry -> IO ()
  listWorkspaceProjects :: api -> ApiKey -> WorkspaceId -> IO [Project]
  
data TogglAPIImpl
  = TogglAPIImpl
  { host :: String
  , port :: Maybe String
  } deriving Show

instance TogglAPI TogglAPIImpl where
  -- https://engineering.toggl.com/docs/projects#create-a-project
  createProject impl apiKey wid name = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v9/workspaces/"
          ++ show wid
          ++ "/projects"
    let body
          = object
          [ "name" .= T.pack name
          , "active" .= True
          ]
    r <- postWith opts url body
      `catch`
      \e@(HttpExceptionRequest _ (StatusCodeException r msg)) -> do
      when ((statusCode . responseStatus $ r) == 400
            && "Name has already been taken" `C.isInfixOf` msg) $ do
        error (printf "Project '%s' exists under wid %i\n" name wid)
      throw e

--    trace ("responseBody: " ++ show r) undefined
    case r ^? responseBody . key "id" . _Number of
      Just id -> return . round $ id
      Nothing -> error "Failed to parse project id in creation response"

  -- https://engineering.toggl.com/docs/tracking#about-the-duration-field
  createTimeEntry impl apiKey wid entry = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v9/workspaces/"
          ++ show wid
          ++ "/time_entries"
    let duration
          = truncate . nominalDiffTimeToSeconds
          $ diffUTCTime
            (zonedTimeToUTC . stop  $ entry)
            (zonedTimeToUTC . start $ entry)
          :: Int
    let body
          = entry `addFields`
            [ "created_with" .= T.pack "task-tags"
            , "duration"     .= duration
            , "wid" .= wid
            ]
    postWith opts url body
    return ()

  -- https://engineering.toggl.com/docs/projects/index.html#get-projects
  listWorkspaceProjects impl apiKey wid = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v9/workspaces/" ++ show wid ++ "/projects"
          ++ "?active=both&per_page=200"
    r <- getWith opts url
    case r ^? responseBody . _JSON of
      Just tes -> return tes
      Nothing  ->
        case r ^? responseBody . _Null of
          Just () -> return []
          Nothing
            -> error $ "Failed to parse response body as JSON:\n" ++ show r

addFields :: ToJSON a => a -> [Pair] -> Value
addFields a pairs = Object $ hm `union` fromList pairs
  where Object hm = toJSON a

togglAPI = TogglAPIImpl "api.track.toggl.com" Nothing

testListWorkspaceProjects = do
  let apiKey = "<Provide the key>"
  listWorkspaceProjects togglAPI apiKey 56773

testCreateProject = do
  let apiKey = "<Provide the key>"
  createProject togglAPI apiKey 8403993 "testProject"

testCreateTimeEntry = do
  let apiKey = "<Provide the key>"
  createTimeEntry
    togglAPI apiKey 8403993
    TimeEntry
    { description = "Test time entry"
    , project_id = 203126652
    , start = fromJust . parseTagTime $ "20240530 22:08:34 +0100"
    , stop = fromJust . parseTagTime $ "20240530 22:10:00 +0100"
    }
