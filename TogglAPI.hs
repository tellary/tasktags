{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TogglAPI where

import           Control.Exception     (catch, throw)
import           Control.Lens          ((&), (?~), (^?))
import           Control.Monad         (when)
import           Data.Aeson            (FromJSON, ToJSON (toJSON),
                                        Value (Object), object, (.=))
import           Data.Aeson.Lens       (key, _JSON, _Null, _Number)
import           Data.Aeson.Types      (Pair)
import qualified Data.ByteString.Char8 as C
import           Data.HashMap.Strict   (fromList, union)
import           Data.Text             as T
import           Data.Time             (ZonedTime, diffUTCTime,
                                        nominalDiffTimeToSeconds,
                                        zonedTimeToUTC)
import           GHC.Generics          (Generic)
import           Network.HTTP.Client   (HttpException (HttpExceptionRequest),
                                        HttpExceptionContent (
                                           StatusCodeException),
                                        Response (responseStatus))
import           Network.HTTP.Types    (Status (statusCode))
import           Network.Wreq          (auth, basicAuth, defaults, getWith,
                                        postWith, responseBody)
import           Text.Printf           (printf)
import           ZonedTime             ()

data TimeEntry
  = TimeEntry
  { description :: String
  , pid :: Int
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
  createTimeEntry :: api -> ApiKey -> TimeEntry -> IO ()
  listWorkspaceProjects :: api -> ApiKey -> WorkspaceId -> IO [Project]
  
data TogglAPIImpl
  = TogglAPIImpl
  { host :: String
  , port :: Maybe String
  } deriving Show

instance TogglAPI TogglAPIImpl where
  -- https://github.com/toggl/toggl_api_docs/blob/master/chapters/projects.md#create-project
  createProject impl apiKey wid name = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v8/projects"
    let body
          = object
          [ "project" .= object [ "name" .= T.pack name, "wid" .= wid ] ]
    r <- postWith opts url body
      `catch`
      \e@(HttpExceptionRequest _ (StatusCodeException r msg)) -> do
      when ((statusCode . responseStatus $ r) == 400
            && "Name has already been taken" `C.isInfixOf` msg) $ do
        error (printf "Project '%s' exists under wid %i\n" name wid)
      throw e

    case r ^? responseBody . key "data" . key "id" . _Number of
      Just id -> return . round $ id
      Nothing -> error "Failed to parse project id in creation response"

  -- https://github.com/toggl/toggl_api_docs/blob/master/chapters/time_entries.md#create-a-time-entry
  createTimeEntry impl apiKey entry = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v8/time_entries"
    let duration
          = truncate . nominalDiffTimeToSeconds
          $ diffUTCTime
            (zonedTimeToUTC . stop  $ entry)
            (zonedTimeToUTC . start $ entry)
          :: Int
    let body
          = object
          [ "time_entry"
            .= entry `addFields`
               [ "created_with" .= T.pack "task-tags"
               , "duration"     .= duration
               ]
          ]
    postWith opts url body
    return ()

  -- https://github.com/toggl/toggl_api_docs/blob/master/chapters/workspaces.md#get-workspace-projects
  listWorkspaceProjects impl apiKey wid = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v8/workspaces/" ++ show wid ++ "/projects"
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
  createProject togglAPI apiKey 2806099 "testProject"
