{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TogglAPI where

import           Control.Exception     (catch, throw)
import           Control.Lens          ((&), (.~), (?~), (^.), (^?))
import           Control.Monad         (when)
import           Data.Aeson            (FromJSON, ToJSON,
                                        Value (Number, Object, String))
import           Data.Aeson.Lens       (key, _JSON, _Number, _String)
import qualified Data.ByteString.Char8 as C
import           Data.HashMap.Strict   (fromList)
import           Data.Text             as T
import           Data.Time             (ZonedTime)
import           GHC.Generics          (Generic)
import           Network.HTTP.Client   (HttpException (HttpExceptionRequest),
                                        HttpExceptionContent (
                                           StatusCodeException),
                                        Response (responseStatus))
import           Network.HTTP.Types    (Status (statusCode))
import           Network.Wreq          (asJSON, auth, basicAuth, defaults,
                                        getWith, param, postWith, responseBody)
import           Text.Printf           (printf)

data TimeEntry
  = TimeEntry
  { description :: String
  , pid :: Int
  , start :: ZonedTime
  , stop :: ZonedTime
  } deriving Show

data Project
  = Project
  { id :: Pid
  , name :: String
  } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project

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
          & param "user_agent" .~ ["task-tags"]
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v8/projects"
    let body
          = Object
          ( fromList
            [ ("project"
              , Object
                ( fromList
                  [ ("name", String . T.pack $ name)
                  , ("wid" , Number . fromIntegral $ wid)
                  ]
                )
              )
            ]
          )
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

  -- https://github.com/toggl/toggl_api_docs/blob/master/chapters/workspaces.md#get-workspace-projects
  listWorkspaceProjects impl apiKey wid = do
    let opts
          = defaults
          & auth ?~ basicAuth (C.pack apiKey) "api_token"
          & param "user_agent" .~ ["task-tags"]
    let url
          =  "https://" ++ host impl
          ++ maybe "" (\p -> ":" ++ show p) (port impl)
          ++ "/api/v8/workspaces/" ++ show wid ++ "/projects"
    r <- getWith opts url
    case r ^? responseBody . _JSON of
      Just tes -> return tes
      Nothing  -> do
        if (r ^. responseBody . _String == "")
          then return []
          else error $ "Failed to parse response body as JSON:\n" ++ show r

togglAPI = TogglAPIImpl "api.track.toggl.com" Nothing

testListWorkspaceProjects = do
  let apiKey = "<Provide the key>"
  listWorkspaceProjects togglAPI apiKey 56773

testCreateProject = do
  let apiKey = "<Provide the key>"
  createProject togglAPI apiKey 2806099 "testProject"
