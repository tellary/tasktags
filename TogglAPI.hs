{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TogglAPI where

import           Control.Lens          ((&), (.~), (?~), (^.))
import           Data.Aeson            (FromJSON)
import qualified Data.ByteString.Char8 as C
import           Data.Time             (ZonedTime)
import           GHC.Generics          (Generic)
import           Network.Wreq          (asJSON, auth, basicAuth, defaults,
                                        getWith, param, responseBody)

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
    r <- asJSON =<< getWith opts url
    return $ r ^. responseBody

togglAPI = TogglAPIImpl "api.track.toggl.com" Nothing

testListWorkspaceProjects = do
  let apiKey = "<Provide the key>"
  listWorkspaceProjects togglAPI apiKey 56773
