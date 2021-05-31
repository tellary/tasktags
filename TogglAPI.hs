module TogglAPI where

import Data.Time (ZonedTime)

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
  } deriving Show

type Pid = Int
type ProjectName = String
type WorkspaceId = Int

class TogglAPI api where
  createProject :: api -> WorkspaceId -> ProjectName -> IO Pid
  createTimeEntry :: api -> TimeEntry -> IO ()
  listWorkspaceProjects :: api -> WorkspaceId -> IO [Project]
  
data TogglAPIImpl
  = TogglAPIImpl
  { host :: String
  , port :: Maybe String
  } deriving Show

instance TogglAPI TogglAPIImpl where

togglAPI = TogglAPIImpl "api.track.toggl.com" Nothing
