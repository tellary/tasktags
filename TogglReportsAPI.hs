{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TogglReportsAPI where

import           Control.Lens          ((&), (.~), (?~), (^?))
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Aeson.Lens       (key, _JSON, _Number)
import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T
import           Data.Time             (Day, ZonedTime)
import           GHC.Generics          (Generic)
import           Network.Wreq          (auth, basicAuth, defaults, getWith,
                                        param, responseBody)
import           Prelude               hiding (until)
import           ZonedTime             ()

data DateRange
  = DateRange
  { since :: Day
  , until :: Day
  } deriving Show

data ReportReq
  = ReportReq
  { workspaceId :: Int
  , dateRange :: DateRange
  } deriving Show

data TimeEntry
  = TimeEntry { start :: ZonedTime, end :: ZonedTime }
  deriving (Show, Generic)

instance FromJSON TimeEntry
instance ToJSON   TimeEntry

data DetailedReport
  = DetailedReport
  { timeEntries :: [TimeEntry]
  , perPage     :: Int
  } deriving Show

data DetailedReportReq
  = DetailedReportReq
  { detailedReportBase :: ReportReq
  , detailedReportPage :: Int
  }

type ApiKey = String

class TogglReportsAPI api where
  detailedReport :: api -> ApiKey -> DetailedReportReq -> IO DetailedReport

getAllPages pageF p = do
  (entries, perPage) <- pageF p
  if length entries < perPage
    then return entries
    else do
      tail <- getAllPages pageF (p + 1)
      return $ entries ++ tail

data TogglReportsAPIImpl
  = TogglReportsAPIImpl
  { host :: String
  , port :: Maybe Int
  } deriving Show

togglReportsAPI = TogglReportsAPIImpl "api.track.toggl.com" Nothing

instance TogglReportsAPI TogglReportsAPIImpl where
  detailedReport impl apiKey req = do
    let opts = defaults
               & auth ?~ basicAuth (C.pack apiKey) "api_token"
               & param "user_agent" .~ ["task-tags"]
               & param "workspace_id"
               .~ [ T.pack . show . workspaceId . detailedReportBase $ req ]
               & param "since"
               .~ [ T.pack . show
                    . since . dateRange . detailedReportBase $ req ]
               & param "until"
               .~ [ T.pack . show
                    . until . dateRange . detailedReportBase $ req ]
               & param "page" .~ [ T.pack . show . detailedReportPage $ req ]
    let url = "https://" ++ host impl
              ++ maybe "" (\p -> ":" ++ show p) (port impl)
              ++  "/reports/api/v2/details"
    r <- getWith opts url
    let perPage = case r ^? responseBody . key "per_page" . _Number of
                    Just perPage -> round perPage
                    Nothing      -> error "Failed to parse 'per_page'"
    case r ^? responseBody . key "data" . _JSON of
      Just tes -> return (DetailedReport tes perPage)
      Nothing  -> error "Failed to parse time entries"

test = do
  let apiKey = "<Provide the key>"
  let range = DateRange (read "2021-05-20") (read "2021-05-21")
  let req = DetailedReportReq (ReportReq 59866 range) 1
  detailedReport togglReportsAPI apiKey req
