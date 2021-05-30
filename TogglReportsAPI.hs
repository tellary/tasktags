module TogglReportsAPI where

import Data.Time (Day, LocalTime)

data ReportReq
  = ReportReq
  { workspaceId :: Int
  , dateRange :: DateRange
  } deriving Show

data DateRange
  = DateRange
  { since :: Day
  , until :: Day
  } deriving Show

type DetailedReport = [TimeEntry]

data DetailedReportReq
  = DetailedReportReq
  { detailedReportBase :: ReportReq
  , detailedReportPage :: Int
  }

data TimeEntry
  = TimeEntry { start :: LocalTime, end :: LocalTime }

type ApiKey = String

class TogglReportsAPI api where
  detailedReport :: api -> ApiKey -> DetailedReportReq -> IO DetailedReport

getAllPages pageF p = do
  entries <- pageF p
  if length entries < 50 -- TODO: Consult with the `per_page` response field
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

