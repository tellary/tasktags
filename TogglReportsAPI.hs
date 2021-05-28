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

class TogglReportsAPI api where
  detailedReport :: api -> DetailedReportReq -> IO DetailedReport

getAllPages api req = do
  entries <- detailedReport api req
  if length entries < 50 -- TODO: Consult with the `per_page` response field
    then return entries
    else do
      tail <- getAllPages
              api
              req { detailedReportPage = detailedReportPage req + 1 }
      return $ entries ++ tail


