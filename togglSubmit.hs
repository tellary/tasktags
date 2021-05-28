import           FileTimeEntry       (FileTimeEntryParams (input),
                                      fileTimeEntryArgs, readTimeEntries)
import           Options.Applicative (execParser, helper, info, progDesc)
import           System.Exit         (exitFailure)

import           Control.Monad       (forM_, when)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Time           (LocalTime (localDay),
                                      ZonedTime (zonedTimeToLocalTime))
import           TaskTagsConfig
import           TimeTag             (TimeEntry (teStart, teStop))
import           TimeTag
import qualified TogglAPI            as API
import           TogglReportsAPI     (DateRange (DateRange),
                                      DetailedReportReq (DetailedReportReq),
                                      ReportReq (ReportReq, dateRange,
                                                 workspaceId),
                                      TimeEntry (start), getAllPages)

main = undefined

togglSubmit config reportsApi api = do
  params <- execParser
          $ info (helper <*> fileTimeEntryArgs)
                 (progDesc "Submit time entries to Toggl CSV")
  wid <- case fileTogglWorkspace config (input params) of
           Just wid -> return wid
           Nothing  -> do
             putStrLn ("No workspace configured for file " ++ (input params))
             exitFailure

  (entries, _) <- readTimeEntries $ params
  when (null entries) $ do
    putStrLn "No entries found"
    exitFailure

  let startDay = localDay . zonedTimeToLocalTime . teStart . head $ entries
  let stopDay  = localDay . zonedTimeToLocalTime . teStop  . last $ entries
  let range    = DateRange startDay stopDay
  let req      = ReportReq
                 { workspaceId = wid
                 , dateRange   = range
                 }
  existingEntries
    <-  S.fromList . map start
    <$> getAllPages reportsApi (DetailedReportReq req 1)

  let newEntries
        = filter (flip S.notMember existingEntries
                  . zonedTimeToLocalTime . teStart)
          entries
  projects 
    <-  M.fromList . map (\p -> (API.name p, API.id p))
    <$> API.listWorkspaceProjects api wid
        
  let submit e = do
        pid <- case M.lookup (teProject e) projects of
                 Just pid -> return pid
                 Nothing  -> API.createProject api wid (teProject e)
        let timeEntry
              = API.TimeEntry
              { API.description = teTask e
              , API.pid         = pid
              , API.start       = teStart e
              , API.stop        = teStop e
              }
        API.createTimeEntry api timeEntry
  forM_ newEntries submit
