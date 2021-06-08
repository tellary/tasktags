{-# LANGUAGE LambdaCase #-}

import           FileTimeEntry       (FileTimeEntryParams (config, input),
                                      fileTimeEntryArgs, readTimeEntries)
import           Options.Applicative (execParser, helper, info, progDesc)
import           System.Exit         (exitFailure, exitSuccess)
import           Text.Printf         (printf)

import           Control.Monad       (foldM_, when)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Time           (LocalTime (localDay),
                                      ZonedTime (zonedTimeToLocalTime),
                                      zonedTimeToUTC)
import           TaskTagsConfig      (fileTogglWorkspace, iniFileConfig,
                                      togglApiKey)
import           TimeTag             (TimeEntry (teProject, teStart, teStop,
                                                 teTask))
import qualified TogglAPI            as API
import           TogglReportsAPI     (DateRange (DateRange),
                                      DetailedReport (perPage, timeEntries),
                                      DetailedReportReq (DetailedReportReq),
                                      ReportReq (ReportReq, dateRange,
                                                 workspaceId),
                                      TimeEntry (start), detailedReport,
                                      getAllPages, togglReportsAPI)

main = togglSubmit iniFileConfig togglReportsAPI API.togglAPI

togglSubmit configF reportsApi api = do
  params <- execParser
          $ info (helper <*> fileTimeEntryArgs)
                 (progDesc "Submit time entries to Toggl")

  config <- configF . config $ params
  wid <- fileTogglWorkspace config (input params) >>= \case
           Right wid -> return wid
           Left  err -> do
             putStrLn err
             exitFailure

  key <- case togglApiKey config of
           Right key -> return key
           Left  err  -> do
             putStrLn err
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
    <-  S.fromList . map (zonedTimeToUTC . start)
    <$> getAllPages
        (\p -> do
            r <- detailedReport
                 reportsApi key (DetailedReportReq req p)
            return (timeEntries r, perPage r)
        )
        1
  printf "Found %i time entries in Toggl in %s\n"
    (S.size existingEntries) (show range)
  let newEntries
        = filter (flip S.notMember existingEntries . zonedTimeToUTC . teStart)
          entries
  when (null newEntries) $ do
    printf "Found no new time entries in %s\n" (show range)
    exitSuccess
  printf "Found %i new time entries\n" (length newEntries)

  projects 
    <-  M.fromList . map (\p -> (API.name p, API.id p))
    <$> API.listWorkspaceProjects api key wid

  let submit projects e = do
        (pid, projects') <- case M.lookup (show . teProject $ e) projects of
                 Just pid -> return (pid, projects)
                 Nothing  -> do
                   printf "Creating project: '%s'\n" (show . teProject $ e)
                   pid <- API.createProject api key wid (show . teProject $ e)
                   return (pid, M.insert (show . teProject $ e) pid projects)
        let timeEntry
              = API.TimeEntry
              { API.description = show . teTask $ e
              , API.pid         = pid
              , API.start       = teStart e
              , API.stop        = teStop e
              }
        printf "Creating time entry %s:%s @ %s\n"
          (show . teProject $ e)
          (show . teTask    $ e)
          (show . teStart   $ e)
        API.createTimeEntry api key timeEntry
        return projects'
  foldM_ submit projects newEntries
