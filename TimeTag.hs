module TimeTag where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Text.Printf (printf)

type Project   = String
type Task      = String
type Timestamp = String
data TimeTag   =
    StartTimeTag Project Task ZonedTime
  | StopTimeTag  Project Task ZonedTime
  deriving Show

ttTime (StartTimeTag _ _ zt) = zt
ttTime (StopTimeTag  _ _ zt) = zt

ttTimeUTC = zonedTimeToUTC . ttTime

ttIsStart (StartTimeTag _ _ _) = True
ttIsStart (StopTimeTag  _ _ _) = False

ttCompare tt1 tt2 =
  case comparing ttTimeUTC tt1 tt2 of
    LT -> LT
    -- Stop tag goes first (is less)
    EQ -> comparing ttIsStart tt1 tt2
    GT -> GT

instance Eq TimeTag where
  (StartTimeTag p1 t1 zt1) == (StartTimeTag p2 t2 zt2) =
    p1 == p2 && t1 == t2 && zonedTimeToUTC zt1 == zonedTimeToUTC zt2
  (StopTimeTag p1 t1 zt1) == (StopTimeTag p2 t2 zt2) =
    p1 == p2 && t1 == t2 && zonedTimeToUTC zt1 == zonedTimeToUTC zt2
  _ == _ = False

data TimeEntry = TimeEntry {
  teProject :: Project  ,
  teTask    :: Task     ,
  teStart   :: ZonedTime,
  teStop    :: ZonedTime
  } deriving Show

teStartUTC = zonedTimeToUTC . teStart

instance Eq TimeEntry where
  (TimeEntry p1 t1 start1 stop1) == (TimeEntry p2 t2 start2 stop2) =
       p1 == p2 && t1 == t2
    && (zonedTimeToUTC start1) == (zonedTimeToUTC start2)
    && (zonedTimeToUTC stop1 ) == (zonedTimeToUTC stop2 )

tagTimeFormat = "%Y%m%d %T %z"
parseTagTime  :: (ParseTime t, Monad m) => String -> m t
parseTagTime  = parseTimeM False defaultTimeLocale tagTimeFormat
formatTagTime :: FormatTime t => t -> String
formatTagTime = formatTime defaultTimeLocale tagTimeFormat

toTimeEntries
  :: (UTCTime -> Bool)
  -> Bool -- ignoreIncompleteLastStartTag
  -> [TimeTag]
  -> Either TimeEntryError [TimeEntry]
toTimeEntries p i = fmap fst
                . foldr step (Right ([], Nothing))
                . sortBy ttCompare
  where
    p' = p . ttTimeUTC
    step
      tt@(StartTimeTag _ _ _) r@(Right (_, Nothing))
      | not i && p' tt     = Left $ incompleteLastStartTag tt
      -- This way we don't generate error for a currently
      -- running task
      | otherwise = r
    step
      tt@(StopTimeTag _ _ _) (Right (result, Nothing))
      = Right (result, Just tt)
    step
      tt@(StartTimeTag p t tz)
         (Right (result, Just tt1@(StopTimeTag p1 t1 tz1)))
      | p == p1 && t == t1 = Right (TimeEntry p t tz tz1:result, Just tt)
      | otherwise = Left $ unexpectedStopTag tt tt1
    step tt@(StopTimeTag _ _ _) (Right (result, Just (StartTimeTag _ _ _)))
      = Right (result, Just tt)
    step
      tt@(StartTimeTag _ _ _)
         (Right (_, Just tt1@(StartTimeTag _ _ _)))
      = Left $ duplicateStartTag tt tt1
    step
      tt@(StopTimeTag _ _ _)
         (Right (_, Just tt1@(StopTimeTag _ _ _)))
      = Left $ duplicateStopTag tt tt1
    step _ l@(Left _) = l

data TimeEntryError =
    IncompleteLastStartTag            String
  | UnexpectedSameProjectStopTag      String
  | UnexpectedDifferentProjectStopTag String
  | SameTaskDuplicateStartTag         String
  | SameProjectDuplicateStartTag      String
  | DifferentProjectDuplicateStartTag String
  | SameTaskDuplicateStopTag          String
  | SameProjectDuplicateStopTag       String
  | DifferentProjectDuplicateStopTag  String
  deriving Show

incompleteLastStartTag (StartTimeTag p t tz) =
  IncompleteLastStartTag
  $ printf ("Incomplete last start tag for task '%s' "
             ++ "in project '%s' started at '%s'")
           t p (formatTagTime tz)
incompleteLastStartTag _ =
  error "incompleteLastStartTag: Only start tag expected"

unexpectedStopTag (StartTimeTag p t tz) (StopTimeTag p1 t1 tz1)
  | p == p1 && t == t1 =
      error "unexpectedStopTag: different task and/or project expected"
  | p == p1 =
      UnexpectedSameProjectStopTag
      $ printf (   "Task '%s' of a project '%s' "
                ++ "started at '%s' still runs while "
                ++ "task '%s' of the same project "
                ++ "is stopped at '%s'")
               t p strTz t1 strTz1
  | otherwise =
      UnexpectedDifferentProjectStopTag
      $ printf (   "Task '%s' of a project '%s' "
                ++ "started at '%s' still runs while "
                ++ "task '%s' of a different project '%s' "
                ++ "is stopped at '%s'")
               t p strTz t1 p1 strTz1
  where strTz  = formatTagTime tz
        strTz1 = formatTagTime tz1
unexpectedStopTag _ _ =
  error "unexpectedStopTag: only start and stop tag expected"

duplicateStartTag (StartTimeTag p t tz) (StartTimeTag p1 t1 tz1)
      | t == t1 && p == p1 =
        SameTaskDuplicateStartTag
        $ printf (   "Same task '%s' of a project '%s' "
                  ++ "is started twice at '%s' and '%s'")
                 t p strTz strTz1
      | p == p1 =
        SameProjectDuplicateStartTag
        $ printf (   "Task '%s' of a project '%s' "
                  ++ "started at '%s' still runs while "
                  ++ "task '%s' of the same project "
                  ++ "is also started at '%s'")
                 t p strTz t1 strTz1
      | otherwise =
        DifferentProjectDuplicateStartTag
        $ printf (   "Task '%s' of a project '%s' "
                  ++ "started at '%s' still runs while "
                  ++ "task '%s' of a different project '%s' "
                  ++ "is also started at '%s'")
                 t p strTz t1 p1 strTz1
      where strTz  = formatTagTime tz
            strTz1 = formatTagTime tz1
duplicateStartTag _ _ =
  error "duplicateStartTag: only two start tags expected"

duplicateStopTag (StopTimeTag p t tz) (StopTimeTag p1 t1 tz1)
  | t == t1 && p == p1 =
      SameTaskDuplicateStopTag
      $ printf (   "Same task '%s' of a project '%s' "
                ++ "is stopped twice at '%s' and '%s'")
               t p strTz strTz1
  | p == p1 =
      SameProjectDuplicateStopTag
      $ printf (   "Task '%s' of a project '%s' "
                ++ "is stopped at '%s' while "
                ++ "task '%s' of the same project still runs "
                ++ "and will be stopped at '%s'")
               t p strTz t1 strTz1
  | otherwise =
      DifferentProjectDuplicateStopTag
      $ printf (   "Task '%s' of a project '%s' "
                ++ "is stopped at '%s' while "
                ++ "task '%s' of a different project '%s' still runs "
                ++ "and will be stopped at '%s'")
               t p strTz t1 p1 strTz1
      where strTz  = formatTagTime tz
            strTz1 = formatTagTime tz1
duplicateStopTag _ _ =
  error "duplicateStopTag: only two stop tags expected"
