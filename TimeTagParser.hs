{-# LANGUAGE FlexibleContexts #-}

module TimeTagParser where

import Data.List (intercalate, isSuffixOf, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Time
import PandocParser
import PandocStream
import Text.Pandoc
import Text.Parsec
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

instance Eq TimeEntry where
  (TimeEntry p1 t1 start1 stop1) == (TimeEntry p2 t2 start2 stop2) =
       p1 == p2 && t1 == t2
    && (zonedTimeToUTC start1) == (zonedTimeToUTC start2)
    && (zonedTimeToUTC stop1 ) == (zonedTimeToUTC stop2 )

tagTimeFormat = "%Y%m%d %T %z"
parseTagTime  = parseTimeM False defaultTimeLocale tagTimeFormat
formatTagTime :: FormatTime t => t -> String
formatTagTime = formatTime defaultTimeLocale tagTimeFormat

isTagElement     e =
     isHeaderL 1 e || isHeaderL 2 e || isHeaderL 3 e
  || maybe False ("<task-start" `isSuffixOf`) s
  || maybe False ("<task-stop"  `isSuffixOf`) s
  where s = toStr e
isNonTagElement  e = not (isTagElement e)
isDayElement     e = not (isHeaderL 1 e)  && isNonTagElement e
isProjectElement e = not (isHeaderL 2 e)  && isDayElement e
isTaskElement    e = not (isHeaderL 3 e)  && isProjectElement e

nonTagElement, dayElement, projectElement, taskElement
  :: Stream s m PandocElement => ParsecT s u m PandocElement
nonTagElement  = satisfyElement isNonTagElement <?> "non-tag element"
dayElement     = satisfyElement isDayElement <?> "day element"
projectElement = satisfyElement isProjectElement <?> "project element"
taskElement    = satisfyElement isTaskElement <?> "task element"

timeTags :: Stream s m PandocElement => ParsecT s u m [TimeTag]
timeTags = concat <$> many dayTimeTags

dayTimeTags, projectTimeTags
  :: Stream s m PandocElement => ParsecT s u m [TimeTag]

dayTimeTags = do
  many nonTagElement
  headerL 1
  concat <$> many (try $ projectTimeTags)

projectTimeTags = do
  many dayElement
  Header _ _ is2 <- headerL 2
  let project = writeInlines is2
  concat <$> many (try $ taskTimeTags project)

taskTimeTags :: Stream s m PandocElement
  => Project -> ParsecT s u m [TimeTag]
taskTimeTags project = do
  many projectElement
  Header _ _ is3 <- headerL 3
  let task = writeInlines is3
  catMaybes
    <$> many (Just <$> try (timeTag project task) <|> Nothing <$ taskElement)

timeTag :: Stream s m PandocElement =>
  String -> String -> ParsecT s u m TimeTag
timeTag project task = do
  start <-
        True  <$ satisfyElement
                 (maybe False ("<task-start" `isSuffixOf`) . toStr)
    <|> False <$ satisfyElement
                 (maybe False ("<task-stop" `isSuffixOf`)  . toStr)
  inline Space
  Str date' <- anyInline
  date      <- case splitAt 3 date' of
                ("t=\"", d) -> return d
                _           ->
                  fail "Malformed <task-start/> or <task-stop/>"
  inline Space
  Str time  <- anyInline
  inline Space
  Str tz'   <- anyInline
  tz        <- case splitAt 5 tz' of
                 (t, "\"/>") -> return t
                 _           ->
                   fail "Malformed <task-start/> or <task-stop/>"
  let timestampStr = intercalate " " [date, time, tz]
  timestamp <-
        case parseTagTime timestampStr of
          Just t  -> return t
          Nothing -> fail $ "Wrong timestamp string: " ++ timestampStr
  if start
    then return $ StartTimeTag project task timestamp
    else return $ StopTimeTag  project task timestamp

timeEntries :: Stream s m PandocElement => ParsecT s u m [TimeEntry]
timeEntries = do
  ts <- timeTags
  case toTimeEntries ts of
    Right es -> return es
    Left  err  -> fail $ show err

toTimeEntries :: [TimeTag] -> Either TimeEntryError [TimeEntry]
toTimeEntries = fmap fst
                . foldr step (Right ([], Nothing))
                . sortBy (comparing ttTimeUTC)
  where
    step
      tt@(StartTimeTag _ _ _) (Right (_, Nothing))
      = Left $ incompleteLastStartTag tt
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
                ++ "task '%s' of a different project '%s still runs "
                ++ "and will be stopped at '%s'")
               t p strTz t1 p1 strTz1
      where strTz  = formatTagTime tz
            strTz1 = formatTagTime tz1
duplicateStopTag _ _ =
  error "duplicateStopTag: only two stop tags expected"
