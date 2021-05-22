{-# LANGUAGE FlexibleContexts #-}

module TimeTagParser where

import           Control.Applicative   ((<|>))
import           Data.List             (intercalate, isSuffixOf)
import           Data.List.Split       (splitOn)
import           Data.Maybe            (catMaybes)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           PandocParser
import           PandocStream
import           Text.Pandoc           (Block (Header),
                                        Extension (Ext_backtick_code_blocks),
                                        Inline (Space, Str), def,
                                        extensionsFromList, readMarkdown,
                                        readerExtensions, runPure)
import           TimeTag
import           Text.Parsec           hiding ((<|>))
import           Text.Printf           (printf)

{-|

> nonHeader n        = < Any pandoc element except header of level n >
> nonTimeTagElement  = < Any pandoc element that's not a string 
>                        terminated by "<task-start" or "<task-stop" >
> dayElement         = < Any pandoc element that
>                        is nonTimeTagElement and is nonHeader 1 >
> projectElement     = < Any pandoc element that
>                        is dayElement and is nonHeader 2 >
> taskElement        = < Any pandoc element that
>                        is projectElement and is nonHeader 3 >
>
> timeTags        = *dayTimeTags *nonTimeTagElement eof
> dayTimeTags     = *dayElement     header1 *projectTimeTags
> projectTimeTags = *projectElement header2 *taskTimeTags
> taskTimeTags    = *taskElement    header3 *( timeTag | taskElement )

|-}

msgAnd :: String -> String -> String
m1 `msgAnd` m2 = printf "%s && %s" m1 m2

msgIsNonHeaderL l = "not (Header " ++ show l ++ " _ _)"

isTagElement tagName e =
  maybe False (("<" ++ tagName) `isSuffixOf`) $ toStr e

isEmailTagElement      = isTagElement "task-config-email"
isTimeStartTagElement  = isTagElement "task-start"
isTimeStopTagElement   = isTagElement "task-stop"
isTimeTagElement     e =
  isTimeStartTagElement e || isTimeStopTagElement e
isNonTimeTagElement    = not . isTimeTagElement
msgIsNonTimeTagElement =
  "(not (Str \".*<task-start\" || Str \".*<task-stop\"))"
isDayElement         e = isNonTimeTagElement e   &&      not (isHeaderL 1 e)
msgIsDayElement        = msgIsNonTimeTagElement `msgAnd` msgIsNonHeaderL 1
isProjectElement     e = isDayElement e          &&      not (isHeaderL 2 e)
msgIsProjectElement    = msgIsDayElement        `msgAnd` msgIsNonHeaderL 2
isTaskElement        e = isProjectElement e      &&      not (isHeaderL 3 e)
msgIsTaskElement       = msgIsProjectElement    `msgAnd` msgIsNonHeaderL 3

nonTimeTagElement, dayElement, projectElement, taskElement
  :: Stream s m PandocElement => ParsecT s u m PandocElement
nonTimeTagElement =
  satisfyElement isNonTimeTagElement <?> msgIsNonTimeTagElement
dayElement        =
  satisfyElement isDayElement        <?> msgIsDayElement
projectElement    =
  satisfyElement isProjectElement    <?> msgIsProjectElement
taskElement       =
  satisfyElement isTaskElement       <?> msgIsTaskElement

timeTags :: Stream s m PandocElement => ParsecT s u m [TimeTag]
timeTags = do
  tags <- concat <$> many dayTimeTags
  many nonTimeTagElement
  eof
  return tags

dayTimeTags, projectTimeTags
  :: Stream s m PandocElement => ParsecT s u m [TimeTag]

dayTimeTags = do
  try (many dayElement *> headerL 1)
  concat <$> many projectTimeTags

projectTimeTags = do
  Header _ _ is2 <- try (many projectElement *> headerL 2)
  let project = writeInlines is2
  concat <$> many (taskTimeTags project)

taskTimeTags :: Stream s m PandocElement
  => Project -> ParsecT s u m [TimeTag]
taskTimeTags project = do
  Header _ _ is3 <- try (many taskElement *> headerL 3)
  let task = writeInlines is3
  catMaybes
    <$> many (Just <$> timeTag project task <|> Nothing <$ taskElement)

timeTag :: Stream s m PandocElement =>
  String -> String -> ParsecT s u m TimeTag
timeTag project task = do
  start <-
        True  <$ satisfyElement isTimeStartTagElement
    <|> False <$ satisfyElement isTimeStopTagElement
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
                   fail $ printf ("Malformed <task-start/> or <task-stop/>: @ "
                                  ++ "%s %s")
                                 date time
  let timestampStr = intercalate " " [date, time, tz]
  timestamp <-
        case parseTagTime timestampStr of
          Just t  -> return t
          Nothing -> fail $ "Wrong timestamp string: " ++ timestampStr
  if start
    then return $ StartTimeTag project task timestamp
    else return $ StopTimeTag  project task timestamp

tagsBetween firstTag lastTag =
    maybeTakeBefore lastTag
  . maybeDropBefore firstTag
maybeDropBefore firstTag = case firstTag of
  Just f  -> dropWhile ((/= fUtc) . ttTimeUTC) where fUtc = zonedTimeToUTC f
  Nothing -> id
maybeTakeBefore lastTag tags = case lastTag of
  Just l  ->
    range t
    where lUtc = zonedTimeToUTC l
          (before, t) = span ((/= lUtc) . ttTimeUTC) tags
          range []    = before
          range (h:_) = before ++ [h]
  Nothing -> tags

timeEntries :: Stream s m PandocElement
  => (UTCTime -> Bool)
  -> Bool -- ignoreIncompleteLastStartTag
  -> ParsecT s u m [TimeEntry]
timeEntries p i = do
  ts <- timeTags
  case toTimeEntries p i ts of
    Right es -> return es
    Left  err  -> fail $ show err

filterOn :: (a -> b) -> (b -> Bool) -> [a] -> [a]
filterOn on p entries =
  map snd . filter (p . fst) $ pairs
  where pairs = map (\e -> (on e, e)) entries

togglCsvHeader =
  "User,Email,Client,Project,Task,Description,"
  ++ "Billable,Start date,Start time,End date,End time,"
  ++ "Duration,Tags,Amount ()"

toTogglCsv :: String -> [TimeEntry] -> String
toTogglCsv email = (++ "\n") . ((togglCsvHeader ++ "\n") ++)
  . intercalate "\n" . map (toTogglCsvLine email)

quoteStr s = "\"" ++ (intercalate "\\\"" . splitOn "\"" $ s) ++ "\""

formatTogglDate :: FormatTime t => t -> String
formatTogglDate = formatTime defaultTimeLocale "%Y-%m-%d"
formatTogglTime :: FormatTime t => t -> String
formatTogglTime = formatTime defaultTimeLocale "%T"

diffTime t2 t1 = posixSecondsToUTCTime
  $ diffUTCTime (zonedTimeToUTC t2) (zonedTimeToUTC t1)

toTogglCsvLine :: String -> TimeEntry -> String
toTogglCsvLine email te = intercalate "," [
  email, email, "",
  quoteStr $ teProject te, "", quoteStr $ teTask te, "No",
  formatTogglDate $ teStart te, formatTogglTime $ teStart te,
  formatTogglDate $ teStop  te, formatTogglTime $ teStop  te,
  formatTogglTime $ diffTime (teStop te) (teStart te),
  "", ""
  ]

readPandoc = skipReadPandoc 0 Nothing
skipReadPandoc start len f = do
  e <- runPure
       . readMarkdown
         (def {
             readerExtensions = extensionsFromList
               [Ext_backtick_code_blocks]
             })
       . maybe id (\l -> T.take l) len
       . T.drop start <$> TIO.readFile f
  case e of
    Right p  -> return p
    Left err -> fail $ show err

maybeSkipReadPandoc startPos endPos f =
  skipReadPandoc start len f
  where start = maybe 0 id startPos
        len   = (\end -> end - start) <$> endPos
