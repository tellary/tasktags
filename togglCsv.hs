import Data.Maybe          (fromJust, isJust)
import Data.Semigroup      ((<>))
import Data.Time           (UTCTime, ZonedTime)
import Options.Applicative (argument, auto, eitherReader, execParser, helper,
                            info, long, many, metavar, option, optional,
                            progDesc, short, str, strOption, switch, (<|>))
import PandocStream        (PandocStream (PandocStream))
import TaskTagsConfig
import Text.Parsec         (parse)
import Text.Printf         (printf)
import TimeTag
import TimeTagParser

data TogglCSV = TogglCSV {
    config                       :: Maybe FilePath,
    email                        :: Maybe String,
    startTimeP                   :: [UTCTime -> Bool],
    firstTag                     :: Maybe ZonedTime,
    lastTag                      :: Maybe ZonedTime,
    startPos                     :: Maybe Int,
    endPos                       :: Maybe Int,
    ignoreIncompleteLastStartTag :: Bool,
    input                        :: FilePath,
    output                       :: Maybe FilePath
  }

togglCsvArgs =
  TogglCSV
  <$> optional (strOption    (long "config" <> short 'c' <> metavar "CONFIG"))
  <*> optional (strOption    (long "email"  <> short 'e' <> metavar "EMAIL" ))
  <*> many     (option timeP (long "startTimeP" <> metavar "START_TIME_P"  ))
  <*> optional (option time  (long "firstTag"   <> metavar "FIRST_TAG_TIME"))
  <*> optional (option time  (long "lastTag"    <> metavar "LAST_TAG_TIME" ))
  <*> optional (option auto  (long "startPos"   <> metavar "START_POS"     ))
  <*> optional (option auto  (long "endPos"     <> metavar "END_POS"       ))
  <*> switch (long "ignoreIncompleteLastStartTag")
  <*> argument str (metavar "IN")
  <*> optional (argument str (metavar "OUT"))

parseTimeArg s =
  case parseTagTime s of
    Just t  -> Right t
    Nothing -> Left $ printf "Failed to parse time '%s'" s

time = eitherReader parseTagTime

timeP = eitherReader $ \s ->
  case s of
    '<':'=':' ':t -> (\t' -> (<= t')) <$> parseTimeArg t
    '<':'=':t     -> (\t' -> (<= t')) <$> parseTimeArg t
    '>':'=':' ':t -> (\t' -> (>= t')) <$> parseTimeArg t
    '>':'=':t     -> (\t' -> (>= t')) <$> parseTimeArg t
    '<':' ':t     -> (\t' -> (< t'))  <$> parseTimeArg t
    '<':t         -> (\t' -> (< t'))  <$> parseTimeArg t
    '>':' ':t     -> (\t' -> (> t'))  <$> parseTimeArg t
    '>':t         -> (\t' -> (> t'))  <$> parseTimeArg t
    _             -> Left $ printf "Failed to parse time predicate '%s'" s

andp ps = \v -> and $ map ($v) ps

main = do
  args      <- execParser
               $ info (helper <*> togglCsvArgs)
                      (progDesc "Generate Toggl CSV out of the IN file")
  e         <- if isJust (email args)
                then return $ fromJust $ email args
                else configEmail (config args)
  let timeP =  (andp $ startTimeP args)
  p         <- parse timeTags (input args)
               .   PandocStream
               <$> maybeSkipReadPandoc
                   (startPos args) (endPos args)
                   (input args)
  tags      <- case p of
                  Right e   -> return e
                  Left  err -> fail $ show err

  case emailValidate e of
    Right _  -> return ()
    Left err -> fail err

  let i       = ignoreIncompleteLastStartTag args
  entries <-
    either (fail . show) return
    . fmap (filterOn teStartUTC timeP)
    . toTimeEntries timeP i
    . tagsBetween (firstTag args) (lastTag  args)
    $ tags

  if not $ null entries then
    do
      let o   = fromJust $ output args <|> Just "toggl.csv"
      let csv = toTogglCsv e entries
      writeFile o csv
      putStrLn $ printf "'%s' written" o
    else
      putStrLn
      $ maybeAppendLastTag  args
      $ maybeAppendFirstTag args
      $ maybeAppendEndPos   args
      $ maybeAppendStartPos args
      $ printf "No time entries found in '%s'" (input args)

maybeAppendStartPos args str =
  maybe str (\p -> str ++ (printf ", start pos %i" p))
  (startPos args)

maybeAppendEndPos args str =
  maybe str (\p -> str ++ (printf ", end pos %i" p))
  (endPos args)

maybeAppendFirstTag args str =
  maybe str (\p -> str ++ (printf ", first tag '%s'" $ formatTagTime p))
  (firstTag args)

maybeAppendLastTag args str =
  maybe str (\p -> str ++ (printf ", last tag '%s'" $ formatTagTime p))
  (lastTag args)
