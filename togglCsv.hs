import Data.Maybe (fromJust, isJust)
import Data.Semigroup ((<>))
import Data.Time (UTCTime)
import Options.Applicative
import PandocStream (PandocStream(PandocStream))
import Text.Parsec (parse)
import Text.Printf (printf)
import TaskTagsConfig
import TimeTagParser

data TogglCSV = TogglCSV {
    config     :: Maybe FilePath,
    email      :: Maybe String,
    startTimeP :: [UTCTime -> Bool],
    startPos   :: Maybe Int,
    endPos     :: Maybe Int,
    ignoreIncompleteLastStartTag :: Bool,
    input      :: FilePath,
    output     :: Maybe FilePath
  }

togglCsvArgs =
  TogglCSV
  <$> optional (strOption    (long "config" <> short 'c' <> metavar "CONFIG"))
  <*> optional (strOption    (long "email" <> short 'e' <> metavar "EMAIL"))
  <*> many     (option timeP (long "startTimeP" <> metavar "START_TIME_P"))
  <*> optional (option auto  (long "startPos" <> metavar "START_POS"))
  <*> optional (option auto  (long "endPos" <> metavar "END_POS"  ))
  <*> switch (long "ignoreIncompleteLastStartTag")
  <*> argument str (metavar "IN")
  <*> optional (argument str (metavar "OUT"))

parseTimeArg s =
  case parseTagTime s of
    Just t  -> Right t
    Nothing -> Left $ printf "Failed to parse time '%s'" s

timeP :: ReadM (UTCTime -> Bool)
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
  args    <- execParser
             $ info (helper <*> togglCsvArgs)
                    (progDesc "Generate Toggl CSV out of the IN file")
  e       <- if isJust (email args)
                then return $ fromJust $ email args
                else configEmail (config args)
  let timeP = (andp $ startTimeP args)
  let i     = ignoreIncompleteLastStartTag args
  p       <- parse (timeEntries timeP i) (input args)
             .   PandocStream
             <$> maybeSkipReadPandoc
                 (startPos args) (endPos args)
                 (input args)
  case emailValidate e of
    Right _  -> return ()
    Left err -> fail err
  entries <- filterOn teStartUTC timeP
    <$> case p of
          Right e   -> return e
          Left  err -> fail $ show err
  let o   =  fromJust $ output args <|> Just "toggl.csv"
  let csv =  toTogglCsv e entries
  if not $ null entries then
    do
      writeFile o csv
      putStrLn $ printf "'%s' written" o
    else
      putStrLn
      $ maybeAppendEndPos   (endPos   args)
      $ maybeAppendStartPos (startPos args)
      $ printf "No time entries found in '%s'" (input args)

maybeAppendStartPos pos str =
  maybe str (\p -> str ++ (printf ", start pos %i" p)) pos

maybeAppendEndPos pos str =
  maybe str (\p -> str ++ (printf ", end pos %i" p)) pos
