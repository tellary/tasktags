import Data.Maybe          (fromJust, isJust)
import Data.Semigroup      ((<>))
import Data.Time           (UTCTime, ZonedTime)
import FileTimeEntryParams (FileTimeEntryParams (config, email, endPos,
                                                 firstTag,
                                                 ignoreIncompleteLastStartTag,
                                                 input, lastTag, startPos,
                                                 startTimeP),
                            fileTimeEntryArgs)
import Options.Applicative (argument, execParser, helper, info, metavar,
                            optional, progDesc, str, (<|>))
import PandocStream        (PandocStream (PandocStream))
import TaskTagsConfig
import Text.Parsec         (parse)
import Text.Printf         (printf)
import TimeTag
import TimeTagParser

data TogglCSV
  = TogglCSV
  { params :: FileTimeEntryParams
  , output :: Maybe FilePath
  }

togglCsvArgs =
  TogglCSV
  <$> fileTimeEntryArgs
  <*> optional (argument str (metavar "OUT"))

andp ps = \v -> and $ map ($v) ps

main = do
  args      <- execParser
               $ info (helper <*> togglCsvArgs)
                      (progDesc "Generate Toggl CSV out of the IN file")
  let argParams = params args
  e         <- if isJust (email argParams)
                then return . fromJust . email $ argParams
                else configEmail (config argParams)
  let timeP =  (andp . startTimeP $ argParams)
  p         <- parse timeTags (input argParams)
               .   PandocStream
               <$> maybeSkipReadPandoc
                   (startPos argParams) (endPos argParams)
                   (input argParams)
  tags      <- case p of
                  Right e   -> return e
                  Left  err -> fail $ show err

  case emailValidate e of
    Right _  -> return ()
    Left err -> fail err

  let i       = ignoreIncompleteLastStartTag argParams
  entries <-
    either (fail . show) return
    . fmap (filterOn teStartUTC timeP)
    . toTimeEntries timeP i
    . tagsBetween (firstTag argParams) (lastTag argParams)
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
      $ printf "No time entries found in '%s'" (input argParams)

maybeAppendStartPos args str =
  maybe str (\p -> str ++ (printf ", start pos %i" p))
  (startPos . params $ args)

maybeAppendEndPos args str =
  maybe str (\p -> str ++ (printf ", end pos %i" p))
  (endPos . params $ args)

maybeAppendFirstTag args str =
  maybe str (\p -> str ++ (printf ", first tag '%s'" $ formatTagTime p))
  (firstTag . params $ args)

maybeAppendLastTag args str =
  maybe str (\p -> str ++ (printf ", last tag '%s'" $ formatTagTime p))
  (lastTag . params $ args)
