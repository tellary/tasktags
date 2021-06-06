import Data.Maybe          (fromJust)
import FileTimeEntry       (FileTimeEntryParams (endPos, firstTag, input,
                                                 lastTag, startPos),
                            fileTimeEntryArgs, readTimeEntries)
import Options.Applicative (argument, execParser, helper, info, metavar,
                            optional, progDesc, str, (<|>))
import Text.Printf         (printf)
import TimeTag             (formatTagTime)
import TimeTagParser       (toTogglCsv)

data TogglCSV
  = TogglCSV
  { params :: FileTimeEntryParams
  , output :: Maybe FilePath
  }

togglCsvArgs =
  TogglCSV
  <$> fileTimeEntryArgs
  <*> optional (argument str (metavar "OUT"))

main = do
  args <- execParser
          $ info (helper <*> togglCsvArgs)
                 (progDesc "Generate Toggl CSV out of the IN file")

  (entries, e) <- readTimeEntries . params $ args
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
      $ printf "No time entries found in '%s'" (input . params $ args)

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
