import Data.Maybe (fromJust, isJust)
import Data.Semigroup ((<>))
import Options.Applicative
import PandocStream (PandocStream(PandocStream))
import Text.Parsec (parse)
import Text.Printf (printf)
import TaskTagsConfig
import TimeTagParser

data TogglCSV = TogglCSV {
    config :: Maybe FilePath,
    email  :: Maybe String,
    input  :: FilePath,
    output :: Maybe FilePath
  } deriving Show

togglCsvArgs =
  TogglCSV
  <$> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG"))
  <*> optional (strOption (long "email" <> short 'e' <> metavar "EMAIL"))
  <*> argument str (metavar "IN")
  <*> optional (argument str (metavar "OUT"))

main = do
  args    <- execParser
             $ info (helper <*> togglCsvArgs)
                    (progDesc "Generate Toggl CSV out of the IN file")
  e       <- if isJust (email args)
                then return $ fromJust $ email args
                else configEmail (config args)
  p       <- parse timeEntries (input args) . PandocStream
             <$> readPandoc (input args)
  case emailValidate e of
    Right _  -> return ()
    Left err -> fail err
  entries <- case p of
    Right e -> return e
    Left  err -> fail $ show err
  let o   =  fromJust $ output args <|> Just "toggl.csv"
  let csv =  toTogglCsv e entries
  writeFile o csv
  putStrLn $ printf "'%s' written" o
