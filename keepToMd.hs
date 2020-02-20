import Data.Semigroup ((<>))
import Data.Time
import Data.Maybe (isJust)
import KeepTimeTagParser
import MarkdownReport
import Options.Applicative
import Text.Parsec (parse)

data KeepToMd = KeepToMd {
  tz     :: Maybe TimeZone,
  input  :: FilePath,
  output :: Maybe FilePath
  }

keepToMdArgs =
  KeepToMd
  <$> optional (option auto (long "tz" <> metavar "TZ"))
  <*> argument str (metavar "IN")
  <*> optional (argument str (metavar "OUT"))

main = do
  args <- execParser
          $ info
            (helper <*> keepToMdArgs)
            (progDesc $ "Convert \"manual\" format convenient for Google Keep "
                        ++ "to Markdown with time tags")
  tz'  <- maybe (zonedTimeZone <$> getZonedTime) return $ tz args
  let out = maybe putStr writeFile $ output args
  let f   = input args
  es   <- either (fail . show) return
          . id . parse (keepTimeEntries tz') f
          <$> readFile f
  out =<< markdownReport <$> es
