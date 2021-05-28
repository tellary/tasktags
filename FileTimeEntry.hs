module FileTimeEntry where

import Data.Maybe          (fromJust, isJust)
import Data.Time           (UTCTime, ZonedTime)
import Options.Applicative (argument, auto, eitherReader, long, many, metavar,
                            option, optional, short, str, strOption, switch)
import PandocStream        (PandocStream (PandocStream))
import TaskTagsConfig      (configEmail, emailValidate)
import Text.Parsec         (parse)
import Text.Printf         (printf)
import TimeTag             (parseTagTime, teStartUTC, toTimeEntries)
import TimeTagParser       (filterOn, maybeSkipReadPandoc, tagsBetween,
                            timeTags)

data FileTimeEntryParams
  = FileTimeEntryParams
  { config                       :: Maybe FilePath,
    email                        :: Maybe String,
    startTimeP                   :: [UTCTime -> Bool],
    firstTag                     :: Maybe ZonedTime,
    lastTag                      :: Maybe ZonedTime,
    startPos                     :: Maybe Int,
    endPos                       :: Maybe Int,
    ignoreIncompleteLastStartTag :: Bool,
    input                        :: FilePath
  }

fileTimeEntryArgs =
  FileTimeEntryParams
  <$> optional (strOption    (long "config" <> short 'c' <> metavar "CONFIG"))
  <*> optional (strOption    (long "email"  <> short 'e' <> metavar "EMAIL" ))
  <*> many     (option timeP (long "startTimeP" <> metavar "START_TIME_P"  ))
  <*> optional (option time  (long "firstTag"   <> metavar "FIRST_TAG_TIME"))
  <*> optional (option time  (long "lastTag"    <> metavar "LAST_TAG_TIME" ))
  <*> optional (option auto  (long "startPos"   <> metavar "START_POS"     ))
  <*> optional (option auto  (long "endPos"     <> metavar "END_POS"       ))
  <*> switch (long "ignoreIncompleteLastStartTag")
  <*> argument str (metavar "IN")

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

readTimeEntries params = do
  e         <- if isJust (email params)
               then return . fromJust . email $ params
               else configEmail (config params)
  let timeP =  (andp . startTimeP $ params)
  p         <- parse timeTags (input params)
               .   PandocStream
               <$> maybeSkipReadPandoc
                   (startPos params) (endPos params)
                   (input params)
  tags      <- case p of
                  Right e   -> return e
                  Left  err -> fail $ show err

  case emailValidate e of
    Right _  -> return ()
    Left err -> fail err

  let i       = ignoreIncompleteLastStartTag params
  either (fail . show) (\tes -> return (tes, e))
    . fmap (filterOn teStartUTC timeP)
    . toTimeEntries timeP i
    . tagsBetween (firstTag params) (lastTag params)
    $ tags
