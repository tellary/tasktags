module FileTimeEntryParams where

import Data.Time           (UTCTime, ZonedTime)
import Options.Applicative (argument, auto, eitherReader, execParser, helper,
                            info, long, many, metavar, option, optional,
                            progDesc, short, str, strOption, switch)
import Text.Printf         (printf)
import TimeTag             (parseTagTime)

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
