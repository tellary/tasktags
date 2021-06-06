{-# LANGUAGE FlexibleContexts #-}

module KeepTimeTagParser where

import Data.Time hiding (parseTime)
import Text.Parsec
import TimeTag

keepNoProject = "(Type correct project names)"

parseDay :: (ParseTime t, MonadFail m) => String -> m t
parseDay = parseTimeM False defaultTimeLocale "%Y%m%d"

parseTime :: (ParseTime t, MonadFail m) => String -> m t
parseTime = parseTimeM False defaultTimeLocale "%H%M"

keepTimeEntries :: Stream s m Char => TimeZone -> ParsecT s u m [TimeEntry]
keepTimeEntries z = do
  date <- keepHeader
  spaces
  tags <- concat <$> many (keepTaskTags z date)
  eof
  case toTimeEntries (const True) False tags of
    Right ts -> return ts
    Left  err  -> fail $ show err

keepHeader :: Stream s m Char => ParsecT s u m Day
keepHeader = keepDay <* manyTill anyChar newline
keepDay :: Stream s m Char => ParsecT s u m Day
keepDay = parseDay =<< count 8 digit
keepTaskTags :: Stream s m Char => TimeZone -> Day -> ParsecT s u m [TimeTag]
keepTaskTags z d = do
  ts <- many ((,) <$> keepStart <*> keepStop)
  c  <- anyChar
  cs <- manyTill anyChar (() <$ newline <* spaces <|> eof)
  let task = c:cs
  return $ taskTimeTags z d keepNoProject task ts

keepStart, keepStop :: Stream s m Char => ParsecT s u m TimeOfDay
keepStart = parseTime =<< count 4 digit <* spaces
keepStop  = keepStart

taskTimeTags z d p t ts = concat . map toTags $ ts
  where toTags (start, stop) =
          [StartTimeTag p t $ toZonedTime start,
           StopTimeTag  p t $ toZonedTime stop]
        toZonedTime t = ZonedTime (LocalTime d t) z
