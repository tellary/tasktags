{-# LANGUAGE FlexibleContexts #-}

module InlineParser where

import Data.List (intercalate)
import Data.Maybe
import Text.Pandoc
import Text.Parsec

satisfyInline :: Stream s m Inline => (Inline -> Bool) -> ParsecT s u m Inline
satisfyInline p =
  tokenPrim show nextPos testInline
  where testInline t    = if p t then Just t else Nothing
        nextPos pos _ _ = incSourceColumn pos 1
        
anyInline :: Stream s m Inline => ParsecT s u m Inline
anyInline = satisfyInline (const True)

inline i = satisfyInline (== i) <?> show i

startStopTag :: Stream s m Inline => ParsecT s u m (Bool, String)
startStopTag = do
  start     <-     (True  <$ inline (Str "<task-start"))
               <|> (False <$ inline (Str "<task-stop" ))
  inline Space
  Str date' <- anyInline
  date      <- case splitAt 3 date' of
                ("t=\"", d) -> return d
                _           ->
                  fail "Malformatted <task-start/> or <task-stop/>"
  inline Space
  Str time  <- anyInline
  inline Space
  Str tz'   <- anyInline
  tz        <- case splitAt 5 tz' of
                 (t, "\"/>") -> return t
                 _           ->
                   fail "Malformatted <task-start/> or <task-stop/>"
  return (start, intercalate " " [date, time, tz])

startStopTags :: Stream s m Inline => ParsecT s u m [(Bool, String)]
startStopTags = 
  catMaybes
  <$> many (try (Just <$> startStopTag) <|> try (Nothing <$ anyInline))
