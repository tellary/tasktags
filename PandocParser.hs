{-# LANGUAGE FlexibleContexts #-}

module PandocParser where

import           Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Text as T
import           PandocStream
import           Text.Pandoc
import           Text.Parsec

showElement (BlockElement  b) = "(" ++ show b ++ ")"
showElement (InlineElement i) = "(" ++ show i ++ ")"

satisfyElement :: Stream s m PandocElement =>
  (PandocElement -> Bool) -> ParsecT s u m PandocElement
satisfyElement p =
  tokenPrim showElement nextPos test
  where test t                          = if p t then Just t else Nothing
        nextPos pos (BlockElement  _) _ =
          setSourceColumn (incSourceLine pos 1) 1
        nextPos pos (InlineElement _) _ =
          incSourceColumn pos 1

anyElement :: Stream s m PandocElement => ParsecT s u m PandocElement
anyElement = satisfyElement (const True)

element e = satisfyElement (== e) <?> show e

isHeaderL l (BlockElement (Header l' _ _)) = l == l'
isHeaderL _ _                              = False

isHeaderS s (BlockElement (Header _ _ is)) = s == writeInlines is
isHeaderS _ _                              = False

isHeader = maybe False (const True) . (blockToHeader =<<) . toBlock

headerL :: Stream s m PandocElement => Int -> ParsecT s u m Block
headerL l = toBlock
  =<< satisfyElement (isHeaderL l) <?> "(Header " ++ show l ++ " _ _)"

headerS :: Stream s m PandocElement => String -> ParsecT s u m Block
headerS s = toBlock
  =<< satisfyElement (isHeaderS s) <?> "(Header _ _ \"" ++ s ++ "\")"

anyHeader :: Stream s m PandocElement => ParsecT s u m Block
anyHeader = blockToHeader =<< toBlock =<< satisfyElement isHeader

writeInlines = T.unpack
  . fromRight (error "Can't write inlines")
  . runPure
  . writePlain def
  . Pandoc (Meta M.empty)
  . (:[]) . Plain

findElement :: Stream s m PandocElement =>
  ParsecT s u m a -> ParsecT s u m a
findElement p = findElementSkip p anyElement

findElementSkip :: Stream s m PandocElement =>
  ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m a
findElementSkip p skip = do
  r <- findMaybe
  case r of
    Just e  -> return e
    Nothing -> findElement p
  where findMaybe = try (Just <$> p) <|> (Nothing <$ skip)

satisfyInline :: Stream s m PandocElement =>
  (Inline -> Bool) -> ParsecT s u m Inline
satisfyInline p = do
  toInline =<< (satisfyElement $ \e -> maybe False p (toInline e))

anyInline :: Stream s m PandocElement => ParsecT s u m Inline
anyInline = satisfyInline (const True)

inline i = satisfyInline (== i) <?> "(" ++ show i ++ ")"
