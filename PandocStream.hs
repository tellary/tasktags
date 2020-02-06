{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module PandocStream where

import Text.Pandoc
import Text.Parsec


data PandocStream =
    PandocStream Pandoc
  | BlockStream [Block]
  | InlineStream [Inline] [Block]
  deriving Show

data PandocElement =
    BlockElement Block
  | InlineElement Inline
  deriving (Eq, Show)

toBlock (BlockElement b) = return b
toBlock _                = fail "Not a block element"

toInline (InlineElement i) = return i
toInline _                 = fail "Not an inline"

blockToHeader h@(Header _ _ _) = return h
blockToHeader _                = fail "Not a header block"

toStr (InlineElement (Str s)) = return s
toStr _                       = fail "Not string"

instance Monad m => Stream PandocStream m PandocElement where
  uncons (PandocStream (Pandoc _ bs)) = uncons (BlockStream bs)

  uncons (BlockStream (h@(Header _ _ is):bs)) =
    return $ Just (BlockElement h, InlineStream is bs)
  uncons (BlockStream (p@(Para is):bs))       =
    return $ Just (BlockElement p, InlineStream is bs)
  uncons (BlockStream (_:bs))                 = uncons (BlockStream bs)
  uncons (BlockStream [])                     = return Nothing

  uncons (InlineStream (s@(Str _):is) bs) =
    return $ Just (InlineElement s, InlineStream is bs)
  uncons (InlineStream (s@Space:is) bs) =
    return $ Just (InlineElement s, InlineStream is bs)
  uncons (InlineStream (_:is) bs)         = uncons (InlineStream is bs)
  uncons (InlineStream [] bs)             = uncons (BlockStream bs)

toList :: Stream s m t => s -> m [t]
toList s = do
  u <- uncons s
  case u of
    Just (e, s') -> (e:) <$> toList s'
    Nothing      -> return []
