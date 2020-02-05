{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module PandocStream where

import Text.Pandoc
import Text.Parsec


data PandocStream =
    PandocStream Pandoc
  | BlockStream [Block]
  | InlineStream [Block] [Inline]
  deriving Show

data PandocElement =
    BlockElement Block
  | InlineElement Inline
  deriving (Eq, Show)

toBlock (BlockElement b) = return b
toBlock _                = fail "Not a block element"

toInline (InlineElement i) = return i
toInline _                 = fail "Not an inline"

instance Monad m => Stream PandocStream m PandocElement where
  uncons (PandocStream (Pandoc _ bs)) = uncons (BlockStream bs)

  uncons (BlockStream (h@(Header _ _ is):bs)) =
    return $ Just (BlockElement h, InlineStream bs is)
  uncons (BlockStream (p@(Para is):bs))       =
    return $ Just (BlockElement p, InlineStream bs is)
  uncons (BlockStream (_:bs))                 = uncons (BlockStream bs)
  uncons (BlockStream [])                     = return Nothing

  uncons (InlineStream bs (s@(Str _):is)) =
    return $ Just (InlineElement s, InlineStream bs is)
  uncons (InlineStream bs (_:is))         = uncons (InlineStream bs is)
  uncons (InlineStream bs [])             = uncons (BlockStream bs)
