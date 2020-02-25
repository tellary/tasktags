module KeepTimeTagParserTest where

import Control.Exception (assert)
import Data.Either
import Data.Time
import KeepTimeTagParser
import Text.Parsec (parse)

d = parse keepHeader "" "20200210 time tracking\n"

t1 = assert ((show . fromRight undefined $ d) == "2020-02-10") "t1"

entries = fromRight undefined . parse (keepTimeEntries $ read "-0800") ""
          <$> readFile "test/keep.txt"
t2 = assert . not . null <$> entries <*> pure "Entries parsed"

tests = sequence [return t1, t2]
