module MarkdownReportTest where

import Control.Exception (assert)
import MarkdownReport
import qualified KeepTimeTagParserTest as KT

report = markdownReport . teSortByStartOfTask <$> KT.entries
putR = putStr =<< report

expected = readFile "test/keep.md"

t1 = (assert .) <$> (==) <$> expected <*> report
     <*> pure "Google Keep entries are rendered in Markdown as expected"

tests = sequence [t1]
