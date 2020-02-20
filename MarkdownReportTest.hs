import MarkdownReport
import KeepTimeTagParserTest

r = markdownReport <$> entries
putR = putStr =<< r
