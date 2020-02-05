import           Control.Exception (assert)
import           Data.Either (fromRight)
import qualified Data.Text as T
import           Text.Pandoc
import           Text.Parsec
import           PandocParser
import           PandocStream

testPandoc = fromRight (error "Can't read test.md")
  . runPure . readMarkdown def . T.pack <$> readFile "test/test.md"

testPandocStream = PandocStream <$> testPandoc

blockToHeaderMaybe h@(Header _ _ _) = Just h
blockToHeaderMaybe _                = Nothing

h2 = fromRight undefined . parse (findElement (header 2)) ""
  <$> testPandocStream

t1 = do
  Header l _ is <- h2
  return [
    assert (l == 2) "h2 header level is 2",
    assert (writeInlines is == "Project A") "h2 header is 'Project A'"]

i1 = fromRight undefined . parse (findElement anyInline) ""
     <$> testPandocStream

t2 = assert . (== Str "2018-May-03") <$> i1 <*> pure "i1 is 2018-May-03"

-- parse (findElement $ inline (Str "<task-start")) "" <$> testPandocStream
