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

h2 = fromRight undefined . parse (findElement (headerL 2)) ""
  <$> testPandocStream

t1 = do
  Header l _ is <- h2
  return [
    assert (l == 2) "h2 header level is 2",
    assert (writeInlines is == "Project A") "h2 header is 'Project A'"]

i1 = fromRight undefined . parse (findElement anyInline) ""
     <$> testPandocStream

t2 = assert . (== Str "2018-May-03") <$> i1 <*> pure "i1 is 2018-May-03"

t3 = do
  Str s <- fromRight undefined
           .   parse (findElement $ inline (Str "<task-start")) ""
           <$> testPandocStream
  return $ assert (s == "<task-start") "t3"

t4 = do
  (Header l _ _) <- fromRight undefined
                    . parse anyHeader "" <$> testPandocStream
  return $ assert (l == 1) "t4"


h3 = fromRight undefined . parse (findElement (headerS "Task A1")) ""
  <$> testPandocStream
t5 = do
  Header l _ is <- h3
  return [
    assert (l == 3) "h3 header level is 3",
    assert (writeInlines is == "Task A1") "h3 header is 'Task A1'"]

tests = do
  putStrLn . show =<< t1
  putStrLn =<< t2
  putStrLn =<< t3
  putStrLn =<< t4
  putStrLn . show  =<< t5
