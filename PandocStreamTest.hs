import           Data.Either
import           Data.Maybe
import qualified Data.Text as T
import           PandocStream
import           Text.Pandoc
import           Text.Parsec

test = fromRight undefined
  . runPure . readMarkdown def . T.pack <$> readFile "test/test.md"

unconsElement :: Stream s m t => Int -> s -> m t
unconsElement 0 = (fmap (fst . fromJust) . uncons)
unconsElement n = (unconsElement 0 =<<) . unconsStream (n - 1)

unconsStream :: Stream s m t => Int -> s -> m s
unconsStream 0 s = fmap (snd . fromJust) . uncons $ s
unconsStream n s = do
  s' <- fmap (snd . fromJust) . uncons $ s
  unconsStream (n - 1) s'

-- putStr =<< unlines . map show <$> testList
testList = toList =<< PandocStream <$> test
