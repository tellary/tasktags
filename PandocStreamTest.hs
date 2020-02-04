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

unconsList :: Stream s m t => s -> m [t]
unconsList s = do
  u <- uncons s
  case u of
    Just (e, s') -> (e:) <$> unconsList s'
    Nothing      -> return []

-- putStrLn =<< unlines . map show <$> testList
testList = unconsList =<< PandocStream <$> test
