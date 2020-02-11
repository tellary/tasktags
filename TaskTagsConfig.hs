module TaskTagsConfig where

import           Data.Maybe (fromJust, isJust)
import           Data.Ini
import qualified Data.Text as T
import           System.Directory (getHomeDirectory)

iniEmail = (getEmail =<<)
  where getEmail ini =
          case lookup (T.pack "email") $ iniGlobals ini of
            Just e  -> Right $ T.unpack e
            Nothing -> Left  "Global key 'email' not found"

loadEmail maybeConfig = do
  c <- if isJust maybeConfig
       then return $ fromJust $ maybeConfig
       else (++) <$> getHomeDirectory <*> pure "/.tasktags"
  e <- iniEmail <$> readIniFile c
  case e of
    Right e'  -> return e'
    Left  err -> fail err
