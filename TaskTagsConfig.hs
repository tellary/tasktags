module TaskTagsConfig where

import           Data.Maybe (fromJust, isJust)
import           Data.Ini
import qualified Data.Text as T
import           Data.ByteString.Char8 as C
import           Text.Email.Validate (validate)
import           Text.Printf (printf)
import           System.Directory (getHomeDirectory)

iniEmail = ((emailValidate =<<) . getEmail =<<)
  where getEmail ini =
          case lookup (T.pack "email") $ iniGlobals ini of
            Just e  -> Right $ T.unpack e
            Nothing -> Left  "Global key 'email' not found"

emailValidate email =
  case validate $ C.pack email of
    Right _  -> Right email
    Left err -> Left $ printf "Bad email '%s': %s" email err

configEmail maybeConfig = do
  c <- if isJust maybeConfig
       then return $ fromJust $ maybeConfig
       else (++) <$> getHomeDirectory <*> pure "/.tasktags"
  e <- iniEmail <$> readIniFile c
  case e of
    Right e'  -> return e'
    Left  err -> fail err

class TaskTagsConfig config where
  fileTogglWorkspace :: config -> String -> Maybe Int
