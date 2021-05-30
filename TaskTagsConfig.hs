{-# LANGUAGE OverloadedStrings #-}

module TaskTagsConfig where

import           Data.ByteString.Char8 as C
import           Data.Ini              (Ini (iniGlobals), lookupValue,
                                        readIniFile)
import qualified Data.Text             as T
import           System.Directory      (canonicalizePath, getHomeDirectory)
import           System.FilePath       ((</>))
import           Text.Email.Validate   (validate)
import           Text.Printf           (printf)

iniEmail = (emailValidate =<<) . getEmail
  where getEmail ini =
          case lookup "email" $ iniGlobals ini of
            Just e  -> Right $ T.unpack e
            Nothing -> Left  "Global key 'email' not found"

emailValidate email =
  case validate $ C.pack email of
    Right _  -> Right email
    Left err -> Left $ printf "Bad email '%s': %s" email err

configEmail (IniFileConfig ini)
  = either fail return . iniEmail $ ini

data IniFileConfig = IniFileConfig Ini

iniFileConfig maybeConfigFile = do
  c <- case maybeConfigFile of
         Just c  -> return c
         Nothing -> (++) <$> getHomeDirectory <*> pure "/.tasktags"
  e <- readIniFile $ c
  case e of
    Right e'  -> return $ IniFileConfig e'
    Left  err -> fail err

class TaskTagsConfig config where
  fileTogglWorkspace :: config -> FilePath -> IO (Either String Int)
  togglApiKey :: config -> Either String String

instance TaskTagsConfig IniFileConfig where
  fileTogglWorkspace c ('~':t) = do
    hd <- getHomeDirectory
    fileTogglWorkspace c (hd </> t)
  fileTogglWorkspace (IniFileConfig ini) file = do
    file2 <- canonicalizePath file
    return
       $ read . T.unpack
      <$> lookupValue "togglWorkspaces" (T.pack file2) ini
  togglApiKey (IniFileConfig ini)
    = case lookup "togglApiKey" . iniGlobals $ ini of
        Just key -> Right . T.unpack $ key
        Nothing  -> Left "Global key 'togglApiKey' not found"

