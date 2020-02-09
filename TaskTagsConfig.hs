module TaskTagsConfig where

import           Data.Ini
import qualified Data.Text as T

iniEmail = (getEmail =<<)
  where getEmail ini =
          case lookup (T.pack "email") $ iniGlobals ini of
            Just e  -> Right $ T.unpack e
            Nothing -> Left  "Global key 'email' not found"
