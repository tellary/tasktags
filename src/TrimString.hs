module TrimString(TrimString, trimString) where

import Data.Char   (isSpace)
import Data.String (IsString (fromString))
import Text.Printf (PrintfArg (formatArg), formatString)

newtype TrimString = TrimString String deriving (Eq, Ord)

instance Show TrimString where
  show (TrimString s) = s

trimString = TrimString . trim

trim s = reverse . dropWhile isSpace . reverse $ s1
  where s1 = dropWhile isSpace s

instance IsString TrimString where
  fromString = trimString

instance PrintfArg TrimString where
  formatArg (TrimString s) = formatString s
