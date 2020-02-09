import Control.Exception (assert)
import Data.Ini
import TaskTagsConfig

tasktagsConfig = readIniFile "test/.tasktags"

t1 = assert . (== Right "name@example.com")
     <$> iniEmail <$> tasktagsConfig
     <*> pure "iniEmail as expected"

tests = sequence [t1]
