module MarkdownReport where

import Data.Time
import TimeTag

formatDate = formatTime defaultTimeLocale "%Y-%b-%d"

printDate    = (++ "\n\n") . ("# "   ++) . formatDate . teStart
printProject = (++ "\n\n") . ("## "  ++) . teProject
printTask    = (++ "\n\n") . ("### " ++) . teTask

printStart te = "<task-start t=\"" ++ formatTagTime (teStart te) ++ "\"/>\n"
printStop  te = "<task-stop t=\""  ++ formatTagTime (teStop  te) ++ "\"/>\n"

markdownReport :: [TimeEntry] -> String
markdownReport [] = ""
markdownReport (t:ts) =
  date ++ project ++ task
  ++ printStart t ++ printStop t
  ++ reportTask date project task ts
  where date    = printDate    t
        project = printProject t
        task    = printTask    t

reportTask _    _       _    []     = ""
reportTask date project task (t:ts)
  | date' == date && project' == project && task' == task =
      start ++ stop ++ cont
  | date' == date && project' == project =
      "\n" ++ task' ++ start ++ stop ++ cont
  | date' == date =
      "\n" ++ project' ++ task' ++ start ++ stop ++ cont
  | otherwise =
      "\n" ++ markdownReport ts
  where date'    = printDate    t
        project' = printProject t
        task'    = printTask    t
        start    = printStart   t
        stop     = printStop    t
        cont     = reportTask date' project' task' ts
