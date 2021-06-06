module MarkdownReport where

import Data.List (groupBy, sortBy)
import Data.Ord  (comparing)
import Data.Time
import TimeTag

formatDate = formatTime defaultTimeLocale "%Y-%b-%d"

printDate    = (++ "\n\n") . ("# "   ++) . formatDate . teStart
printProject = (++ "\n\n") . ("## "  ++) . show . teProject
printTask    = (++ "\n\n") . ("### " ++) . show . teTask

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

compareByTaskAndTime t1 t2 =
  case comparing teTask t1 t2 of
    EQ -> comparing teStartUTC t1 t2
    r  -> r

teSortByStart = sortBy (comparing teStartUTC)

teSortByStartOfTask =
  concat . sortBy compareTaskGroups . taskGroups
  where taskGroups =
          groupBy (\t1 t2 -> teTask t1 == teTask t2)
          . sortBy compareByTaskAndTime
        compareTaskGroups g1 g2 =
          compare t1 t2
          where t1 = teStartUTC . head $ g1
                t2 = teStartUTC . head $ g2
