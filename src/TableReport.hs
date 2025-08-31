module TableReport where

import Data.List (groupBy, sortBy, intercalate)
import Data.Ord (comparing)
import Data.Time (Day, ZonedTime, formatTime, defaultTimeLocale, zonedTimeToLocalTime, localDay, localTimeOfDay)
import TimeTag (TimeEntry(..), teStartUTC)

formatTimeOfDay :: ZonedTime -> String
formatTimeOfDay = formatTime defaultTimeLocale "%H:%M:%S" . localTimeOfDay . zonedTimeToLocalTime

formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

wrapText :: Int -> String -> [String]
wrapText width text = wrapWords width (words text) []

wrapWords :: Int -> [String] -> String -> [String]
wrapWords _ [] currentLine 
  | null currentLine = []
  | otherwise = [currentLine]
wrapWords width (w:ws) currentLine
  | null currentLine = 
      if length w <= width
      then wrapWords width ws w
      else w : wrapWords width ws ""
  | length currentLine + 1 + length w <= width =
      wrapWords width ws (currentLine ++ " " ++ w)
  | otherwise =
      currentLine : wrapWords width (w:ws) ""

padLines :: Int -> [String] -> [String]
padLines width lines = map (\line -> line ++ replicate (width - length line) ' ') lines

data TableConfig = TableConfig
  { maxWidth :: Int
  , minColWidth :: Int
  } deriving Show

defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig { maxWidth = 120, minColWidth = 8 }

getMaxWordLength :: String -> Int
getMaxWordLength = maximum . map length . words

calculateColumnWidths :: TableConfig -> [[String]] -> (Int, Int, Int, Int)
calculateColumnWidths config rows =
  let startWidth = max (minColWidth config) $ maximum $ map (length . (!! 0)) rows
      stopWidth = max (minColWidth config) $ maximum $ map (length . (!! 1)) rows
      projectWidth = max (minColWidth config) $ maximum $ map (length . (!! 2)) rows
      taskWidth = max (minColWidth config) $ maximum $ map (length . (!! 3)) rows
      
      -- Ensure minimum width accommodates the longest word in each column
      startMaxWord = maximum $ map (getMaxWordLength . (!! 0)) rows
      stopMaxWord = maximum $ map (getMaxWordLength . (!! 1)) rows
      projectMaxWord = maximum $ map (getMaxWordLength . (!! 2)) rows
      taskMaxWord = maximum $ map (getMaxWordLength . (!! 3)) rows

      startWidthAdj = max startWidth startMaxWord
      stopWidthAdj = max stopWidth stopMaxWord
      projectWidthAdj = max projectWidth projectMaxWord
      taskWidthAdj = max taskWidth taskMaxWord

      -- Account for "| content | content | content | content |" format
      -- Each column needs +2 for " |" except the last which needs +2 for " |"
      totalWidth = startWidthAdj + stopWidthAdj + projectWidthAdj + taskWidthAdj + 4 * 2 + 1
      
      (adjProjectWidth, adjTaskWidth) = 
        if totalWidth <= maxWidth config
        then (projectWidthAdj, taskWidthAdj)
        else let available = maxWidth config - startWidthAdj - stopWidthAdj - 4 * 2 - 1
                 minProjectWidth = max (minColWidth config) projectMaxWord
                 minTaskWidth = max (minColWidth config) taskMaxWord
                 minTotal = minProjectWidth + minTaskWidth
                 projectRatio = fromIntegral projectWidthAdj / fromIntegral (projectWidthAdj + taskWidthAdj)
             in if available >= minTotal
                then let newProjectWidth = max minProjectWidth $
                           min projectWidthAdj $ floor $ fromIntegral available * projectRatio
                         newTaskWidth = max minTaskWidth $ available - newProjectWidth
                     in (newProjectWidth, newTaskWidth)
                else (minProjectWidth, minTaskWidth)
  in (startWidthAdj + 2, stopWidthAdj + 2, adjProjectWidth + 2, adjTaskWidth + 2)

createGridSeparator :: (Int, Int, Int, Int) -> Char -> String
createGridSeparator (startW, stopW, projW, taskW) char =
  "+" ++ replicate startW char ++ "+" ++ 
         replicate stopW char ++ "+" ++ 
         replicate projW char ++ "+" ++ 
         replicate taskW char ++ "+"

createGridHeaderSeparator :: (Int, Int, Int, Int) -> String
createGridHeaderSeparator (startW, stopW, projW, taskW) =
  "+:" ++ replicate (startW - 1) '=' ++ "+:" ++ 
          replicate (stopW - 1) '=' ++ "+:" ++ 
          replicate (projW - 1) '=' ++ "+:" ++ 
          replicate (taskW - 1) '=' ++ "+"

formatGridRow :: (Int, Int, Int, Int) -> [String] -> String
formatGridRow (startW, stopW, projW, taskW) [start, stop, proj, task] =
  "| " ++ padRight (startW - 2) start ++ " | " ++ 
         padRight (stopW - 2) stop ++ " | " ++ 
         padRight (projW - 2) proj ++ " | " ++ 
         padRight (taskW - 2) task ++ " |"
  where padRight width text = text ++ replicate (width - length text) ' '
formatGridRow _ row = error $ "Expected 4 columns, got " ++ show (length row)

wrapGridRow :: (Int, Int, Int, Int) -> [String] -> [String]
wrapGridRow colWidths@(startW, stopW, projW, taskW) [start, stop, proj, task] =
  let startLines = wrapText (startW - 2) start
      stopLines = wrapText (stopW - 2) stop
      projLines = wrapText (projW - 2) proj
      taskLines = wrapText (taskW - 2) task
      maxLines = maximum $ map length [startLines, stopLines, projLines, taskLines]
      
      padCellLines width lines = padLines width (lines ++ replicate (maxLines - length lines) "")
      
      startPadded = padCellLines (startW - 2) startLines
      stopPadded = padCellLines (stopW - 2) stopLines
      projPadded = padCellLines (projW - 2) projLines
      taskPadded = padCellLines (taskW - 2) taskLines
      
  in zipWith (\(s1, s2) (s3, s4) -> 
       "| " ++ s1 ++ " | " ++ s2 ++ " | " ++ s3 ++ " | " ++ s4 ++ " |")
       (zip startPadded stopPadded)
       (zip projPadded taskPadded)

wrapGridRow _ row = error $ "Expected 4 columns, got " ++ show (length row)

timeEntryToRow :: TimeEntry -> [String]
timeEntryToRow te = 
  [ formatTimeOfDay (teStart te)
  , formatTimeOfDay (teStop te)
  , show (teProject te)
  , show (teTask te)
  ]

groupTimeEntriesByDate :: [TimeEntry] -> [(Day, [TimeEntry])]
groupTimeEntriesByDate = 
  map (\group -> (localDay . zonedTimeToLocalTime . teStart . head $ group, group))
  . groupBy (\a b -> localDay (zonedTimeToLocalTime (teStart a)) == 
                     localDay (zonedTimeToLocalTime (teStart b)))
  . sortBy (comparing teStartUTC)

formatDateGroup :: TableConfig -> Day -> [TimeEntry] -> String
formatDateGroup config date entries =
  let header = "# " ++ formatDate date
      rows = map timeEntryToRow entries
      headerRow = ["Start", "Stop", "Project", "Task"]
      allRows = headerRow : rows
      colWidths = calculateColumnWidths config allRows
      
      topSeparator = createGridSeparator colWidths '-'
      headerSeparator = createGridHeaderSeparator colWidths
      rowSeparator = createGridSeparator colWidths '-'
      
      headerLines = wrapGridRow colWidths headerRow
      dataLines = concatMap (\row -> wrapGridRow colWidths row ++ [rowSeparator]) rows
      
      gridTable = [topSeparator] ++ 
                  headerLines ++ 
                  [headerSeparator] ++ 
                  init dataLines  -- Remove last separator
      
  in unlines $ [header, ""] ++ gridTable ++ [""]

tableReport :: [TimeEntry] -> String
tableReport = tableReportWithConfig defaultTableConfig

tableReportWithConfig :: TableConfig -> [TimeEntry] -> String
tableReportWithConfig config entries =
  let dateGroups = groupTimeEntriesByDate entries
  in concatMap (uncurry (formatDateGroup config)) dateGroups
