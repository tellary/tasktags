import           Control.Exception (assert)
import           Data.Either (fromRight)
import qualified Data.Text as T
import           PandocParser
import           PandocStream
import           Text.Pandoc
import           Text.Parsec
import           TimeTagParser

readPandoc f = fromRight (error $ "Can't read " ++ f)
  . runPure . readMarkdown def . T.pack <$> readFile f
readPandocStream f = PandocStream <$> readPandoc f

testPandoc = readPandoc "test/test.md"

testPandocStream = PandocStream <$> testPandoc

tagTime = maybe (error "Bad time") id . parseTagTime

tag1 = fromRight undefined . parse (findElement $ timeTag "p" "t") ""
  <$> testPandocStream

t1 = assert
  .   (== StartTimeTag "p" "t" (tagTime "20180506 12:31:51 -0700"))
  <$> tag1 <*> pure "tag1 correct"

tags1Either = parse ((count 2 $ findElement (headerS "Project A"))
                     *> taskTimeTags "p") ""
              <$> testPandocStream
tags1 = fromRight undefined <$> tags1Either

taskA2Entries =
  [StartTimeTag "p" "Task A2" $ tagTime "20180506 12:31:51 -0700",
   StartTimeTag "p" "Task A2" $ tagTime "20180506 12:20:54 -0700",
   StopTimeTag  "p" "Task A2" $ tagTime "20180506 12:25:50 -0700",
   StopTimeTag  "p" "Task A2" $ tagTime "20180506 12:41:18 -0700"]

t2 = assert
     .   (== taskA2Entries)
     <$> tags1 <*> pure "Tags after second \"Task A2\" are correct"

tags0 = fromRight undefined
        .   parse (findElement (headerS "Task A1")
                 *> taskTimeTags "p") ""
        <$> testPandocStream
t3 = assert . (== []) <$> tags0
     <*> pure "No tags after first \"Task A2\" header"

allTagsEither = parse timeTags "" <$> testPandocStream
allTags = fromRight undefined <$> allTagsEither

expectedTimeTags =
  [StartTimeTag "Project A" "Task A2" $ tagTime "20180506 12:31:51 -0700",
   StartTimeTag "Project A" "Task A2" $ tagTime "20180506 12:20:54 -0700",
   StopTimeTag  "Project A" "Task A2" $ tagTime "20180506 12:25:50 -0700",
   StopTimeTag  "Project A" "Task A2" $ tagTime "20180506 12:41:18 -0700",
   StartTimeTag "Project A" "Task A3" $ tagTime "20180506 09:00:02 -0700",
   StopTimeTag  "Project A" "Task A3" $ tagTime "20180506 11:05:00 -0700",
   StartTimeTag "Project B" "Task B2" $ tagTime "20180506 13:41:02 -0700",
   StopTimeTag  "Project B" "Task B2" $ tagTime "20180506 14:05:18 -0700"]

t4 = assert
     .   (== expectedTimeTags)
     <$> allTags
     <*> pure "All tags from test.md are as expected"

loadedTimeEntriesEither = parse timeEntries "" <$> testPandocStream
loadedTimeEntries = fromRight undefined <$> loadedTimeEntriesEither
expectedTimeEntries = [
  TimeEntry
    "Project A" "Task A3"
    (tagTime "20180506 09:00:02 -0700")
    (tagTime "20180506 11:05:00 -0700"),
  TimeEntry
    "Project A" "Task A2"
    (tagTime "20180506 12:20:54 -0700")
    (tagTime "20180506 12:25:50 -0700"),
  TimeEntry
    "Project A" "Task A2"
    (tagTime "20180506 12:31:51 -0700")
    (tagTime "20180506 12:41:18 -0700"),
  TimeEntry
    "Project B" "Task B2"
    (tagTime "20180506 13:41:02 -0700")
    (tagTime "20180506 14:05:18 -0700")
  ]
testTimeEntries =
  assert . (== expectedTimeEntries)
  <$> loadedTimeEntries <*> pure "test.md time entries are as expected"

togglCsv = toTogglCsv "tellary@gmail.com" <$> loadedTimeEntries

expectedTogglCsv = readFile "test/toggl.csv"

testTogglCsv = do
  csv <- togglCsv
  exp <- expectedTogglCsv
  return $ assert (csv == exp) "toggl.csv as expected"

tests = do
  putStr . unlines =<< sequence [
    t1, t2, t3, t4, testTimeEntries, testTogglCsv]
