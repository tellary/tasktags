import           Control.Exception (assert)
import           Data.Either (fromLeft, fromRight)
import           Data.List (isInfixOf)
import qualified Data.Text    as T
import           PandocParser
import           PandocStream
import           Text.Pandoc
import           Text.Parsec
import           TimeTagParser

readPandocStream f = PandocStream <$> readPandoc f

testPandoc = readPandoc "test/test.md"

testPandocStream = PandocStream <$> testPandoc

tagTime = maybe (error "Bad time") id . parseTagTime

tag1 = fromRight undefined . parse (findElement $ timeTag "p" "t") ""
  <$> testPandocStream

t1 = assert
  .   (== StartTimeTag "p" "t" (tagTime "20180506 12:25:50 -0700"))
  <$> tag1 <*> pure "tag1 correct"

tags1Either = parse ((count 2 $ findElement (headerS "Project A"))
                     *> taskTimeTags "p") ""
              <$> testPandocStream
tags1 = fromRight undefined <$> tags1Either

taskA2Entries =
  [StartTimeTag "p" "Task A2" $ tagTime "20180506 12:25:50 -0700",
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
  [StartTimeTag "Project A" "Task A2"     $ tagTime "20180506 12:25:50 -0700",
   StartTimeTag "Project A" "Task A2"     $ tagTime "20180506 12:20:54 -0700",
   StopTimeTag  "Project A" "Task A2"     $ tagTime "20180506 12:25:50 -0700",
   StopTimeTag  "Project A" "Task A2"     $ tagTime "20180506 12:41:18 -0700",
   StartTimeTag "Project A" "Task \"A3\"" $ tagTime "20180506 09:00:02 -0700",
   StopTimeTag  "Project A" "Task \"A3\"" $ tagTime "20180506 11:05:00 -0700",
   StartTimeTag "Project B" "Task B2"     $ tagTime "20180506 13:41:02 -0700",
   StopTimeTag  "Project B" "Task B2"     $ tagTime "20180506 14:05:18 -0700"]

t4 = assert
     .   (== expectedTimeTags)
     <$> allTags
     <*> pure "All tags from test.md are as expected"

loadedTimeEntriesEither = parse timeEntries "" <$> testPandocStream
loadedTimeEntries = fromRight undefined <$> loadedTimeEntriesEither
expectedTimeEntries = [
  TimeEntry
    "Project A" "Task \"A3\""
    (tagTime "20180506 09:00:02 -0700")
    (tagTime "20180506 11:05:00 -0700"),
  TimeEntry
    "Project A" "Task A2"
    (tagTime "20180506 12:20:54 -0700")
    (tagTime "20180506 12:25:50 -0700"),
  TimeEntry
    "Project A" "Task A2"
    (tagTime "20180506 12:25:50 -0700")
    (tagTime "20180506 12:41:18 -0700"),
  TimeEntry
    "Project B" "Task B2"
    (tagTime "20180506 13:41:02 -0700")
    (tagTime "20180506 14:05:18 -0700")
  ]
testTimeEntries =
  assert . (== expectedTimeEntries)
  <$> loadedTimeEntries <*> pure "test.md time entries are as expected"

togglCsv = toTogglCsv "name@example.com" <$> loadedTimeEntries

expectedTogglCsv = readFile "test/toggl.csv"

testTogglCsv = do
  csv <- togglCsv
  exp <- expectedTogglCsv
  return $ assert (csv == exp) "toggl.csv as expected"

h3WithoutH2WithTag =
     "# 2018-May-06\n"
  ++ "\n"
  ++ "### H3 without H2\n"
  ++ "\n"
  ++ "<task-start t=\"20180506 12:20:54 -0700\"/>\n"
  ++ "\n"
  ++ "H3 without H2 is disallowed if it has a time tag\n"
  ++ "\n"
  ++ "## Project A\n"
  ++ "\n"
  ++ "### Task A2\n"
  ++ "\n"
  ++ "<task-start t=\"20180506 12:31:51 -0700\"/>\n"
  ++ "\n"
parseErrorH3WithoutH2WithTag = fromLeft undefined
  . parse timeTags ""
  . PandocStream
  . fromRight undefined
  . runPure . readMarkdown def . T.pack $ h3WithoutH2WithTag
testParseErrorH3WithoutH2WithTag =
  return $ assert
  ("unexpected InlineElement (Str \"<task-start\")"
     `isInfixOf` show parseErrorH3WithoutH2WithTag)
  "parseErrorH3WithoutH2WithTag"

lifelogEither = parse timeEntries "" . PandocStream <$> skipReadPandoc 1026089 "/home/ilya/safeplace/lifelog/lifelog.md"
lifelogCSV = toTogglCsv "tellary@gmail.com" . fromRight undefined <$> lifelogEither

tests = do
  putStr . unlines =<< sequence [
    t1, t2, t3, t4, testTimeEntries, testTogglCsv,
    testParseErrorH3WithoutH2WithTag]
