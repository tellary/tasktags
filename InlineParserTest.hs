import InlineParser

import Control.Exception (assert)
import Text.Pandoc
import Text.Parsec

p = [Str "<task-start",Space,Str "t=\"20180506",Space,Str "13:41:02",Space,Str "-0700\"/>",SoftBreak,Str "<task-stop",Space,Str "t=\"20180506",Space,Str "14:05:18",Space,Str "-0700\"/>"]

start1 = parse startStopTag "" p
t1 = assert (start1 == Right (True,"20180506 13:41:02 -0700")) "t1"

startStops1 = parse startStopTags "" p

t2 = assert
  (startStops1 == Right [
      (True,"20180506 13:41:02 -0700"),
      (False,"20180506 14:05:18 -0700")])
  "t2"

tests = [t1, t2]
