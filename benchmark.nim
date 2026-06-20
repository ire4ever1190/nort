import benchy
import nort
import std/strutils

timeIt "Parse a long string":
  let g = *e('a')
  doAssert g.match("a".repeat(8000)).isSome()

timeIt "Parse a long string in bigger chunks":
  let g = *e("a")
  doAssert g.match("a".repeat(8000)).isSome()

timeIt "Parse a long string, ignoring everything":
  let g = *dot()
  doAssert g.match("a".repeat(8000)).isSome()
