## Some basic checks on the base functions. Helps track problems faster

import std/[unittest, strutils]

import nort
import nort/[base, helpers, parser]

import ./utils

suite "continuesWith":
  let p = initParser("hello world")

  test "It exists at the start":
    check p.continuesWith("hello").isSome()

  test "Doesn't exist at the start":
    check p.continuesWith("world").isNone()

  test "Can match something at the end":
    check p.skip("hello ".len).continuesWith("world").isSome()

  test "Doesn't match if no space left":
    check p.skip("hello w".len).continuesWith("world").isNone()
