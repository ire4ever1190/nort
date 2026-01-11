# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import std/[unittest, strutils]

import nort
import nort/base

template check(comb: Combinator, tests: openArray[(string, bool)]) =
  ## Runs a series of checks
  for (input, expected) in tests:
    let matched = comb.test(input)
    if matched != expected:
      checkpoint input
    check matched == expected

template check[T](comb: Combinator[T], tests: openArray[(string, T)]) =
  ## Runs a series of checks
  for (input, expected) in tests:
    let matched = comb.match(input)
    if matched.isNone() or matched.get() != expected:
      checkpoint input

    check matched.isSome()
    check matched.get() == expected

test "Can match digit":
  let g = digit
  g.check {
    "1": 1,
    "-1": -1,
    "00999": 999
  }
  g.check {
    "a": false,
  }

test "Can match string":
  let g = e"hello"
  g.check {
    "hello": "hello",
    "helloworld": "hello"
  }

test "Can match * that are turned into strings":
  let g = *e'c'
  g.check {
    "": "",
    "ccc": "ccc",
    "a": ""
  }

test "* doesn't eat too much":
  let g = (*e'a') * e'b'
  g.check {
    "b": true,
    "aaaab": true,
    "a": false
  }

test "Can match +":
  let g = +e'c'
  g.check {
    "ccc": "ccc",
    "c": "c"
  }
  g.check {
    "": false,
    "c": true
  }

test "fin matches end of string":
  let g = e"hello" * fin
  g.check {
    "hello world": false,
    "hello": true
  }


test "Can match union":
  let init = any(e("hello"), e("goodbye"))
  let val = any((
    bar: init * e(Whitespace) * e"world" * fin,
    foo: e"world"
  )).match("goodbye world")

  checkpoint $val

  require val.isSome()

  case val.get.branch
  of foo: discard
  of bar: discard

  echo val.get().bar

test "Negation matches":
  let g = not e"hello"
  g.check {
    "world": true,
    "hello": false
  }
