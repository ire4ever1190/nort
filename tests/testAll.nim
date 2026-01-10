# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

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


test "Can match *":
  let g = *e'c'
  g.check {
    "": @[],
    "ccc": @['c', 'c', 'c'],
    "a": @[]
  }

test "Can match +":
  let g = +e'c'
  g.check {
    "ccc": @['c', 'c', 'c'],
    "c": @['c']
  }
  g.check {
    "": false,
    "c": true
  }


test "Can match union":
  let init = any(expect("hello"), expect("goodbye"))
  let val = any((
    bar: init * e" world" * fin,
    foo: e" world"
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
