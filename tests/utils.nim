import nort
import nort/base
import std/unittest

template check*(comb: Combinator, tests: openArray[(string, bool)]) =
  ## Runs a series of checks
  for (input, expected) in tests:
    let matched = comb.test(input)
    if matched != expected:
      checkpoint input
    check matched == expected

template check*[T](comb: Combinator[T], tests: openArray[(string, T)]) =
  ## Runs a series of checks
  for (input, expected) in tests:
    let matched = comb.match(input)
    if matched.isNone() or matched.get() != expected:
      checkpoint input

    check matched.isSome()
    check matched.get() == expected
