## This is internal library code to set everything up
import std/[options, macros, strformat, sugar]
export options

import ./[parser, utils]

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  ParseResult*[P, T] = tuple[parser: P, value: T]
    ## Single parse result.
    ## This should return a parser with the new state along with the value that was parsed

  Explorer*[P, T] = iterator (p: P): ParseResult[P, T] {.closure.}
    ## Iterator that returns all the paths a parser can take

  BaseCombinator*[P, T] = object
    ## Parser that optionally returns data.
    ## This returns all possible matches of running the parser in a lazy manner
    iter: proc(): Explorer[P, T]
      ## Factory that produces an iterator with all the paths

  Combinator*[T] = BaseCombinator[Parser, T]
    ## Combinator that operates on strings
    ##
  Chain*[T] = (when T is char: string elif T is Void: Void else: seq[T])
    ## Represents how types get chained together when using repition like `+` and `*`
    ## - `char` becomes a `string`
    ## - `Void` stays `Void`, it doesn't make sense to join these
    ## - everything else gets joined in a sequence

iterator results*[P, T](comb: BaseCombinator[P, T], parser: P): ParseResult[P, T] =
  ## Yields all the iteration results of a combinator
  let iter = comb.iter()
  for item in iter(parser):
    yield item

proc initCombinator*[P, T](factory: proc (): Explorer[P, T]): BaseCombinator[P, T] =
  ## Builds a combinator from an iterator
  BaseCombinator[P, T](iter: factory)

proc bindTo*[T, P; R: tuple](comb: BaseCombinator[P, T]): BaseCombinator[P, R] =
  return initCombinator(proc (): Explorer[P, R] =
    iterator (p: P): ParseResult[P, (T,)] {.closure.} =
      for val in comb.results(p):
        yield (val.parser, (val.value,))
  )

proc bindTo*[T, R](comb: Combinator[Void]) {.error: "Can't attach a variable name to a `Void` combinator".}

template `$`*[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)

proc trace*[T](comb: Combinator[T]): Combinator[T] =
  ## Utility function that echos the result of a combinator
  return initCombinator(proc (): Explorer[T] =
    iterator (p: Parser): ParseResult[T] {.closure.} =
      var foundSomething = false
      for path in comb.results(p):
        foundSomething = true
        echo fmt"Parsed: '{p.data[p.pos ..< path.parser.pos]}'"
        when T isnot Void:
            echo fmt"Got: '{path.value}'"
        yield path
      if not foundSomething:
        echo "Failed to parse"
  )

# Functions to make Void compose with Chain
proc add*(coll: var Chain[Void], val: Void) = discard
proc `&`*(a, b: Void): Void = a

proc match*[T](comb: Combinator[T], data: string): Option[T] =
  ## Checks if a string matches a pattern. Returns the first match
  let p = Parser(data: data)
  for res in comb.results(p):
    return res.value.some()

proc test*[T](comb: Combinator[T], data: sink string): bool =
  ## Tests if an input matches a combinator
  runnableExamples:
    import nort

    let g = e"hello"
    assert g.test("hello")
    assert not g.test("bye")

  comb.match(data).isSome()

proc lazy*[T](comb: proc (): Combinator[T]): Combinator[T] =
  ## Makes a lazy version of `comb` that is only created when needed.
  ## Use this when you have recursive grammars
  runnableExamples:
    import nort
    import std/sugar

    proc paran(): Combinator[int] =
      ## Combinator that returns the nesting of paranthesis
      # We need `lazy` here or else it would go into an infinite loop calling itself
      lazy(paran).between(e'(', e')').map(count => count + 1) | succeed(0)

    let g = paran()
    assert g.match("(())").get() == 2
    assert g.match("").get() == 0

  return initCombinator(proc (): Explorer[T] =
    iterator (p: Parser): ParseResult[T] {.closure.} =
      yieldfrom comb().results(p)
  )
