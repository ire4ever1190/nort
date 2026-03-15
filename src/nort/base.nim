## This is internal library code to set everything up
import std/[options, macros, strformat, sugar]
export options

import ./[parser, utils]

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  ParseResult*[T] = tuple[parser: Parser, value: T]
    ## Single parse result. This should return the remaining data to parse along with
    ## the value that was returned

  ParseTree*[T] = seq[ParseResult[T]]
    ## Tree of parsed result values

  Explorer*[T] = iterator (p: Parser): ParseResult[T] {.closure.}
    ## Iterator that returns all the paths a parser can take

  Combinator*[T] = object
    ## Parser that optionally returns data.
    ## This returns all possible matches of running the parser in a lazy manner
    iter: proc(): Explorer[T]
      ## Internal interator that yields matches
  Chain*[T] = (when T is char: string elif T is Void: Void else: seq[T])
    ## Represents how types get chained together when using repition like `+` and `*`
    ## - `char` becomes a `string`
    ## - `Void` stays `Void`, it doesn't make sense to join these
    ## - everything else gets joined in a sequence

iterator results*[T](comb: Combinator[T], parser: Parser): ParseResult[T] =
  ## Yields all the iteration results of a combinator
  echo comb.iter().finished
  for item in comb.iter()(parser):
    yield item

template initCombinator*[T](explorer: Explorer[T]): Combinator[T] =
  ## Builds a combinator from an iterator
  Combinator[T](iter: () => explorer)

proc bindTo*[T; R: tuple](comb: Combinator[T]): Combinator[R] =
  return proc (p: Parser): ParseTree[(T,)] =
    for val in comb(p):
      result &= (val.parser, (val.value,))

proc bindTo*[T, R](comb: Combinator[Void]) {.error: "Can't attach a variable name to a `Void` combinator".}

template `$`*[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)

proc trace*[T](comb: Combinator[T]): Combinator[T] =
  ## Utility function that echos the result of a combinator
  return iterator (p: Parser): ParseResult[T] {.closure.} =
    var foundSomething = false
    for path in comb.results(p):
      foundSomething = true
      echo fmt"Parsed: '{p.data[p.pos ..< path.parser.pos]}'"
      when T isnot Void:
          echo fmt"Got: '{path.value}'"
      yield path
    if not foundSomething:
      echo "Failed to parse"

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

  return initCombinator(
    iterator (p: Parser): ParseResult[T] {.closure.} =
      yieldfrom comb().results(p)
  )
