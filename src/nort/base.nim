## This is internal library code to set everything up
import std/[options, macros, strformat]
export options

import ./parser

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  ParseResult*[T] = tuple[parser: Parser, value: T]
    ## Single parse result. This should return the remaining data to parse along with
    ## the value that was returned

  ParseTree*[T] = seq[ParseResult[T]]
    ## Tree of parsed result values

  Combinator*[T] = proc (p: Parser): ParseTree[T] {.closure.}
    ## Parser that optionally returns data.
    ## This returns all possible matches of running the parser
    # TODO: Make this lazy

  Chain*[T] = (when T is char: string elif T is Void: Void else: seq[T])
    ## Represents how types get chained together when using repition like `+` and `*`
    ## - `char` becomes a `string`
    ## - `Void` stays `Void`, it doesn't make sense to join these
    ## - everything else gets joined in a sequence

proc bindTo*[T; R: tuple](comb: Combinator[T]): Combinator[R] =
  return proc (p: Parser): ParseTree[(T,)] =
    for val in comb(p):
      result &= (val.parser, (val.value,))

proc bindTo*[T, R](comb: Combinator[Void]) {.error: "Can't attach a variable name to a `Void` combinator".}

template `$`*[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)

proc trace*[T](comb: Combinator[T]): Combinator[T] =
  ## Utility function that echos the result of a combinator
  return proc (p: Parser): ParseTree[T] =
    result = comb(p)
    if result.len == 0:
      echo "Failed to parse"
    else:
      for path in result:
        echo fmt"Parsed: '{p.data[p.pos ..< path.parser.pos]}'"
        when T isnot Void:
            echo fmt"Got: '{path.value}'"


# Functions to make Void compose with Chain
proc add*(coll: var Chain[Void], val: Void) = discard
proc `&`*(a, b: Void): Void = a

proc match*[T](comb: Combinator[T], data: string): Option[T] =
  ## Checks if a string matches a pattern. Returns the first match
  let p = Parser(data: data)
  for res in comb(p):
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

  var stored: Combinator[T] = nil
  return proc (p: Parser): ParseTree[T] =
    if stored == nil:
      stored = comb()
    return stored(p)
