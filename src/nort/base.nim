## This is internal library code to set everything up
import std/[options, macros, strformat]
export options

import ./parser

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  Combinator*[T] = proc (p: var Parser): Option[T] {.closure.}
    ## Parser that optionally returns data

  Chain*[T] = (when T is char: string elif T is Void: Void else: seq[T])
    ## Represents how types get chained together when using repition like `+` and `*`
    ## - `char` becomes a `string`
    ## - `Void` stays `Void`, it doesn't make sense to join these
    ## - everything else gets joined in a sequence

proc bindTo*[T; R: tuple](comb: Combinator[T]): Combinator[R] =
  return proc (p: var Parser): Option[(T,)] =
    let val = comb(p)
    if val.isSome():
      return some((val.get(),))

proc bindTo*[T, R](comb: Combinator[Void]) {.error: "Can't attach a variable name to a `Void` combinator".}

template `$`*[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)

proc trace*[T](g: Combinator[T]): Combinator[T] =
  ## Utility function that echos the result of a combinator
  return proc (p: var Parser): Option[T] =
    let start = p.pos
    result = g(p)
    if result.isNone:
      echo "Failed to parse"
    else:
      echo fmt"Parsed: '{p.data[start ..< p.pos]}'"
      when T isnot Void:
        echo fmt"Got: '{result.get()}'"


# Functions to make Void compose with Chain
proc add*(coll: var Chain[Void], val: Void) = discard
