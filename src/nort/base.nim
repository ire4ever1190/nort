## This is internal library code to set everything up
import std/options
export options

import ./parser

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  Combinator*[T] = proc (p: var Parser): Option[T] {.closure.}
    ## Parser that optionally returns data


template `$`[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)
