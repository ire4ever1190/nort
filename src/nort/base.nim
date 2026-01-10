## This is internal library code to set everything up
import std/options
export options

import ./parser

type
  Void* = object
    ## Internal type for representing a parser that returns nothing

  Combinator*[T] = proc (p: var Parser): Option[T] {.closure.}
    ## Parser that optionally returns data




proc bindTo[T; R: tuple](comb: Combinator[T]): Combinator[R] =
  return proc (p: var Parser): Option[(T,)] =
    let val = comb(p)
    if val.isSome():
      return some((val.get(),))

template `$`*[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)
