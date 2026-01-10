## Contains basic parsing code. This can be reused for writing your own combinators

import std/[options, parseutils]

type
  Parser* = object
    data*: string
    pos*: int

func eof*(p: Parser): bool =
  ## Checks if the parser is at the end of the data
  return p.pos >= p.data.len

func peek*(p: Parser): Option[char] =
  ## Returns next character (if not eof). Does not use input
  if p.eof: none(char)
  else: some p.data[p.pos]

func eat*(p: var Parser): Option[char] =
  ## Returns next character and also consumes input
  result = p.peek()
  p.pos += 1

func continuesWith*(p: var Parser, token: string): Option[string] =
  ## Checks if the parser continues with a string
  let init = p.pos
  p.pos += p.data.skip(token, start = p.pos)
  if p.pos == init: none(string)
  else: some(token)
