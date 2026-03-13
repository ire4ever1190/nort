## Contains basic parsing code. This can be reused for writing your own combinators

import std/[options, parseutils, sugar]

type
  Parser* = object
    ## Represents the base parser for stepping through a stream.
    ## Used to represent a slice without needing to copy everything
    data*: string # TODO: Ensure this is copy-on-write or else we'll explode
    pos*: int ## Current position inside `data`

func len*(p: Parser): int =
  ## Returns the length of remaining data
  return p.data.len - p.pos

func eof*(p: Parser): bool =
  ## Checks if the parser is at the end of the data
  return p.len == 0

func peek*(p: Parser): Option[char] =
  ## Returns next character (if not eof). Does not use input
  if p.eof: none(char)
  else: some p.data[p.pos]

func skip*(p: Parser, n: int): Parser =
  ## Skips the parser by `n` characters
  return Parser(data: p.data, pos: p.pos + n)

func eat*(p: Parser): Option[(Parser, char)] =
  ## Attempts to grab the next character and return rest of data
  p.peek().map(c => (p.skip(1), c))

func continuesWith*(p: Parser, token: string): Option[(Parser, string)] =
  ## Checks if the parser continues with a string
  let init = p.pos
  p.pos += p.data.skip(token, start = p.pos)
  if p.pos == init: none(string)
  else: some((p.skip(token.len), token))
