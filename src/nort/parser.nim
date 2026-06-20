## Contains basic parsing code. This can be reused for writing your own combinators

import std/[options, parseutils, sugar]

type
  Parser* = object
    ## Represents the base parser for stepping through a stream.
    ## Used to represent a slice without needing to copy everything
    data: ref string # done to ensure the string isn't copied
    pos: int ## Current position inside `data`

func initParser*(data: sink string): Parser =
  ## Initialises a new parser
  let payload = new string
  payload[] = data
  Parser(data: payload, pos: 0)

func len*(p: Parser): int =
  ## Returns the length of remaining data
  return p.data[].len - p.pos

func eof*(p: Parser): bool =
  ## Checks if the parser is at the end of the data
  return p.len == 0

func slice*(p: Parser, data: HSlice[int, int]): string =
  ## Returns a slice of data. This is for debugging, should
  ## not be used for actual parsing
  return p.data[][data]

func pos*(p: Parser): int =
  ## Getter for the parsers current position
  return p.pos

func peek*(p: Parser): Option[char] =
  ## Returns next character (if not eof). Does not use input
  if p.eof: none(char)
  else: some p.data[p.pos]

func skip*(p: sink Parser, n: int): Parser =
  ## Skips the parser by `n` characters
  return Parser(data: p.data, pos: p.pos + n)

func eat*(p: sink Parser): Option[(Parser, char)] =
  ## Attempts to grab the next character and return rest of data
  p.peek().map(c => (p.skip(1), c))

func continuesWith*(p: sink Parser, token: sink string): Option[(Parser, string)] =
  ## Checks if the parser continues with a string
  let matched = p.data[].skip(token, p.pos) == 0
  if matched: some((p.skip(token.len), token))
  else: none((Parser, string))
