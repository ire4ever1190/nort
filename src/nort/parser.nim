## Contains basic parsing code. This can be reused for writing your own combinators

import std/[options, parseutils, sugar]
import pkg/casserole

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

template len*(p: Parser): int =
  ## Returns the length of remaining data
  p.data[].len - p.pos

template eof*(p: Parser): bool =
  ## Checks if the parser is at the end of the data
  p.len == 0

func slice*(p: Parser, data: HSlice[int, int]): string =
  ## Returns a slice of data. This is for debugging, should
  ## not be used for actual parsing
  p.data[][data]

template pos*(p: Parser): int =
  ## Getter for the parsers current position
  p.pos

template peek*(p: Parser): Option[char] =
  ## Returns next character (if not eof). Does not use input
  if p.eof: none(char)
  else: some p.data[][p.pos]

template skip*(p: sink Parser, n: int): Parser =
  ## Skips the parser by `n` characters
  Parser(data: p.data, pos: p.pos + n)

template eat*(p: sink Parser): Option[(Parser, char)] =
  ## Attempts to grab the next character and return rest of data
  let v = p.peek()
  if v.isSome():
    some((p.skip(1), v.unsafeGet()))
  else:
    none((Parser, char))

func continuesWith*(p: sink Parser, token: sink string): Option[(Parser, string)] =
  ## Checks if the parser continues with a string
  let matched = p.data[].skip(token, p.pos) > 0
  if matched: some((p.skip(token.len), token))
  else: none((Parser, string))
