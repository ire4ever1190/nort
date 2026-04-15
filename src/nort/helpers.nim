## This contains pre-built grammars that you might find helpful

import ./combinators

import std/strutils

let
  nl*: Combinator[Void] = ?e('\r') * e('\n')
    ## Newline character. Is cross platform and handles both windows and linux line endings

  ws*: Combinator[Void] = -e(Whitespace)

  anyAll*: Combinator[Void] = *(-dot())
    ## Matches anything without returning any value. Use this to make your pattern a substring match
