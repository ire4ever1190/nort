import nort/combinators
export combinators

## This is a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) library with an emphasis on being typed. This allows
## you to parse a string into structured data in a single step.
##
## For example, if we had a list of users along with their login counts, we could parse it easily like so
runnableExamples:
  import std/strutils
  # Data we are going to parse
  const data = """
  jdog1 attempts:5 failed:3
  robot attempts:700 failed:0
  """.unindent()

  # Unlike regex, we can build up the grammar to make it easier to read
