import nort/combinators
export combinators

## This is a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) library with an emphasis on being typed. This allows
## you to parse a string into structured data in a single step.
##
## For example, if we had a list of users along with their login counts, we could parse it easily like so
runnableExamples:
  import std/[strutils, strformat]
  # Data we are going to parse
  const data = """
  jdog1 attempts:5 failed:3
  robot attempts:700 failed:0
  """.unindent()

  # Unlike regex, we can build up the grammar to make it easier to read
  let
    # `expect` is the main function for mapping
    name = +(expect IdentChars) # Username is anything that isn't Whitespace
    # `e` is an alias for expect
    attempts = e"attempts:" * digit$count # We can capture and store in a name with `$`
    failed = e"failed:" * digit$count
    # `*` is used to join items
    line = name$name * (e' ') * attempts$attempt * (e' ') * failed$failure
    # Just like regex, * and + can be used to repeat items. They need to be
    # prefixes though
    everything = *line

  echo name.match("jgod1")

  for line in everything.match(data):
    # Each line has a tuple with the names we binded
    if line.failure.count == 0: # Count was actually parsed as an int!
      echo fmt"{line.name} never failed!"
