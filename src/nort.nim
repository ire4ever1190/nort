import nort/combinators
export combinators

## .. importdoc:: nort/base.nim
## .. importdoc:: nort/combinators.nim
##
## This is a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) library with an emphasis on being typed. This allows
## you to parse a string into structured data in a single step.
## There are many combinators built into the library which you can find in [combinators](nort/combinators.html). There are also some
## pre-built [helpers](nort/helpers.html) that can come in handy
##
## ## Basics
##
## ### Expectation
## Use [e] or [expect] to match a literal string, character, or a set of characters.
runnableExamples:
  let a = e"hello"     # Matches "hello"
  let b = e'a'         # Matches 'a'
  let c = e({'a'..'z'}) # Matches any lowercase letter
##
## ### Joining
## Use `*` infix to join combinators sequentially.
##
## Type matching rules:
## - `Tuple * Tuple` -> Merged Tuple
## - `Tuple * Non-Tuple` -> Tuple
## - `Non-Tuple * Tuple` -> Tuple
## - `Non-Tuple * Non-Tuple` -> `Void`
## - `T * Void` -> `T`
## - `Void * T` -> `T`
## - `Void * Void` -> `Void`
##
runnableExamples:
   let g = e"foo" * e"bar" # Matches "foobar"
##
## ### Repetition
## Prefix a combinator with `*` for zero or more repetitions, or `+` for one or more.
## See [Chain] for how different types are merged together
runnableExamples:
  let many = *e"a" # matches "", "a", "aa", etc.
  let some = +e"a" # matches "a", "aa", etc.
##
## ### Alternatives
## Use `|` or [any] to try multiple combinators until one matches.
runnableExamples:
  let either = e"yes" | e"no"
  let anyOf = any(e"a", e"b", e"c")
##
## ### Naming
## Use the `$` operator to bind a result to a combinator's result as a field name in the resulting tuple.
## When using `combinator$name`, the result of the combinator is wrapped in a tuple `tuple[name: T]`.
runnableExamples:
  let name = e"John"$name
##
## ### Optional
## Use [?] to make a combinator optional.
runnableExamples:
  let opt = ?e"hello"
##
## ### Transformation
## Use [map] to transform the result of a combinator.
runnableExamples:
  import std/strutils

  let upper = e"hello".map(toUpperAscii) # Will return "HELLO"
##
## ### Filtering
## Use [filter] to constrain the values returned by a combinator.
runnableExamples:
  import std/sugar

  let digits = filter(digit(), it => it > 5)

##
## ## Worked Example
## If we had a list of users along with their login counts, we could parse it easily like so
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
    attempts = e"attempts:" * digit()$count # We can capture and store in a name with `$`
    failed = e"failed:" * digit()$count
    # `*` is used to join items
    line = name$name * (e' ') * attempts$attempt * (e' ') * failed$failure
    # Just like regex, * and + can be used to repeat items. They need to be
    # prefixes though
    everything = *line

  echo name.match("jgod1")

  for line in everything.match(data).get():
    # Each line has a tuple with the names we binded
    if line.failure.count == 0: # Count was actually parsed as an int!
      echo fmt"{line.name} never failed!"


# Imports for docs
{.push warning[UnusedImport]: off.}
import nort/helpers
