import std/[options, sugar, parseutils, macros, sequtils, enumutils, strutils, setutils, sets, sequtils]

export options

import ./[base, parser, utils, union]

export union, base, sets, options

#
# Internal functions
#

macro mapAny(t: typedesc): typedesc =
  ## Maps a tuple type into a simple union.
  ## The union has an inline enum (names of the enum being the tuple fields) which is used as a discriminator
  ## in the object with each field being a tuple name.
  ## This does have problems since the enum isn't exposed cleanly, see the `branch` functions.
  ## TODO: Move into union.nim
  result = nnkTupleTy.newTree()
  var branches: seq[(string, NimNode)]
  for def in t.getTypeImpl()[1]:
    branches &= (def[0].strVal, nnkDotExpr.newTree(def[1], ident"T"))
  # Create a enum for all the branches
  let
    enumName = genSym(nskType, "descriminator")
    enumTyp = nnkEnumTy.newTree(newEmptyNode()).add(branches.map(b => ident b[0]))
    typName = genSym(nskType, "union")
    unionObj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(),
      nnkRecList.newTree(nnkRecCase.newTree(newIdentdefs(public(ident"name"), enumName))
      .add(branches.map(b => nnkOfBranch.newTree(ident b[0], nnkRecList.newTree(newIdentdefs(public(ident b[0]), b[1]))))))
    )
  result = newStmtList(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(enumName, newEmptyNode(), enumTyp),
      nnkTypeDef.newTree(typName, newEmptyNode(), unionObj)
    ),
    typName
  )

proc join[R](a: tuple, b: tuple, ret: typedesc[R]): R =
  ## Joins two tuples together. `ret` must be a combination of both tuples
  for name, val in a.fieldPairs:
    access(result, name) = val
  for name, val in b.fieldPairs:
    access(result, name) = val

#
# Combinators
#

proc filter*[P, T](comb: BaseCombinator[P, T], check: proc (val: T): bool): BaseCombinator[P, T] =
  ## Filter what values are allowed to consider that the filter has passed
  runnableExamples:
    import std/[strutils, sugar]
    let g = filter(*e({'a' .. 'z', ' '}), inp => inp.startsWith("hello"))
    assert g.test("hello world")
    assert not g.test("world")

  return initCombinator(proc (): Explorer[P, T] =
    iterator (p: P): ParseResult[P, T] {.closure.} =
      for res in comb.results(p):
        if check(res.value):
          yield res
  )

proc dot*(): Combinator[char] =
  ## Parses any character
  runnableExamples:
    assert dot().match("abc") == some('a')

  return initCombinator(proc (): Explorer[Parser, char] =
    iterator (p: Parser): ParseResult[Parser, char] {.closure.} =
      if p.eat().safeGet(res):
        yield res
  )

proc succeed*[T](value: T): Combinator[T] =
  ## Combinator that always successeds and never comsumes input
  return initCombinator(proc (): Explorer[Parser, T] =
    iterator (p: Parser): ParseResult[Parser, T] {.closure.} =
      yield (p, value)
  )

proc failure*(): Combinator[Void] =
  ## Combinator that matches nothing
  return initCombinator(proc (): Explorer[Parser, Void] =
    iterator (p: Parser): ParseResult[Parser, Void] {.closure.} =
      discard
  )

proc epsilon(): Combinator[Void] =
  ## Matches the empty string and doesn't return any input
  return succeed(Void())

proc expect*(input: char): Combinator[char] =
  ## Expects a character to appear
  runnableExamples:
    let g = expect('a')
    assert g.match("a") == some('a')
    assert g.match("b").isNone()

  return filter(dot(), it => it == input)

proc expect*(expect: set[char]): Combinator[char] =
  ## Expects a set of characters, returns the matched value
  runnableExamples:
    let g = expect({'a', 'b', 'c'})
    assert g.match("a") == some('a')
    assert g.match("d").isNone()
  return filter(dot(), it => it in expect)

proc expect*(expect: string): Combinator[string] =
  ## Expects a certain string
  runnableExamples:
    let g = expect("foo")
    assert g.match("foo") == some("foo")
    assert g.match("bar").isNone()

  return initCombinator(proc (): Explorer[Parser, string] =
    iterator (p: Parser): ParseResult[Parser, string] {.closure.} =
      if p.continuesWith(expect).safeGet(res):
        yield res
  )

proc expect*[T](values: HashSet[T]): Combinator[T] =
  ## Expects a single value from a set of values.
  ## This is a generic function that calls `expect` on each value
  let possible = collect:
    for value in values:
      expect(value)
  return any(possible)

proc just*[P, T](comb: BaseCombinator[P, T]): BaseCombinator[P, T] =
  ## Expects the combinator to fully match the input.
  runnableExamples:
    let g = just(e"hello")

    assert g.test("hello")
    assert not g.test("hello world")

  return initCombinator(proc (): Explorer[P, T] =
    iterator (p: P): ParseResult[P, T] {.closure.} =
      for res in comb.results(p):
        when P is Parser:
          if res.parser.len == 0: # No input left
            yield res
        else:
          yield res
  )

# You'll see functions like this that don't need to be functions.
# It helps with errors if the type system knows its a Combinator, compiler should inline it
proc fin*(): Combinator[Void] {.inline.} =
  ## Expects there to be no more data
  runnableExamples:
    let g = e"hello" * fin()

    assert g.test("hello")
    assert not g.test("hello world")

  return initCombinator(proc (): Explorer[Parser, Void] =
    iterator (p: Parser): ParseResult[Parser, Void] {.closure.}=
      if p.len == 0:
        yield (p, Void())
  )

proc map*[P, T, R](comb: BaseCombinator[P, T], op: proc (inp: T): R): BaseCombinator[P, R] =
  ## Allows you to perform an operator on a combinators output if it passes
  runnableExamples:
    import std/[sugar, strutils]

    let g = "hello".expect.map(toUpperAscii)
    assert g.match("hello").get() == "HELLO"

  return initCombinator(proc (): Explorer[P, R] =
    iterator (p: P): ParseResult[P, R] {.closure.} =
      for res in comb.results(p):
        yield (res.parser, op(res.value))
  )

proc `-`*[P](comb: BaseCombinator[P, auto]): BaseCombinator[P, Void] =
  ## Erases the type from a combinator
  return comb.map(it => Void())

proc `<*>`*[P, L, R](left: BaseCombinator[P, L], right: BaseCombinator[P, R]): BaseCombinator[P, tuple[left: L, right: R]] =
  ## Joins two combinators and returns a tuple of both parsed values.
  ## The [*] series of operators are more user friendly by flattening the returned values
  return initCombinator(proc (): Explorer[P, tuple[left: L, right: R]] =
    iterator (parser: P): ParseResult[P, tuple[left: L, right: R]] {.closure.} =
      for (newParser, leftValue) in left.results(parser):
        for (finalParser, rightValue) in right.results(newParser):
          yield (finalParser, (leftValue, rightValue))
  )

proc `<*`*[P, L, R](left: BaseCombinator[P, L], right: BaseCombinator[P, R]): BaseCombinator[P, L] =
  ## Joins two combinators but only retains the left value
  (left <*> right).map(it => it.left)


proc `*>`*[P, L, R](left: BaseCombinator[P, L], right: BaseCombinator[P, R]): BaseCombinator[P, R] =
  ## Joins two combinators but only retains the right value
  (left <*> right).map(it => it.right)

proc `*`*[P; A: tuple, B: tuple](left: BaseCombinator[P, A], right: BaseCombinator[P, B]): BaseCombinator[P, merge(A, B)] =
  ## Joins two combinators and merges the tuples together
  runnableExamples:
    let g = any(e"won", e"lost")$outcome * e" " * digit()$score

    let res = g.match("won 9").get()
    # The names were merged into a single tuple
    assert res.outcome == "won"
    assert res.score == 9

  (left <*> right).map(values => join(values.left, values.right, type(merge(A, B))))

proc `*`*[P; A: tuple, B: not tuple](left: BaseCombinator[P, A], right: BaseCombinator[P, B]): BaseCombinator[P, A] =
  ## Joins two combinators. Only returns the left combinator so named values are carried through
  left <* right

proc `*`*[P; A: not tuple, B: tuple](left: BaseCombinator[P, A], right: BaseCombinator[P, B]): BaseCombinator[P, B] =
  ## Joins two combinators.  Only returns the right combinator so named values are carried through
  left *> right

template `*`*[P, A, B](left: BaseCombinator[P, A], right: BaseCombinator[P, B]): BaseCombinator[P, Void] =
  ## Joins two combinators. Types are erased since we don't know what to do
  ## with them
  -(left <*> right)

proc `*`*[P](left: BaseCombinator[P, Void], right: BaseCombinator[P, Void]): BaseCombinator[P, Void] =
  ## Joins two combinators
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return -(left <*> right)

proc `*`*[P, T: not tuple](left: BaseCombinator[P, T], right: BaseCombinator[P, Void]): BaseCombinator[P, T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  left <* right

proc `*`*[P, T](left: BaseCombinator[P, Void], right: BaseCombinator[P, T]): BaseCombinator[P, T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  left *> right

proc `*`*[P, T](left: BaseCombinator[P, T], right: BaseCombinator[P, Chain[T]]): BaseCombinator[P, Chain[T]] =
  ## Joins two combinators, merging the results of both
  (left <*> right).map(values => values.left & values.right)

proc `*`*[P, T](left: BaseCombinator[P, Chain[T]], right: BaseCombinator[P, T]): BaseCombinator[P, Chain[T]] =
  ## Joins two combinators, merging the results of both
  (left <*> right).map(values => values.left & values.right)

proc any*[T: tuple](options: T): Combinator[mapAny(T)] =
  ## Named branch of what to expect
  type Ret = result.T
  return initCombinator(proc (): Explorer[Parser, Ret] =
    iterator (p: Parser): ParseResult[Parser, Ret] {.closure.} =
      for field, comb in options.fieldPairs:
        block:
          for path in comb.results(p):
            var ret = result.T(name: makeIdent(field))
            {.cast(uncheckedAssign).}:
              access(ret, field) = path.value
            yield (path.parser, ret)
  )

proc any*[P, T](options: varargs[BaseCombinator[P, T]]): BaseCombinator[P, T] =
  ## Passes if any of the combinators pass, this returns the value that passed
  runnableExamples:
    let g = any(e"yes", e"no")
    assert g.match("yes").get() == "yes"
    assert g.match("no").get() == "no"

  # Just implemented as the union of all possible values
  let opts = @options
  return initCombinator(proc (): Explorer[P, T] =
    iterator (p: P): ParseResult[P, T] {.closure.} =
      for combinator in opts:
        yieldfrom combinator.results(p)
  )

proc `|`*[P, T](left, right: BaseCombinator[P, T]): BaseCombinator[P, T] =
  ## This picks either left or right, returning the value that matches
  runnableExamples:
    let g = e"yes" | e"no"
    assert g.match("yes").get() == "yes"
    assert g.match("no").get() == "no"

  any(left, right)

proc `|`*[P, L, R](left: BaseCombinator[P, L], right: BaseCombinator[P, R]): BaseCombinator[P, Void] =
  ## This picks either left or right. Since they are different types, it erases the type
  runnableExamples:
    let g = digit() | e"hello" | e'L'
    assert g.test("hello")
    assert not g.test("a")

  any(-left, -right)

proc e*[T](val: T): Combinator[T] =
  ## Alias for [expect]
  expect(val)

proc e*[T](val: set[T]): Combinator[T] =
  ## Alias for [expect], expect for sets we expect the single match to return
  expect(val)

proc expect*[T: enum](e: typedesc[T]): Combinator[T] =
  ## Expects to read an enum. This is based on the stringified value of the enum
  runnableExamples:
    type
      Colours = enum
        Red = "rua"
        Green
        Blue = (4, "blu")
    let g = expect(Colours)
    assert g.match("rua").get() == Red
    assert g.match("Green").get() == Green
    assert g.match("blu").get() == Blue

  const possible = toHashSet do: collect:
    for val in fullset(e):
      $val
  expect(possible).map(parseEnum[T])

proc `not`*[P](comb: BaseCombinator[P, auto]): BaseCombinator[P, Void] =
  ## Expects a combinator to not match. This is a negative lookahead that doesn't consume
  ## any input
  runnableExamples:
    let g = not e"hello"
    assert not g.test("hello")
    assert g.test("goodbye")

  return initCombinator(proc (): Explorer[P, Void] =
    iterator (p: P): ParseResult[P, Void] {.closure.} =
      for item in comb.results(p):
        return # Something was found, means we don't match
      yield (p, Void())
  )

proc `*`*[P, T](comb: BaseCombinator[P, T]): BaseCombinator[P, Chain[T]] =
  ## Expects a combinator to match zero or more times. Returns all matches
  ## This is greedy and tries to match the most
  runnableExamples:
    let g = *e"hey"
    assert g.test("")
    assert g.test("heyhey")

  # The right recursion will make this find the longest match first
  (comb <*> lazy(() => *comb)).map(values => values.left & values.right) | succeed(default(Chain[T]))

proc `+`*[P, T](comb: BaseCombinator[P, T]): BaseCombinator[P, Chain[T]] =
  ## Expects a combinator to match 1 or more times. Returns all matches
  runnableExamples:
    let g = +e"hey"
    assert g.test("hey")
    assert not g.test("")

  comb * *comb

proc `?`*[P, T](comb: BaseCombinator[P, T]): BaseCombinator[P, Option[T]] =
  ## Optionally matches a combinator. Attempts to parse it first, but will continue without using input if fails
  runnableExamples:
    let g = ?e"hello"
    assert g.match("foo").get().isNone() # Still matches, but returns nothing
    assert g.match("hello").get() == some("hello")

  let wrapped = comb.map() do (inp: T) -> Option[T]: some(inp)
  any(wrapped, epsilon().map(it => none(T)))

proc sep*[P, T](comb: BaseCombinator[P, T], sep: BaseCombinator[P, auto]): BaseCombinator[P, seq[T]] =
  ## Matches a zero or more of `comb` that is separate by `sep`
  runnableExamples:
    let g = digit().sep(e", ")
    assert g.match("1, 2, 3").get() == @[1, 2, 3]

  # We need to convert it to a tuple and then unwrap it or else we lose the types
  *(comb <* ?sep)

proc listOf*[P, T](comb: BaseCombinator[P, T], sep: BaseCombinator[P, auto]): BaseCombinator[P, T] =
  ## Matches one or more of `comb` that is separate by `sep`
  return comb * comb.sep(sep)

type Reducer*[T, S] = proc (left: T, middle: S, right: T): T
  ## Operation that reduces left and right depending on the middle value

proc chainl*[P, T, S](comb: BaseCombinator[P, T], sep: BaseCombinator[P, S], combine: Reducer[T, S]): BaseCombinator[P, T] =
  ## Like [sep] or [listOf] except performs operations on the separaters. Use this when the separaters have
  ## some special meaning
  runnableExamples:
    let
      num = digit()
      op = any(e'+', e'*', e'-', e'/')
      expr = chainl(num, op) do (l: int, op: char, r: int) -> int:
        case op
        of '+': l + r
        of '*': l * r
        of '-': l - r
        of '/': int(l / r)
        else: raise (ref Exception)(msg: "How?")

    assert expr.match("1+2+3*4").get() == 24

  (comb <*> *(sep <*> comb)).map() do (matches: (T, seq[(S, T)])) -> T:
    let (elem, seps) = matches
    result = elem
    for (sep, item) in seps:
      result = combine(result, sep, item)

proc chainr*[P, T, S](comb: BaseCombinator[P, T], sep: BaseCombinator[P, S], combine: Reducer[T, S]): BaseCombinator[P, T] =
  ## Like [chainl] but applies operations from right to left
  runnableExamples:
    let
      num = digit()
      op = any(e'+', e'*', e'-', e'/')
      expr = chainr(num, op) do (l: int, op: char, r: int) -> int:
        case op
        of '+': l + r
        of '*': l * r
        of '-': l - r
        of '/': int(l / r)
        else: raise (ref Exception)(msg: "How?")

    assert expr.match("1+2+3*4").get() == 15

  (comb <*> ?(sep <*> lazy(() => chainr(comb, sep, combine)))).map() do (match: (T, Option[(S, T)])) -> T:
    let (elem, recurse) = match
    result = elem
    if recurse.isSome:
      let (sep, item) = recurse.get()
      result = combine(result, sep, item)

proc until*[P, T](comb: BaseCombinator[P, T], target: BaseCombinator[P, auto]): BaseCombinator[P, Chain[T]] =
  ## Parses `comb` until it encounters `target` (without consuming target)
  runnableExamples:
    let g = dot().until(e"world")
    assert g.match("helloworld").get() == "hello"
    # world isn't eaten, so we still need to consume it to continue
    assert (g * e"world").test("helloworld")

  *(not target * comb)

proc untilIncl*[P, T](comb: BaseCombinator[P, T], target: BaseCombinator[P, auto]): BaseCombinator[P, Chain[T]] =
  ## Parses `comb` until it encounters `target` (consumes target)
  runnableExamples:
    let g = dot().untilIncl(e"world")
    # world is eaten, but not parsed
    assert g.match("helloworld").get() == "hello"
    # world is eaten, so we don't need to get it after
    assert not (g * e"world").test("helloworld")

  comb.until(target) * -target

proc between*[P, T, L, R](comb: BaseCombinator[P, T], left: BaseCombinator[P, L], right: BaseCombinator[P, R]): BaseCombinator[P, T] =
  ## Checks that `comb` appears after `left` and before `right`
  runnableExamples:
    let g = digit().between(e'[', e']')
    assert g.match("[1]").get() == 1

  -left * comb * -right

proc occurs*[P, T](comb: BaseCombinator[P, T]): BaseCombinator[P, bool] =
  ## Returns true if `comb` occurs, false otherwise
  runnableExamples:
    let g = digit() * occurs(e';')$semicolon

    assert g.match("100;").get().semicolon
    assert not g.match("100").get().semicolon

  (?comb).map(it => it.isSome)

proc digit*(): Combinator[int] =
  ## Expects a digit
  runnableExamples:
    assert digit().match("123").get() == 123
    assert digit().match("-123").get() == -123

  (e('-').occurs() <*> +e({'0'..'9'})).map() do (values: (bool, string)) -> int:
    result = values[1].parseInt()
    if values[0]:
      result *= -1

proc map*[P, R](mapping: openArray[(BaseCombinator[P, Void], R)]): BaseCombinator[P, R] =
  ## Maps matching input values to output values
  runnableExamples:
    type
      Greeting = enum
        Hello
        Goodbye

    let g = map({
      -e("Hello World"): Hello,
      -e("Goodbye"): Goodbye
    })
    assert g.match("Hello World").get() == Hello
    assert g.match("Goodbye").get() == Goodbye

  let mapping = @mapping
  return any(mapping.mapIt(it[0] *> succeed(it[1])))
