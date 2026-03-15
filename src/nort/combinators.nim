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

proc filter*[T](comb: Combinator[T], check: proc (val: T): bool): Combinator[T] =
  ## Filter what values are allowed to consider that the filter has passed
  runnableExamples:
    import std/[strutils, sugar]
    let g = filter(*e({'a' .. 'z', ' '}), inp => inp.startsWith("hello"))
    assert g.test("hello world")
    assert not g.test("world")

  return proc (p: Parser): ParseTree[T] =
    comb(p).filter(res => check(res.value))

proc dot*(): Combinator[char] =
  ## Parses any character
  runnableExamples:
    assert dot().match("abc") == some('a')

  return proc (p: Parser): ParseTree[char] =
    p.eat().map(c => @[c]).get(@[])

proc succeed*[T](value: T): Combinator[T] =
  ## Combinator that always successeds and never comsumes input
  return proc (p: Parser): ParseTree[T] = @[(p, value)]

proc failure*(): Combinator[Void] =
  ## Combinator that matches nothing
  return proc (p: Parser): ParseTree[Void] = @[]

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

  return proc (p: Parser): ParseTree[string] =
    p.continuesWith(expect).map(val => @[val]).get(@[])

proc expect*[T](values: HashSet[T]): Combinator[T] =
  ## Expects a single value from a set of values.
  ## This is a generic function that calls `expect` on each value
  let possible = collect:
    for value in values:
      expect(value)
  return any(possible)

proc just*[T](comb: Combinator[T]): Combinator[T] =
  ## Expects the combinator to fully match the input.
  runnableExamples:
    let g = just(e"hello")

    assert g.test("hello")
    assert not g.test("hello world")

  return proc (p: Parser): ParseTree[T] =
    for res in comb(p):
      if res.parser.len == 0: # No input left
        result &= res

# You'll see functions like this that don't need to be functions.
# It helps with errors if the type system knows its a Combinator, compiler should inline it
proc fin*(): Combinator[Void] {.inline.} =
  ## Expects there to be no more data
  runnableExamples:
    let g = e"hello" * fin()

    assert g.test("hello")
    assert not g.test("hello world")

  return proc (p: Parser): ParseTree[Void] =
    if p.len == 0: @[(p, Void())]
    else: @[]

proc map*[T, R](comb: Combinator[T], op: proc (inp: T): R): Combinator[R] =
  ## Allows you to perform an operator on a combinators output if it passes
  runnableExamples:
    import std/[sugar, strutils]

    let g = "hello".expect.map(toUpperAscii)
    assert g.match("hello").get() == "HELLO"

  return proc (p: Parser): ParseTree[R] =
    comb(p).map(res => (res.parser, op(res.value)))

proc `-`*(comb: Combinator): Combinator[Void] =
  ## Erases the type from a combinator
  return comb.map(it => Void())

proc `<*>`*[L, R](left: Combinator[L], right: Combinator[R]): Combinator[tuple[left: L, right: R]] =
  ## Joins two combinators and returns a tuple of both parsed values.
  ## The [*] series of operators are more user friendly by flattening the returned values
  return proc (parser: Parser): ParseTree[tuple[left: L, right: R]] =
    for (newParser, leftValue) in left(parser):
      for (finalParser, rightValue) in right(newParser):
        result &= (finalParser, (leftValue, rightValue))

proc `<*`*[L, R](left: Combinator[L], right: Combinator[R]): Combinator[L] =
  ## Joins two combinators but only retains the left value
  (left <*> right).map(it => it.left)


proc `*>`*[L, R](left: Combinator[L], right: Combinator[R]): Combinator[R] =
  ## Joins two combinators but only retains the right value
  (left <*> right).map(it => it.right)

proc `*`*[A: tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  ## Joins two combinators and merges the tuples together
  runnableExamples:
    let g = any(e"won", e"lost")$outcome * e" " * digit()$score

    let res = g.match("won 9").get()
    # The names were merged into a single tuple
    assert res.outcome == "won"
    assert res.score == 9

  (left <*> right).map(values => join(values.left, values.right, type(merge(A, B))))

proc `*`*[A: tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[A] =
  ## Joins two combinators. Only returns the left combinator so named values are carried through
  left <* right

proc `*`*[A: not tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[B] =
  ## Joins two combinators.  Only returns the right combinator so named values are carried through
  left *> right

template `*`*[A, B](left: Combinator[A], right: Combinator[B]): Combinator[Void] =
  ## Joins two combinators. Types are erased since we don't know what to do
  ## with them
  -(left <*> right)

proc `*`*(left: Combinator[Void], right: Combinator[Void]): Combinator[Void] =
  ## Joins two combinators
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return -(left <*> right)

proc `*`*[T: not tuple](left: Combinator[T], right: Combinator[Void]): Combinator[T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  left <* right

proc `*`*[T](left: Combinator[Void], right: Combinator[T]): Combinator[T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  left *> right

proc `*`*[T](left: Combinator[T], right: Combinator[Chain[T]]): Combinator[Chain[T]] =
  ## Joins two combinators, merging the results of both
  (left <*> right).map(values => values.left & values.right)

proc `*`*[T](left: Combinator[Chain[T]], right: Combinator[T]): Combinator[Chain[T]] =
  ## Joins two combinators, merging the results of both
  (left <*> right).map(values => values.left & values.right)

proc any*[T: tuple](options: T): Combinator[mapAny(T)] =
  ## Named branch of what to expect
  return proc (p: Parser): ParseTree[result.T] =
    for field, comb in options.fieldPairs:
      block:
        let res = comb(p)
        for path in res:
          var ret = result.T(name: makeIdent(field))
          {.cast(uncheckedAssign).}:
            access(ret, field) = path.value
          result &= (path.parser, ret)

proc any*[T](options: varargs[Combinator[T]]): Combinator[T] =
  ## Passes if any of the combinators pass, this returns the value that passed
  runnableExamples:
    let g = any(e"yes", e"no")
    assert g.match("yes").get() == "yes"
    assert g.match("no").get() == "no"

  # Just implemented as the union of all possible values
  let opts = @options
  return proc (p: Parser): ParseTree[T] =
    for combinator in opts:
      result &= combinator(p)

proc `|`*[T](left, right: Combinator[T]): Combinator[T] =
  ## This picks either left or right, returning the value that matches
  runnableExamples:
    let g = e"yes" | e"no"
    assert g.match("yes").get() == "yes"
    assert g.match("no").get() == "no"

  any(left, right)

proc `|`*[L, R](left: Combinator[L], right: Combinator[R]): Combinator[Void] =
  ## This picks either left or right. Since they are different types, it erases the type
  runnableExamples:
    let g = digit() | e"hello" | e'L'
    assert g.test("hello")
    assert not g.test("a")

  any(-left, -right)

proc e*[T](val: T): Combinator[T] =
  ## Alias for [expect]
  return expect(val)

proc e*[T](val: set[T]): Combinator[T] =
  ## Alias for [expect], expect for sets we expect the single match to return
  return expect(val)

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
  return expect(possible).map(parseEnum[T])

proc `not`*(comb: Combinator): Combinator[Void] =
  ## Expects a combinator to not match. This is a negative lookahead that doesn't consume
  ## any input
  runnableExamples:
    let g = not e"hello"
    assert not g.test("hello")
    assert g.test("goodbye")

  return proc (p: Parser): ParseTree[Void] =
    let results = comb(p)
    if results.len > 0: return @[]
    else: return @[(p, Void())]

proc `*`*[T](comb: Combinator[T]): Combinator[Chain[T]] =
  ## Expects a combinator to match zero or more times. Returns all matches
  ## This is greedy and tries to match the most
  runnableExamples:
    let g = *e"hey"
    assert g.test("")
    assert g.test("heyhey")

  # The right recursion will make this find the longest match first
  (comb <*> lazy(() => *comb)).map(values => values.left & values.right) | succeed(default(Chain[T]))

proc `+`*[T](comb: Combinator[T]): Combinator[Chain[T]] =
  ## Expects a combinator to match 1 or more times. Returns all matches
  runnableExamples:
    let g = +e"hey"
    assert g.test("hey")
    assert not g.test("")

  return comb * *comb

proc `?`*[T](comb: Combinator[T]): Combinator[Option[T]] =
  ## Optionally matches a combinator. Attempts to parse it first, but will continue without using input if fails
  runnableExamples:
    let g = ?e"hello"
    assert g.match("foo").get().isNone() # Still matches, but returns nothing
    assert g.match("hello").get() == some("hello")

  let wrapped = comb.map() do (inp: T) -> Option[T]: some(inp)
  return any(wrapped, epsilon().map(it => none(T)))

proc sep*[T](comb: Combinator[T], sep: Combinator): Combinator[seq[T]] =
  ## Matches a zero or more of `comb` that is separate by `sep`
  runnableExamples:
    let g = digit().sep(e", ")
    assert g.match("1, 2, 3").get() == @[1, 2, 3]

  # We need to convert it to a tuple and then unwrap it or else we lose the types
  return * (comb * -(?sep))

proc listOf*[T](comb: Combinator[T], sep: Combinator): Combinator[T] =
  ## Matches one or more of `comb` that is separate by `sep`
  return comb * comb.sep(sep)

type Reducer*[T, S] = proc (left: T, middle: S, right: T): T
  ## Operation that reduces left and right depending on the middle value

proc chainl*[T, S](comb: Combinator[T], sep: Combinator[S], combine: Reducer[T, S]): Combinator[T] =
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

proc chainr*[T, S](comb: Combinator[T], sep: Combinator[S], combine: Reducer[T, S]): Combinator[T] =
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

proc until*[T](comb: Combinator[T], target: Combinator): Combinator[Chain[T]] =
  ## Parses `comb` until it encounters `target` (without consuming target)
  runnableExamples:
    let g = dot().until(e"world")
    assert g.match("helloworld").get() == "hello"
    # world isn't eaten, so we still need to consume it to continue
    assert (g * e"world").test("helloworld")

  *(not target * comb)

proc untilIncl*[T](comb: Combinator[T], target: Combinator): Combinator[Chain[T]] =
  ## Parses `comb` until it encounters `target` (consumes target)
  runnableExamples:
    let g = dot().untilIncl(e"world")
    # world is eaten, but not parsed
    assert g.match("helloworld").get() == "hello"
    # world is eaten, so we don't need to get it after
    assert not (g * e"world").test("helloworld")

  comb.until(target) * -target

proc between*[T, L, R](comb: Combinator[T], left: Combinator[L], right: Combinator[R]): Combinator[T] =
  ## Checks that `comb` appears after `left` and before `right`
  runnableExamples:
    let g = digit().between(e'[', e']')
    assert g.match("[1]").get() == 1

  -left * comb * -right

proc occurs*[T](comb: Combinator[T]): Combinator[bool] =
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

proc map*[R](mapping: openArray[(Combinator[Void], R)]): Combinator[R] =
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
  return proc (parser: Parser): ParseTree[R] =
    for (gram, ret) in mapping:
      for res in (gram *> succeed(ret))(parser):
        result &= res
