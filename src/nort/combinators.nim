import std/[options, sugar, parseutils, macros, sequtils, enumutils, strutils, setutils, sets]

export options

import ./[base, parser, utils, union]
export union, base, sets, options

type
  Chain*[T] = (when T is char: string else: seq[T])
    ## Represents chaining combinators together. Characters
    ## are joined into strings but other types just become `seq`

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

  return proc (p: var Parser): Option[T] =
    comb(p).filter(check)

proc dot*: Combinator[char] =
  ## Parses any character
  runnableExamples:
    assert dot().match("abc") == some('a')
  return proc (p: var Parser): Option[char] = p.eat()

proc expect*(expect: set[char]): Combinator[char] =
  ## Expects a set of characters, returns the matched value
  runnableExamples:
    let g = expect({'a', 'b', 'c'})
    assert g.match("a") == some('a')
    assert g.match("d").isNone()
  return filter(eat, it => it in expect)

proc expect*(input: char): Combinator[char] =
  ## Expects a character to appear
  runnableExamples:
    let g = expect('a')
    assert g.match("a") == some('a')
    assert g.match("b").isNone()

  return expect({input})

proc expect*(expect: string): Combinator[string] =
  ## Expects a certain string
  runnableExamples:
    let g = expect("foo")
    assert g.match("foo") == some("foo")
    assert g.match("bar").isNone()

  return proc (p: var Parser): Option[string] =
    p.continuesWith(expect)

proc expect*[T](values: HashSet[T]): Combinator[T] =
  ## Expects a single value from a set of values.
  ## This is a generic function that calls `expect` on each value
  let possible = collect:
    for value in values:
      expect(value)
  return any(possible)

proc digit*(): Combinator[int] =
  ## Expects a digit
  runnableExamples:
    assert digit().match("123").get() == 123
  return proc (p: var Parser): Option[int] =
    let init = p.pos
    var res: int
    p.pos += p.data.parseInt(res, start=init)

    # If the position progressed, then the parsing was a success
    if p.pos == init: none(int)
    else: some(res)

proc `-`*(comb: Combinator): Combinator[Void] =
  ## Erases the type from a combinator
  return proc (p: var Parser): Option[Void] =
    if comb(p).isNone(): none(Void)
    else: some(Void())

proc attempt*[T](p: var Parser, comb: Combinator[T]): Option[T] =
  ## Attempts to run a combinator. Resets the parser if it fails
  let init = p.pos
  result = comb(p)
  if result.isNone():
    p.pos = init

proc prec[L, R, T](p: var Parser, left: Combinator[L], right: Combinator[R], join: proc (l: L, r: R): T): Option[T] =
  ## Attempts to run `left`, if that successeds then it runs `right`.
  ## If both pass then it calls `join` to merge them
  let l = p.attempt(left)
  if l.isNone: return none(T)

  let r = p.attempt(right)
  if r.isNone: return none(T)

  return some(join(l.get(), r.get()))

proc `*`*[A: tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  ## Joins two combinators along with their outputs
  runnableExamples:
    let g = any(e"won", e"lost")$outcome * e" " * digit()$score

    let res = g.match("won 9").get()
    # The names were merged into a single tuple
    assert res.outcome == "won"
    assert res.score == 9

  return proc (p: var Parser): Option[merge(A, B)] =
    p.prec(left, right) do (l: A, r: B) -> merge(A, B):
      join(l, r, type(result))

proc `*`*[A: tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[A] =
  ## Joins two combinators
  return proc (p: var Parser): Option[A] =
    p.prec(left, right) do (l: A, r: B) -> A: l

proc `*`*[A: not tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[B] =
  ## Joins two combinators
  return proc (p: var Parser): Option[B] =
    p.prec(left, right) do (l: A, r: B) -> B: r

template `*`*[A, B](left: Combinator[A], right: Combinator[B]): Combinator[Void] =
  ## Joins two combinators. Types are erased since we don't know what to do
  ## with them
  proc (p: var Parser): Option[Void] =
    p.prec(left, right) do (l: left.T, r: right.T) -> Void: Void()

proc `*`*(left: Combinator[Void], right: Combinator[Void]): Combinator[Void] =
  ## Joins two combinators
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return proc (p: var Parser): Option[Void] =
    p.prec(left, right) do (l, r: Void) -> Void: Void()

proc `*`*[T: not tuple](left: Combinator[T], right: Combinator[Void]): Combinator[T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return proc (p: var Parser): Option[T] =
    p.prec(left, right) do (l: T, r: Void) -> T: l

proc `*`*[T](left: Combinator[Void], right: Combinator[T]): Combinator[T] =
  ## Carries a type through if the right side doesn't have one
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return proc (p: var Parser): Option[T] =
    p.prec(left, right) do (l: Void, r: T) -> T: r

proc `*`*[T](left: Combinator[T], right: Combinator[Chain[T]]): Combinator[Chain[T]] =
  ## Joins two combinators, merging the results of both
  return proc (p: var Parser): Option[Chain[T]] =
    p.prec(left, right) do (l: T, r: Chain[T]) -> Chain[T]: l & r

proc `*`*[T](left: Combinator[Chain[T]], right: Combinator[T]): Combinator[Chain[T]] =
  ## Joins two combinators, merging the results of both
  return proc (p: var Parser): Option[Chain[T]] =
    p.prec(left, right) do (l: Chain[T], r: T) -> Chain[T]: l & r

proc match*[T](comb: Combinator[T], data: string): Option[T] =
  ## Checks if a string matches a pattern. Returns the matched data
  var p = Parser(data: data)
  comb(p)

iterator match*[T](comb: Combinator[seq[T]], data: string): T =
  ## Returns each line that is matched
  runnableExamples:
    # Will echo 3 phrases
    var count = 0
    let g = *any(e"hi", e"bye")
    for line in g.match("hibyehi"):
      count += 1
      echo line
    assert count == 3

  var p = Parser(data: data)
  let ret: typeof(comb(p)) = comb(p)
  if ret.isSome():
    for data in ret.get():
      yield data

iterator match*[T](comb: Combinator[T], data: string): T =
  ## Returns zero or more matches of `comb` in data
  runnableExamples:
    # Will echo 3 phrases
    var count = 0
    let g = any(e"hi", e"bye")
    for line in g.match("hibyehi"):
      count += 1
      echo line
    assert count == 3

  var p = Parser(data: data)
  while true:
    let ret = comb(p)
    if ret.isNone: break
    yield ret.get()

proc test*[T](comb: Combinator[T], data: sink string): bool =
  ## Tests if an input matches a combinator
  runnableExamples:
    let g = e"hello"
    assert g.test("hello")
    assert not g.test("bye")

  var p = Parser(data: data)
  comb(p).isSome()

# You'll see functions like this that don't need to be functions.
# It helps with errors if the type system knows its a Combinator, compiler should inline it
proc fin*(): Combinator[Void] {.inline.} =
  ## Expects there to be no more data
  runnableExamples "-r:off": # Reenable after https://github.com/nim-lang/Nim/issues/25433
    let g = e"hello" * fin()

    assert g.test("hello")
    assert not g.test("hello world")

  return proc (p: var Parser): Option[Void] =
    if p.eof(): some(Void())
    else: none(Void)

proc any*[T: tuple](options: T): Combinator[mapAny(T)] =
  ## Named branch of what to expect
  return proc (p: var Parser): Option[result.T] =
    for field, comb in options.fieldPairs:
      block:
        let init = p.pos
        let res = comb(p)
        if res.isSome():
          var ret = result.T(name: makeIdent(field))
          {.cast(uncheckedAssign).}:
            access(ret, field) = res.get()
          return some(ret)
        else:
          p.pos = init

proc any*[T](options: varargs[Combinator[T]]): Combinator[T] =
  ## Passes if any of the combinators pass, this returns the value that passed
  runnableExamples:
    let g = any(e"yes", e"no")
    assert g.match("yes").get() == "yes"
    assert g.match("no").get() == "no"

  let opts = @options
  return proc (p: var Parser): Option[T] =
    for opt in opts:
      let res = p.attempt(opt)
      if res.isSome():
        return res

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

proc noop*[T](p: var Parser): Option[Option[T]] =
  ## Combinator that always matches. Since this version is typed,
  ## the data return is `none(T)` (but the parsing does pass)
  return some(none(T))

proc map*[T, R](comb: Combinator[T], op: proc (inp: T): R): Combinator[R] =
  ## Allows you to perform an operator on a combinators output if it passes
  runnableExamples:
    import std/[sugar, strutils]

    let g = "hello".expect.map(toUpperAscii)
    assert g.match("hello").get() == "HELLO"

  return proc (p: var Parser): Option[R] =
    comb(p).map(op)

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

proc error*(msg: string): Combinator[Void] =
  ## Throws an error, useful for debugging to see if
  ## the combinator hits something
  return proc (p: var Parser): Option[Void] =
    raise (ref CatchableError)(msg: msg)

proc `not`*(comb: Combinator): Combinator[Void] =
  ## Expects a combinator to not match. This is a negative lookahead that doesn't consume
  ## any input
  runnableExamples:
    let g = not e"hello"
    assert not g.test("hello")
    assert g.test("goodbye")

  return proc (p: var Parser): Option[Void] =
    let start = p.pos
    if p.attempt(comb).isSome():
      p.pos = start # Make sure we reset
      none(Void)
    else:
      some(Void())

proc `*`*[T](comb: Combinator[T]): Combinator[seq[T]] =
  ## Expects a combinator to match zero or more times. Returns all matches
  runnableExamples:
    let g = *e"hey"
    assert g.test("")
    assert g.test("heyhey")

  return proc (p: var Parser): Option[seq[T]] =
    var found: seq[T]
    while true:
      let res = p.attempt(comb)
      if res.isSome():
        found &= res.get()
      else:
        break
    return some(found)

proc `*`*(comb: Combinator[char]): Combinator[Chain[char]] =
  ## Optimised version that produces a string instead of a sequence of chars
  runnableExamples:
    let g = *e'a'
    assert g.match("aaaaa").get() == "aaaaa"
    assert g.match("").get() == ""

  return proc (p: var Parser): Option[string] =
    let start = p.pos
    while p.attempt(comb).isSome():
      discard

    # Copy it instead of joining each character
    some(p.data[start ..< p.pos])

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
  return any(wrapped, Combinator[Option[T]](noop[T]))

proc sep*[T](comb: Combinator[T], sep: Combinator): Combinator[seq[T]] =
  ## Matches a zero or more of `comb` that is separate by `sep`
  runnableExamples:
    let g = digit().sep(e", ")
    assert g.match("1, 2, 3").get() == @[1, 2, 3]

  # We need to convert it to a tuple and then unwrap it or else we lose the types
  return * (comb * -(?sep))

proc until*[T](comb: Combinator[T], target: Combinator): Combinator[Chain[T]] =
  ## Parses `comb` until it encounters `target` (without consuming target)
  *(not target * comb)

proc untilIncl*[T](comb: Combinator[T], target: Combinator): Combinator[Chain[T]] =
  ## Parses `comb` until it encounters `target` (consumes target)
  comb.until(target) * -target

proc between*[T, L, R](comb: Combinator[T], left: Combinator[L], right: Combinator[R]): Combinator[T] =
  ## Checks that `comb` appears after `left` and before `right`
  runnableExamples:
    let g = digit().between(e'[', e']')
    assert g.match("[1]").get() == 1

  -left * comb * -right
