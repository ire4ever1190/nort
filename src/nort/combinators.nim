import std/[options, sugar, parseutils, macros, sequtils]

export options

import ./[base, parser, utils, union]
export union

#
# Internal functions
#

macro merge(a, b: typedesc): typedesc =
  ## Merges two types into a tuple. If `a` is already a tuple then `b` is appended to it
  let
    a = a.getTypeImpl()[1]
    b = b.getTypeImpl()[1]
  # Both are tuples
  result = nnkTupleTy.newTree()
  for child in a:
    result.add(nnkIdentDefs.newTree(ident child[0].strVal, child[1], newEmptyNode()))
  for child in b:
    result.add(nnkIdentDefs.newTree(ident child[0].strVal, child[1], newEmptyNode()))

macro mapAny(t: typedesc): typedesc =
  ## Maps a tuple type into a simple union
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

proc any*(p: var Parser): Option[char] =
  ## Parses any character
  runnableExamples:
    assert any.match("abc") == some('a')
  return p.eat()

proc expect*(expect: set[char]): Combinator[char] =
  ## Expects a set of characters, returns the matched value
  runnableExamples:
    let g = expect({'a', 'b', 'c'})
    assert g.match("a") == some('a')
    assert g.match("d").isNone()

  return proc (p: var Parser): Option[char] =
    p.eat().filter(it => it in expect)

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

proc digit*(p: var Parser): Option[int] =
  ## Expects a digit
  runnableExamples:
    assert digit.match("123").get() == 123

  let init = p.pos
  var res: int
  p.pos += p.data.parseInt(res, start=init)
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

proc `*`*[A: tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  ## Joins two combinators along with their outputs
  return proc (p: var Parser): Option[merge(A, B)] =
    let a = p.attempt(left)
    if a.isNone:
      return none(merge(A, B))

    let b = p.attempt(right)
    if b.isNone:
      return none(merge(A, B))
    return some(join(a.get(), b.get(), merge(A, B)))

proc `*`*[A: tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  ## Joins two combinators
  return proc (p: var Parser): Option[merge(A, B)] =
    let a = p.attempt(left)
    if a.isNone:
      return none(merge(A, B))

    let b = p.attempt(right)
    if b.isNone:
      return none(merge(A, B))
    return some(a.get())

proc `*`*[A: not tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[Void] =
  ## Joins two combinators
  runnableExamples:
    let g = e"hello" * e" " * e"world"
    assert g.test("hello world")

  return proc (p: var Parser): Option[Void] =
    let a = p.attempt(left)
    if a.isNone:
      return none(Void)

    let b = p.attempt(right)
    if b.isNone:
      return none(Void)
    return some(Void())


proc `*`*[T: seq](left, right: Combinator[T]): Combinator[T] =
  ## Joins two combinators, merging the results of both
  return proc (p: var Parser): Option[seq[T]] =
    let a = p.attempt(left)
    if a.isNone:
      return none(seq[T])

    let b = p.attempt(right)
    if b.isNone:
      return none(seq[T])
    return some(a.get() & b.get())

proc `*`*[T](left: Combinator[T], right: Combinator[seq[T]]): Combinator[seq[T]] =
  ## Joins two combinators, merging the results of both
  return proc (p: var Parser): Option[seq[T]] =
    let a = p.attempt(left)
    if a.isNone:
      return none(seq[T])

    let b = p.attempt(right)
    if b.isNone:
      return none(seq[T])

    return some(a.get() & b.get())

proc match*[T](comb: Combinator[T], data: string): Option[T] =
  ## Checks if a string matches a pattern. Returns the matched data
  var p = Parser(data: data)
  comb(p)

proc test*[T](comb: Combinator[T], data: sink string): bool =
  ## Tests if an input matches a combinator
  var p = Parser(data: data)
  comb(p).isSome()

proc fin*(p: var Parser): Option[Void] =
  ## Expects there to be no more data
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
  ## Anonymous branches
  let opts = @options
  return proc (p: var Parser): Option[T] =
    for opt in opts:
      let res = p.attempt(opt)
      if res.isSome():
        return res

proc e*[T](val: T): Combinator[T] =
  ## Alias for [expect]
  return expect(val)

proc e*[T](val: set[T]): Combinator[T] =
  ## Alias for [expect], expect for sets we expect the single match to return
  return expect(val)

proc error*(msg: string): Combinator[Void] =
  ## Throws an error, useful for debugging to see if
  ## the combinator hits something
  return proc (p: var Parser): Option[Void] =
    raise (ref CatchableError)(msg: msg)

proc `not`*(comb: Combinator): Combinator[Void] =
  ## Expects a combinator to not match
  return proc (p: var Parser): Option[Void] =
    if comb(p).isSome():
      none(Void)
    else:
      some(Void())

proc `*`*[T](comb: Combinator[T]): Combinator[seq[T]] =
  ## Expects a combinator to match zero or more times. Returns all matches
  runnableExamples:
    let g = *e'a'
    assert g.test("")
    assert g.test("aa")

  return proc (p: var Parser): Option[seq[T]] =
    var found: seq[T]
    while true:
      let res = p.attempt(comb)
      if res.isSome():
        found &= res.get()
      else:
        break
    return some(found)

proc `+`*[T](comb: Combinator[T]): Combinator[seq[T]] =
  ## Expects a combinator to match 1 or more times. Returns all matches
  runnableExamples:
    let g = +e'a' # one or more letter 'a'
    assert g.test("aaa")
    assert not g.test("")

  return comb * *comb

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

proc `?`*[T](comb: Combinator[T]): Combinator[Option[T]] =
  ## Optionally matches a combinator. Attempts to parse it first, but will continue without using input if fails
  runnableExamples:
    let g = ?e"hello"
    assert g.match("foo").get().isNone() # Still matches, but returns nothing
    assert g.match("hello").get() == some("hello")

  let wrapped = comb.map() do (inp: T) -> Option[T]: some(inp)
  return any(wrapped, Combinator[Option[T]](noop[T]))
