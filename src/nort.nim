# This is just an example to get you started. A typical library package
# exports the main API in this file. Note that you cannot rename this file
# but you can remove it if you wish.

import std/[options, sugar, parseutils, macros, sequtils]

macro peg(body: untyped) = discard

type
  Parser = object
    data: string
    pos: int

  Void = object

  Combinator[T] = proc (p: var Parser): Option[T] {.closure.}
    ## Parser that optionally returns data

func eof(p: Parser): bool =
  return p.pos >= p.data.len

func peek(p: Parser): Option[char] =
  if p.eof: none(char)
  else: some p.data[p.pos]

func eat(p: var Parser): Option[char] =
  result = p.peek()
  p.pos += 1

func continuesWith(p: var Parser, token: string): Option[string] =
  let init = p.pos
  p.pos += p.data.skip(token, start = p.pos)
  if p.pos == init: none(string)
  else: some(token)

# Combinators

proc any(p: var Parser): Option[char] =
  return p.eat()

proc expect(expect: char): Combinator[char] =
  return proc (p: var Parser): Option[char] =
    p.eat().filter(it => it == expect)

proc expect(expect: string): Combinator[string] =
  return proc (p: var Parser): Option[string] =
    p.continuesWith(expect)

proc digit(p: var Parser): Option[int] =
  let init = p.pos
  var res: int
  p.pos += p.data.parseInt(res, start=init)
  if p.pos == init: none(int)
  else: some(res)

proc `-`(comb: Combinator): Combinator[Void] =
  return proc (p: var Parser): Option[Void] =
    if comb(p).isNone(): none(Void)
    else: some(Void())

proc bindTo[T; R: tuple](comb: Combinator[T]): Combinator[R] =
  return proc (p: var Parser): Option[(T,)] =
    let val = comb(p)
    if val.isSome():
      return some((val.get(),))

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

template `$`[T](comb: Combinator[T], name: untyped): untyped =
  bindTo[T, tuple[name: T]](comb)

macro access(a: untyped, name: static[string]): untyped =
  return nnkDotExpr.newTree(a, ident(name))

macro makeIdent(name: static[string]): untyped =
  return ident name

macro mapAny(t: typedesc): typedesc =
  ## Maps a tuple type into a simple union
  result = nnkTupleTy.newTree()
  var branches: seq[(string, NimNode)]
  for def in t.getTypeImpl()[1]:
    echo def.treeRepr
    branches &= (def[0].strVal, nnkDotExpr.newTree(def[1], ident"T"))
  # Create a enum for all the branches
  let
    enumName = genSym(nskType, "descriminator")
    enumTyp = nnkEnumTy.newTree(newEmptyNode()).add(branches.map(b => ident b[0]))
    typName = genSym(nskType, "union")
    unionObj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(),
      nnkRecList.newTree(nnkRecCase.newTree(newIdentdefs(ident"name", enumName))
      .add(branches.map(b => nnkOfBranch.newTree(ident b[0], nnkRecList.newTree(newIdentdefs(ident b[0], b[1]))))))
    )
  result = newStmtList(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(enumName, newEmptyNode(), enumTyp),
      nnkTypeDef.newTree(typName, newEmptyNode(), unionObj)
    ),
    typName
  )

proc join[R](a: tuple, b: tuple, ret: typedesc[R]): R =
  for name, val in a.fieldPairs:
    access(result, name) = val
  for name, val in b.fieldPairs:
    access(result, name) = val

proc `*`[A: tuple, B: tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  return proc (p: var Parser): Option[merge(A, B)] =
    let start = p.pos
    let a = left(p)
    if a.isNone:
      p.pos = start
      return none(merge(A, B))

    let b = right(p)
    if b.isNone:
      p.pos = start
      return none(merge(A, B))
    return some(join(a.get(), b.get(), merge(A, B)))

proc `*`[A: tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[merge(A, B)] =
  return proc (p: var Parser): Option[merge(A, B)] =
    let start = p.pos
    let a = left(p)
    if a.isNone:
      p.pos = start
      return none(merge(A, B))

    let b = right(p)
    if b.isNone:
      p.pos = start
      return none(merge(A, B))
    return some(a.get())

proc `*`[A: not tuple, B: not tuple](left: Combinator[A], right: Combinator[B]): Combinator[Void] =
  return proc (p: var Parser): Option[Void] =
    let start = p.pos
    let a = left(p)
    if a.isNone:
      p.pos = start
      return none(Void)

    let b = right(p)
    if b.isNone:
      p.pos = start
      return none(Void)
    return some(Void())


proc match[T](comb: Combinator[T], data: string): Option[T] =
  var p = Parser(data: data)
  comb(p)

proc any[T: tuple](options: T): Combinator[mapAny(T)] =
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

echo any((bar: expect("hello"), foo: expect(" world"))).match("hello world")
