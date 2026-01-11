import std/macros

macro access*(a: untyped, name: static[string]): untyped =
  return nnkDotExpr.newTree(a, ident(name))

macro makeIdent*(name: static[string]): untyped =
  return ident name

proc public*(ident: NimNode): NimNode =
  return nnkPostFix.newTree(ident"*", ident)

proc lookupType(x: NimNode): NimNode =
  case x.kind
  of nnkSym: x.getTypeImpl().lookupType()
  of nnkBracketExpr: x[1].lookupType()
  else: x

macro merge*(a: typedesc[tuple], b: typedesc[tuple]): typedesc =
  ## Merges two types into a tuple. If `a` is already a tuple then `b` is appended to it
  let
    a = a.lookupType()
    b = b.lookupType()

  # Both are tuples
  result = nnkTupleTy.newTree()
  for child in a:
    result.add(nnkIdentDefs.newTree(ident child[0].strVal, child[1], newEmptyNode()))

  for child in b:
    result.add(nnkIdentDefs.newTree(ident child[0].strVal, child[1], newEmptyNode()))

macro merge*(a: typedesc[not tuple], b: typedesc[tuple]): typedesc =
  ## Just returns `b` since we don't merge tuples with single types
  return b

macro merge*(a: typedesc[tuple], b: typedesc[not tuple]): typedesc =
  ## Just returns `a` since we don't merge tuples with single types
  return a
