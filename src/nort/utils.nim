import std/macros

macro access*(a: untyped, name: static[string]): untyped =
  return nnkDotExpr.newTree(a, ident(name))

macro makeIdent*(name: static[string]): untyped =
  return ident name

proc public*(ident: NimNode): NimNode =
  return nnkPostFix.newTree(ident"*", ident)
