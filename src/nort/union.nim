## Basic union implementation
import std/macros

type
  UnionDiscriminator*[U] = object
    union*: U

proc branch*[U](union: U): UnionDiscriminator[U] =
  UnionDiscriminator[U](union: union)

macro `case`*(u: UnionDiscriminator): untyped =
  ## Needed to let users check the different cases

  # The issue is that the anonymous enum generated can't be exported so the user can't access the branches easily.
  # They instead need to grab the type of the discrimator which can be a hassle
  result = u
  let discrim = nnkDotExpr.newTree(nnkDotExpr.newTree(result[0], ident"union"), ident"name")
  result[0] = discrim

  for i in 1 ..< result.len:
    let branch = result[i]
    if branch.kind == nnkOfBranch:
      branch[0] = nnkDotExpr.newTree(newCall(ident"type", discrim), branch[0])
