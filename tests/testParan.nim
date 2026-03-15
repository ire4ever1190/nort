## One of the examples from the paper. Whether we can parse paranthesis
## This tests recursive parsers

import nort
import std/[sugar, unittest]
import ./utils

type
  Tree = ref object
    case hasVal: bool
    of true: limbs: tuple[left: Tree, right: Tree]
    of false: discard

func tree(inp: tuple[left: Tree, right: Tree]): Tree =
  Tree(hasVal: true, limbs: inp)

func tree(): Tree = Tree(hasVal: false)

proc `$`(x: Tree): string =
  if x.hasVal:
    result = "(" & $x.limbs.left & ")" & $x.limbs.right
  else:
    result = ""

let
  open = e'('
  close = e')'

proc paran(): Combinator[Tree] =
  ((open *> lazy(paran) <* close) <*> lazy(paran)).map(tree) |
  succeed(tree())

test "Paranthesis example":
  paran().just().check {
    "": true,
    "()": true,
    "()()": true,
    "(())": true,
    "(()": false
  }
