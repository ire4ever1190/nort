import std/[unittest, strutils]

import nort
import nort/base

import ./utils

test "Can match digit":
  let g = digit()
  g.check {
    "1": 1,
    "-1": -1,
    "00999": 999
  }
  g.check {
    "a": false,
  }

test "Can match string":
  let g = e"hello"
  g.check {
    "hello": "hello",
    "helloworld": "hello"
  }

test "Can parse until target":
  let g = dot().until(e"hello")
  g.check {
    "abcdhello": "abcd",
    "hello": "",
    "hell": "hell"
  }

test "Can match * that are turned into strings":
  let g = *e'c'
  g.check {
    "": "",
    "ccc": "ccc",
    "a": ""
  }

test "* doesn't eat too much":
  let g = (*e'a') * e'b'
  g.check {
    "b": true,
    "aaaab": true,
    "a": false
  }

test "Can match +":
  let g = +e'c'
  g.check {
    "ccc": "ccc",
    "c": "c"
  }
  g.check {
    "": false,
    "c": true
  }

test "Can chain Void":
  let g = +(-e"hello")
  g.check {
    "hellohello": true,
    "hello": true
  }

test "Chain non strings":
  let g = +(digit() * -e',')
  g.check {
    "1,2,3,4,": @[1, 2, 3, 4]
  }

test "fin matches end of string":
  let g = e"hello" * fin()
  g.check {
    "hello world": false,
    "hello": true
  }

test "Ambigious grammar works":
  let g = e('a') * *e({'a', 'b'}) * e("ba") * fin()
  g.check {
    "aba": true,
    "aaaabbbabababbababababa": true,
    "ba": false
  }


test "Can match union":
  let init: Combinator[Chain[char]] = any(e("hello"), e("goodbye"))
  let val = any((
    bar: init * e(Whitespace) * e"world" * fin(),
    foo: e"world"
  )).match("goodbye world")

  checkpoint $val

  require val.isSome()

  case val.get.branch
  of foo: discard
  of bar: discard

  discard val.get().bar

test "Negation matches":
  let g = not e"hello"
  g.check {
    "world": true,
    "hello": false
  }

test "Can join unrelated types":
  let g = e('(') * digit()
  g.check {
    "(123": true
  }

test "Trace prints result":
  # Smoke test to ensure the function compiles
  let g = digit().trace().between(e'[', e']')
  check g.match("[100]").isSome()

import nort/helpers
suite "Helpers":
  test "new lines":
    nl.check {
      "\r\n": true,
      "\n": true,
      "\r": false,
      "": false
    }
