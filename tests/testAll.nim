import std/[unittest, strutils]

import nort
import nort/[base, helpers]

import ./utils

test "Can match a character":
  let g = dot()
  g.check {
    "a": 'a',
    "b": 'b',
    "cd": 'c'
  }

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

test "* matches in right order":
  let g = *dot() * e('d')
  g.check {
    "abcd": "abcd"
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

suite "ReDoS":
  # Series of grammars to test how we hold up against ReDoS attacks
  # Lazy evaluation should help with most
  test "Any amount of any amount of a's":
    # If we didn't have lazy evaluation, this would fail to parse
    let redos = +(+(e'a'))
    assert redos.match("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").isSome()

  test "Can parse long strings":
    let g = *e('a')
    assert g.match("a".repeat(2000)).isSome()

  test "Multiple long string matches":
    let g = *(dot() * nl)
    assert g.match((" ".repeat(100) & "\n").repeat(100)).isSome()

suite "Tuple type carrying":
  test "Two tuples copy fields":
    let g = e"hello"$left * e"world"$right
    assert g.T is tuple[left: string, right: string]

  test "Right tuple is carried over type":
    let g = e"hello" * e"world"$right
    assert g.T is tuple[right: string]

  test "Left tuple is carried over type":
    let g = e"hello"$left * e"world"
    assert g.T is tuple[left: string]

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
