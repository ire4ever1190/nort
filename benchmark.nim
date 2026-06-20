import benchy
import nort
import nort/helpers
import std/[strutils, sugar]

timeIt "Parse a long string":
  let g = *e('a')
  doAssert g.match("a".repeat(8000)).isSome()

timeIt "Parse a long string in bigger chunks":
  let g = *e("aaaaaaaaaa")
  doAssert g.match("a".repeat(8000)).isSome()

timeIt "Parse a long string, ignoring everything":
  let g = *dot()
  doAssert g.match("a".repeat(8000)).isSome()

type
  ReportLevel = enum
    Hint
    Info
    Warning
    Error

let mismatchGrammar = block:
  let
    header = -e"Expression: " * -dot().untilIncl(nl)
    expectedHeader = e"Expected one of (first mismatch at [position]):"
    idx = between(digit()$position, e'[', e']')
    mismatch = idx * ws * dot().until(nl)$decl
    passedType: Combinator[string] = ws * -idx * -dot().untilIncl(e": ") * dot().untilIncl(nl)

  -dot().untilIncl(header) * (*passedType)$passedTypes * -dot().untilIncl(expectedHeader) * *(nl * mismatch)$mismatches

let errGrammar = block:
  let
    stacktraceHeader = e "stack trace: (most recent call last)\n"
    position = e('(') * digit()$line * e", " * digit()$column * e')'
    path = *(-(not (position | nl)) * dot())
    errorLevel = expect(ReportLevel)
    # Code name of the error/warning the compiler has internally e.g. [UndeclaredIdentifier].
    # This always appears at the end
    internalName = dot().until(e']').between(e'[', e']') * fin()
    instantiation = dot().until(nl)
    errorMsg = dot().until(internalName)$msg * ws * ?internalName$name
    errorLine = errorLevel$level * e": " * errorMsg
    msgLine = path$file * position * -(e' ') * any((error: errorLine, instantiation: instantiation * -nl))$info
  # We need to track if the first line indicates its a stacktrace, this lets us
  # catch if its a static exception
  (?stacktraceHeader).map(it => it.isSome())$isException * ws * msgLine

timeIt "Matching an actual grammar":
  const msg = """
  /tmp/test.nim(1, 17) Error: type mismatch
  Expression: "hello" * "world"
    [1] "hello": string
    [2] "world": string

  Expected one of (first mismatch at [position]):
  [1] func `*`[T](x, y: set[T]): set[T]
  [1] proc `*`(x, y: float): float
  [1] proc `*`(x, y: float32): float32
  [1] proc `*`(x, y: int): int
  [1] proc `*`(x, y: int16): int16
  [1] proc `*`(x, y: int32): int32
  [1] proc `*`(x, y: int64): int64
  [1] proc `*`(x, y: int8): int8
  [1] proc `*`(x, y: uint): uint
  [1] proc `*`(x, y: uint16): uint16
  [1] proc `*`(x, y: uint32): uint32
  [1] proc `*`(x, y: uint64): uint64
  [1] proc `*`(x, y: uint8): uint8"""
  doAssert errGrammar.match(msg).isSome()
