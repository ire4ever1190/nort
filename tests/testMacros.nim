import std/unittest
import nort/utils

suite "Merging tuples":
  type
    L = tuple[name: string]
    R = tuple[age: int]

  test "Type + tuple":
    check merge(int, R) is R

  test "Tuple + type":
    check merge(L, int) is L

  test "Tuple + tuple":
    check merge(L, R) is tuple[name: string, age: int]
