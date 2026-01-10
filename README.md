# Nort

[docs](https://ire4ever1190.github.io/nort/develop)

Parser combination library that is type safe, mainly just an experiment on if I could

```nim
let init = any(e("hello"), e("goodbye"))
let val = any((
  bar: init$saying * e(Whitespace) * e"world" * fin,
  foo: e"world"
)).match("goodbye world")

echo val.bar.saying #> "goodbye"
```
