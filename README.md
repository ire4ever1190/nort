# Nort

[docs](https://ire4ever1190.github.io/nort/develop/nort)

Parser combination library that is type safe, mainly just an experiment on if I could

```nim
let init = any(e("hello"), e("goodbye"))
let val = any((
  bar: init$saying * e(Whitespace) * e"world" * fin,
  foo: e"world"
)).match("goodbye world")

echo val.bar.saying #> "goodbye"
```


### References

Things I read while developing this

- [Functional Parsers by Jeroen Fokker](http://cmsc-16100.cs.uchicago.edu/2017/Lectures/17/parsers.pdf)
