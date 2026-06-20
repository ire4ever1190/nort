# Nort

[docs](https://ire4ever1190.github.io/nort/stable/nort)

Parser combination library that is
- **Type Safe**: You know what the parser will return (And its not just strings)
- **Composable**: Build your patterns up instead of having one massive mess
- **Easy to Use**: Subjective, but I find this a lot easier than trying to explain regex

```nim
let greeting = any(
  e("hello"),
  e("goodbye")
).map(it => it == "hello") # Parsing can be refined on the go

# Now we know its one of the defined greetings, and we know
# if its a greeting or a goodbyte
echo greeting.match("hello").get()
```


### References

Things I read while developing this

- [Functional Parsers by Jeroen Fokker](http://cmsc-16100.cs.uchicago.edu/2017/Lectures/17/parsers.pdf)
