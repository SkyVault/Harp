# Harp

The Harp Programming Language

```
let two := 2

let dictionary #[:hello "world" :another "value"]

fun double (val) { 
    val * two 
}

fun world () { dictionary.:hello }

fun helloWorld () {
    print("Hello " world() ": " double(32))
    print(dictionary.("another"))
}
```
