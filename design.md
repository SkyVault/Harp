# Design of the Harp programming language

## Targets (javascript / lua / vm)

## Lisp like

## Final syntax

```harp typed

type point = Int & Int
type Player = { x Int, y Int }

let dict String & Int Dict = { "test": 100, "dict": 20 }
let list Int List = [1, 2, 3]

fun add (a Int, b Int) Int (a + b)

## weak types
fun make_pair 'A 'B (a 'A, b 'B) 'A & 'B (a & b)

let a Int = 32
let test Int = (a + 1)

each i in 0..100 (
    print(i + 100)
)

fun update_player (player Player) Player (
    let dt = get_frame_time(),
    let x = player.x + 100 * dt,
    { player | x = x } # Functional update
)

```

```harp untyped
type Player = { x, y }

let dict = { "test": 100, "dict": 20 }

fun add (a, b) (a + b)

let a = 32
let test = (a + 1)

each i in 0..100 (
    print(i + 100)
)

fun update_player (player) (
    let dt = get_frame_time(),
    let x = player.x + 100 * dt,
    { player | x = x } # Functional update
)
```
