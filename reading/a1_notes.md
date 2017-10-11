# OCaml week 1: basic syntax and types

## What is OCaml

Typed functional langage

OCaml, like SML and Haskell, is an offshoot of Edinburgh ML.

Really similar to Haskell, but not lazy, and a little easier to escape-hatch into side effects

Most types are inferred, so it's statically typed, but not nearly as verbose as Java

Unline with TypeScript, the type system is sound - a well-typed program won't have type errors at runtime

# Types

The interesting parts are mostly in the types. This week starts getting into working with primitive types and basic data structures.

## Type checking

The type system catches errors

```ocaml
"hello" + 1;;
Error: This expression has type string but an
expression was expected of type int
```

## Unit type for side effects

- The unit or empty type is: `()` or `unit`.
- All expressions return something - for calls that just do side effects, it's `()`.
- To handle that, you can either pattern match the unit, or use a single `;`
- `()` is just a 0-length tuple

```ocaml
let () = print_string "hello" in
let _ = print_string "world" in

print_string "hello";
print_string "world";
```

## Type-Directed Programming

This is a technique the course reccomends:

1. Write down the name of the function and the types of its arguments and results.
1. Write a comment that explains its purpose and any preconditions.
1. Write several examples of what your function does.
1. Write the body of the function. (This is the hard part!)
1. Turn your examples into tests.

Seems a little verbose, but the point is to write down the types, and then "follow the types" to build the program.

## Generic Types

- OCaml has "parametric polymorphism"
- same deal as in haskell
- You can create functions that operate on many types of parameters
- Polymorphic types are typed as `'a`
- A generic function can operate over many types - but all the `'a`s have to be the same - passing in different types for x and y will cause an error
- when the compiler can't infer an exact type, it defaults to `'a`

```ocaml
# let first_if_true test x y =
    if test x then x else y
  ;;
val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun>
```

# Syntax

You can look up the syntax, but here are some highlights

## Calling functions

Like haskell and lisp, but unlike C-like languages, parentheses go around the outside of a calling function, and arguments dont have commas between them. Like haskell, you only use parentheses when nessecary to disambiguate, and otherwise avoid them.

Javascript:

```javascript
add(1, add(1, 2))
```

Lisp:

```lisp
(add 1 (add 1 2))
```

Ocaml & Haskell:

```ocaml
add 1 (add 1 2)
```

## Ints and Floats

- You can write numbers with underscores in them, like in Ruby: `1_000_000`
- Floats can be written with a trailing period: `2. == 2.0`
- Float math has different operators that int math: `2 * 2` and `2. *. 2.`
- To convert between float and int, use `float_of_int` and `int_of_float`

## Let binding

- in the repl, you bind a variable with `let x = 1;;`
- in code, you bind a variable with `let x = 1 in`

## Variables

- variable names have to start with an underscore or lowercase letter
- they can include letters, numbers, underscores, and `'`
- variable names are generally all snake_case
- Unlike in some functional languages, variables can be reassigned

```ocaml
let foo = 1 in
let foo = foo + 1 in
```

## Function definitions

### Normal function definiton

```ocaml
let function_name x y = x + y
```

Becasue we're using +, the compiler can infer the type of `x` and `y`

```ocaml
utop # function_name;;
- : int -> int -> int = <fun>
```

### Typed function

You can also provide types:

```ocaml
let function_name (x: int) (y: int): int = x + y
```

To let a function call itself recursivly, add `rec` to the let binding

```ocaml
let rec function_name (x: int) (y: int): int =
  (function_name x 1) + y
```

### Anonymous function

You can also define anonymous functions, to pass around

```ocaml
(fun x y -> x + y)
```

`let` function definitions are just syntactic sugar for anonymous functions:

```ocaml
let add = (fun x y -> x + y)
let add x y = x + y
```

### Curried

Like with Haskell, all OCaml functions are curried - so `add 1` returns a partially applied function awaiting a second number.

### Infix operators

- Infix operators like `+` are functions
- they can be used as prefix functions by wrapping them in `()`

```ocaml
(+) 1 2
```

- Unlike in haskell, it seems that you can't create arbitrary infix funtions. Instead, any function that has a name made up of the symbols: `! $ % & * + - . / : < = > ? @ ^ | ~`, or is a special reserved word like `mod`.
- Given that it's made up of symbols, you define an infix operator with parens in the let:

```ocaml
let (+++) x y = x + y
1 +++ 3
```

- The first couple character in a prefix operator determines its presedence in order of operations, with `!` the highest and `;` the lowest.

### Composition operators

It seems like pretty recently OCaml added some infix operators for function composition:

- `|>`: like in Elixir, this takes the previous value, and passes it to the next function. But unlike elixir, it goes to the last argument (which is better for currying)
- `@@`: Like the Haskell `.`, this is like `|>`, but the order is reversed

```ocaml
f (g (h 1))
h 1 |> g |> f
f @@ g @@ h @@ 1
```

## Data structures

### Tuple

An ordered collection of values that can each be of a different type.

- Surround with parentheses, seperate with commas
- The type is totally different, types seperated with `*`

__code__: `(3,"four",5.)`

__type__: `int * string * float`

### List

- Like most fp languages, OCaml uses linked lists instead of arrays
- Can only be a list of a single type
- written with square brackets and seperated with `;`

__code__: `[1; 2; 3]`

__type__: `list int`

The `List` module has lots of list functions, like `map`

```ocaml
# List.map languages ~f:String.length;;
- : int list = [5; 4; 1]
```

You can also add an item to a head of a list with `::`

```ocaml
[1; 2; 3] == 1 :: [2; 3] == 1 :: 2 :: 3
```

Concat (`@`), is slower, but works with two lists:

```ocaml
[1; 2; 3] @ [1; 2; 3]
```

### Options

- Similar to `Maybe` in haskell. But with `None` and `Some` instead of `Nothing` and `Just`
- Used to deal with a value that could be undefined
- typed as `int option`

```ocaml
let empty = None in
let full = Some 1 in
```

## Pattern matching

### Tuples

```ocaml
let (x,y) = a_tuple;;
val x : int = 3
val y : string = "three"
```

### Match

Match lets you pattern match a sequence of patterns. The first one that matches is run. This is similiar to `case` in elixir.

You list your patterns starting with a `|` before each one. The pipe before the first pattern is optional.

### Declaring Functions with Function

If you want to call `match` on a function argument, you can use the `function` keyword to do that:

```ocaml
let some_or_zero = function
  | Some x -> x
  | None -> 0
```

### Arrays

Test for the empty case, and pull off from the head:

```ocaml
let rec sum (numbers: list int): int =
  match numbers with
  | [] -> 0
  | hd :: tl -> hd + sum tl
```

### Options

You can pattern match options too

```ocaml
let log_entry maybe_time message =
  let time =
    match maybe_time with
    | Some x -> x
    | None -> Time.now ()
  in
  Time.to_sec_string time ^ " -- " ^ message
```

## Labeled arguments

- pass positonal arguments by tagging them with a `~`
- This seems to be common with functions like `map`

```ocaml
let ratio ~num ~denom = float num /. float denom;;
ratio ~num:3 ~denom:10;;
```
