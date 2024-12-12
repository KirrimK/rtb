# rtb

Rémy's Tiny Basic interpreter
Copyright (C) 2024 Rémy B.
[License MIT](https://github.com/KirrimK/rtb/blob/main/LICENSE)

A toy project about implementing a Tiny Basic interpreter (and later a compiler if i care enough) based off of my sloppy interpretation of the [wikipedia page](https://en.wikipedia.org/wiki/Tiny_BASIC).

There's a REPL where you can write code "interactively".

I tried to make the code clean, but for the README and the docs i gave up so here you go:

## things you can do with this

```
# print values to stdout
print <one or more things>

# add a marker to a line
<integer line marker> print "here"

# basic oneline if
if <compare two things> then <do something>

# goto
goto <integer line marker>

# input one or more variables (space separated at input)

# here input the whole line in variable a
input a

# here input the first word into a and the rest into b
input a, b

# declare variable
let bruh = 7

# goto a specific line in subroutine mode (the current line gets added to the return stack)
gosub <integer line marker>

# return from the subroutine
return

# REPL: clear all the code previously entered
clear

# REPL: list all the code lines ready to be run
list

# REPL: run the program written
run

# quit everything
end
```

## how to build this thing

On linux (no windows blame OCaml), use `opam` to install `dune`, `menhir` and `dune-build-info`, then run `dune build` to build.

To run, you can use `dune exec bin/main.exe` to launch the REPL.
If you can to run a program written in a separate file run `dune exec bin/main.exe -- -c <your_file>`.


## Things that are left to do

- proper docs
- standard lib (so that it can do a bit more things)
- the compiler (to what target format i don't know yet)
- proper testing
