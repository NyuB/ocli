# OCaml CLI demo

+ Utility lib for tty control
    - Wrapper around OCaml Stdlib termios functions (**tty/tty.ml**)
    - Wrapper around ANSI terminal control sequences

+ Skeleton for a minimal interactive cli application (**bin/tty_demo.ml**)
    - Display, style, progress bar ... 
    - "Elm like" architecture with terminal lines as view

## Requirements

- Linux
- opam
- terminal with termios support

## Build

```bash
opam switch create .
dune build
```

## Run

```bash
dune exec tty_demo
```

## Run the test suite

```bash
dune test
```
