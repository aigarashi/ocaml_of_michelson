# ocaml_of_michelson
Tiny translator from Michelson to OCaml

## Requirement

* OCaml (>=4.11.0)
* menhir
* ocamlfind

## Build

```
% cd src
% make depend
% make
```

will build `ocaml_of_michelson` in `src`.

## Usage

```
ocaml_of_michelson
```
(There is no available option.)

It reads a Michelson script from stdin.
The output is written to stdout and debug messages are also written to stderr.
So, typical usage would be:

```
ocaml_of_michelson < foo.tz > foo.ml
```

The output file should typecheck with `test/inst.mli`.  (There is no
implementation for `inst.mli`, so the generated code cannot run.)

## Test

```
% cd test
% make
```

All `.tz` files are translated into `.ml`, which is typedchecked with `ocamlopt`.
The interface file `test/inst.mli` lists the types of instructions.

## Bugs

* Input checking is loose.  It assumes the code section is well typed.

* The produced ocaml code should typecheck but we make loose assumptions about the types of instructions
(e.g., `string` and `address` are identified, `compare` can be applied to any type, and so on).
So, a potential bug of the translator may be overlooked.
