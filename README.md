# ocaml_of_michelson
Tiny translator from Michelson to OCaml

## Requirement

* OCaml (>=4.11.0)
* menhir
* ocamlfind

## Build

```
% dune build
```

will build `ocaml_of_michelson` in `src`.

## Usage

```
ocaml_of_michelson [-spec spec_file] [-o output_file] [michelson_file]
```

It reads a Michelson script from `michelson_file` (or stdin if not specified).
The output is written to `output_file` (or stdout if `-o` is omitted); debug messages are also written to stderr.

The option `-spec` (default `./inst.mli`) specifies an OCaml interface file that describes the OCaml types of Michelson instructions.  The output file should typecheck with it.

## Test

```
% dune test
```

All `.tz` files are translated into `.ml`, which is typedchecked with `ocamlopt`.
The interface file `test/inst.mli` lists the types of instructions.

## Bugs

* The generated program is _not_ supposed to run :sweat_drops:.  (This is a *FEATURE*!)

* Input checking is loose.  It assumes the code section is well typed.

* The produced ocaml code should typecheck but we make loose assumptions about the types of instructions
(e.g., `string` and `address` are identified, `compare` can be applied to any type, and so on).
So, a potential bug of the translator may be overlooked.

* Some overloaded instructions (at least `MAP`) can't be handled properly even with loose checking.

* `GET n` and `UPDATE n` assumes `n` is less than 7.  (See `test/inst.mli` to see why.)

* Macros are not supported yet.

* Some forms for compound literals are not supported:
    * `{1; "foo"; True}` for tuples.
    * `{}` for the empty map (it is always a list).
    * lambdas.
