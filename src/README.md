# Internals of ocaml_of_michelson

## How to extend the translator

Most instructions follow the same pattern: Remove a few elements from
the operand stack, do some computation, puts new elements back on the
stack.  The correponding OCaml code would be something like `let x1,
..., xn = f y1 ... ym in ...`, where `f` is the name of the function
corresponding to the instruction.

To make the translator easily extensible with new instructions,
the translator uses `test/inst.mli`, which declares, for example,

```
add : int -> int -> int
```

which the translator interprets as a counterpart of the instruction
`ADD`, which consumes two values and produces one value.  The
translator reads off only the arity of the function and ignores
detailed type information, which is used only when the translated code
is compiled against `inst.mli`.  If the name of the instruction is an
OCaml keyword (after making it lowercase), add `_` to the function name.
(See `and_`, for example.)  If the instruction puts more than one value
on the stack, use a tuple type (see `unpair`, for example).

The translator reads `inst.mli` every time.  So, there is no need to
rebuild it after adding function declarations.

## Basic translation scheme

* `Trans.exp_of_prog` is the main translating function, which takes
    * `kont`, which represents the translation of preceding instructions as an OCaml function.
    * a list of instructions, and
    * a list of identifiers (strings), which represents the shape of the stack
  and returns
    * the translated code (as a function) and
    * a list (`Some [x1; ...; xn]`) of identifiers, which represents the shape of the stack at the end of the current block, or `None` to signal that the block ends with `FAILWITH`.

* Each stack element is given names, which will be variables in OCaml code.
  Names may be shared by different elements.

* OCaml code is built by using compiler-libs.

* Inline expansion is implemented also by using compiler-libs
  (more specifically, `Ast_mapper`, typically used for PPX extensions).

## When control flows merge
