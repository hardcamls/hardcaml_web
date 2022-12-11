---
title: naming
layout: default
category: getting-started
---
# Naming

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Names pop up in a couple of places. Input and output ports of a
circuit have names. Also, all internal nodes within a logic design
need to have some sort of name.

Judicious use of names becomes very important for debugging
simulations or for understanding reports when using vendor tools like
Vivado.

# Port names

Port names are specified with the `input` and `output` functions.
There are some rules which are checked by Hardcaml.

- Port names must be unique.  No input or output may share a name.
- The names must be legal for the language they are used in. For
  Hardcaml simulation this doesn't matter so much, but if we write the
  design to Verilog the names must not clash with a Verilog reserved
  word.
- Hardcaml will never try to alter a port name. It will simply raise
  if it deems it illegal.

# Internal names

When we create vectors in Hardcaml they are labelled with a unique ID.
Without further information hardcaml will implicitly name the vector
as `_<uid>`. It is possible to manually label any vector with a new
name using the `(--)` operator.

```ocaml
# let foo = Hardcaml.Signal.(of_int ~width:8 7 -- "foo")
val foo : Hardcaml.Signal.t =
  (const (names (foo)) (width 8) (value 0b00000111))
```

There are really no rules as to what internal names it is OK to use. Hardcaml
will happily rewrite the given internal names to make them legal.

- Any non-alphanumeric character (or '$') is rewritten with an `_`.
- Internal names cannot start with a number - a prefix is added.
- Internal names cannot be reserved words - the name gets mangled.
- Internal names need to be unique - the name get mangled if not.

Mangling means adding a numeric suffix and checking against the rules
again.
