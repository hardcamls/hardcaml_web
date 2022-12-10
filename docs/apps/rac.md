---
layout: app
title: Rom Accumulator
hardcaml_app: /apps/rac.bc.js
---

The distributed arithmetic ROM accumulator is a bit serial, area efficient way of
computing the following function, given a small number of static coefficients.

$$∑↙{i=0}↖{n-1} c_{i}.x_{i}$$

The basic idea is an extension of multiplying by iterating over the bits of one
of the operands.  For example, given $a$ and $b=13=1101$ we can multiply
by adding and shifting at the locations of $b$ which are one.

$$a * b = a + 2^2a + 2^3a$$

 This can also be done in a bitserial fashion starting at the most significant
bit of $b$ and multiplying the result by 2 on each iteration.

The rom-accumulator extends this idea by taking 1 bit from multiple input values per
iteration and using them to address into a ROM formed from the coefficients as
follows.

| input bits| rom value           |
|-----------|---------------------|
| 000       | $0$                 |
| 001       | $c_{0}$             |
| 010       | $c_{1}$             |
| 011       | $c_{1}+c_{0}$       |
| 100       | $c_{2}$             |
| 101       | $c_{2}+c_{0}$       |
| 110       | $c_{2}+c_{1}$       |
| 111       | $c_{2}+c_{1}+c_{0}$ |

The value from the ROM is added to the accumulated result.

By starting at the msb and shifting up each iteration we are able to accumulate an
exact result.  It is also possible start at the bottom and shift downwards
instead - usually keeping some extra fractional precision within the accumulator.
This is useful when working on approximations of real numbers and is demonstrated
in the simulation below.
