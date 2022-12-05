---
layout: app
title: Linear feedback shift register
---

A LFSR can be used to compute a psueodo-random sequence of numbers.  For a given
width $N$, the sequence has length $2^N-1$, and repeats.

<br/>

If the circuit is built using XOR gates, the all zeros value is unreachable.  If built
using XNOR gates, the all ones value is unreachable.

<br/>

The generator supports widths from 2 to 168 bits.  A suitable set of taps is automatically
chosen.  A related counterpart set of taps may be selected.

<br/>

The LFSR architecture may be built using either Galois or Fibonacci form.  Generally
one prefers the former as it is very slightely more efficient.

<br/>

As can be seen in the simulations, the architecture, gate selection and counterpart tap
selection each lead to different sequences.

