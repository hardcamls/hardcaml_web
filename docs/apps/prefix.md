---
layout: app
title: Parallel prefix adder
hardcaml_app: /apps/prefix.bc.js
---

A parallel prefix adder utilises a parallel prefix network to perform binary
addition with different tradeoffs regarding delay, area and fanout.

| network     | area   | delay   | fanout |
|-------------|--------|---------|--------|
| serial      | low    | high    | low    |
| sklansky    | medium | low     | high   |
| brent-kung  | low    | medium  | low    |
| kogge-stone | high   | low     | low    |
