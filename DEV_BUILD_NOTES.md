We (currently) need to build this repo using some patches to the open 
source releases of the hardcaml libraries.  Below are the requirements.

Get a working opam environment

```
opam install hardcaml_waveterm js_of_ocaml zarith_js_stubs
```

Grab the dev version of brr

```
opam pin brr --dev
```

Clone the following set of repositories into a fresh directory.

```
git clone https://github.com/hardcamls/hardcaml -b jsoo
git clone https://github.com/hardcamls/hardcaml_circuits -b jsoo
git clone https://github.com/hardcamls/hardcaml_waveterm -b jsoo
git clone https://github.com/hardcamls/hardcaml_verilator -b jsoo
git clone https://github.com/hardcamls/reedsolomon
git clone https://github.com/janestreet/ppx_deriving_hardcaml
git clone https://github.com/janestreet/hardcaml_step_testbench
git clone https://github.com/janestreet/hardcaml_verify
git clone https://github.com/hardcamls/hardcaml_web
```

In the directory where all these repos exist, create a dune-project file as follows:

```
$ cat dune-project
(lang dune 2.4)
```

We are starting to work on the hardcaml\_zprize submission.  Note that the repo is currently private, so ask billy345 for access.

```
git clone https://github.com/janestreet/hardcaml_xilinx
git clone https://github.com/fyquah/hardcaml_zprize -b port-to-latest-hardcaml
```
