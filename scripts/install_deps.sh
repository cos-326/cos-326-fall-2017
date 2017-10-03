#!/bin/bash

opam switch 4.05.0

eval `opam config env`

opam install merlin
opam install ounit

echo 'WARNING: You must run this command before interacting with OCaml in this shell:'
echo ''
echo 'eval `opam config env`'
echo ''
