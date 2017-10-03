#!/bin/bash

if ! [ -x "$(command -v brew)" ]; then
    echo 'Error: Homebrew is not installed.' >&2
    exit 1
fi

brew install ocaml opam

opam switch 4.05.0

eval `opam config env`

opam install merlin
opam install ounit
opam install utop

echo 'WARNING: You must run this command before interacting with OCaml in this shell:'
echo ''
echo 'eval `opam config env`'
echo ''
