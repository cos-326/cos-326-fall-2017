.PHONY: test clean build
all: test

clean:
	ocamlbuild -clean

test: build
	./test

build: clean
	ocamlfind ocamlopt -o test -package oUnit -linkpkg -g a1.ml test.ml
