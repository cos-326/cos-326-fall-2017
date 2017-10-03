.PHONY: test clean build
all: test

clean:
	find . -type f \( -iname \*.o -o -iname \*.cmx \) | xargs rm

test: build
	./test

build: clean
	ocamlfind ocamlopt -o test -package oUnit -linkpkg -g a1.ml test.ml
