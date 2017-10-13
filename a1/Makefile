.PHONY: test clean build
all: test

clean:
	find . -type f \( -iname \*.cache -o -iname \*.log -o -iname \*.cm\* -o -iname \*.o \) | xargs rm

test: build
	./test

build: clean
	ocamlfind ocamlopt -o test -package oUnit -linkpkg -g a1.ml test.ml
