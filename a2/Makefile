all: boxoffice

boxoffice:  query.ml io.ml main.ml 
	ocamlbuild -lib str main.d.byte
	cp main.d.byte boxoffice

check: boxoffice
	./boxoffice -echo < data/trial1.txt

topG:  boxoffice
	./boxoffice -sort-gross < data/G.txt | ./boxoffice -take 1

part1.d.byte:  part1.ml
	ocamlbuild part1.d.byte

clean:
	ocamlbuild -clean

test: build
	./test

build: clean
	ocamlfind ocamlopt -o test -package oUnit -linkpkg -g part1.ml query.ml test.ml