all: boxoffice

boxoffice:  query.ml io.ml main.ml 
	ocamlbuild -lib str main.d.byte
	cp main.d.byte boxoffice

check: boxoffice
	./boxoffice -echo < data/trial1.txt

topG:  boxoffice
	./boxoffice -sort-gross < data/G.txt | ./boxoffice -take 1

part1.d.native:  part1.ml
	ocamlbuild part1.native

clean:
	ocamlbuild -clean

test: build
	-./test.native
	rm test.native

build: clean
	ocamlbuild -use-ocamlfind -pkgs 'oUnit' test.native
