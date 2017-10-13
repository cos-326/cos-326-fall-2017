.PHONY: test clean build
all: test

clean:
	ocamlbuild -clean

test: build
	-./test.d.byte
	rm test.d.byte

build: clean
	ocamlbuild -use-ocamlfind -pkgs 'oUnit' test.d.byte
