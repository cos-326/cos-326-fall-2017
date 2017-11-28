streams: streams.ml
	ocamlbuild -libs nums streams.d.byte

theory: theory.ml
	ocamlbuild theory.d.byte

FIB_FILES=memoizer.mli memoizer.ml timing.mli timing.ml fib.ml
LCS_FILES=base.mli base.ml memoizer.mli memoizer.ml timing.mli timing.ml lcs.ml

fib : $(FIB_FILES)
	ocamlbuild -libs unix fib.d.byte

lcs : $(LCS_FILES)
	ocamlbuild -libs unix lcs.d.byte

clean:
	ocamlbuild -clean

