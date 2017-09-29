all: expression mapreduce

expression: expression.d.byte 

mapreduce: mapreduce.d.byte


mapreduce.d.byte: mapreduce.ml
	ocamlbuild mapreduce.d.byte

LIB_FILES=ast.ml expressionLibrary.ml 
FILES=$(LIB_FILES) expression.ml

expression.d.byte: $(FILES)
	ocamlbuild expression.d.byte

clean: 
	ocamlbuild -clean

#rm -f expression mapreduce *.cmi *.cmo
