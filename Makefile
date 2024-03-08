all: comp

# Compilation of Ocaml files
# Attention: order of object files important 
comp: lang.cmo parser.cmo lexer.cmo interf.cmo comp.cmo 
	ocamlfind ocamlc -linkpkg -package pprint -o comp $^

# Compilation of .ml files
lang.cmo: lang.ml
	ocamlc -c $<

interf.cmo: interf.ml parser.cmo lexer.cmo
	ocamlc -c $<

comp.cmo: comp.ml interf.cmo
	ocamlc -c $<

pprinter.cmo: pprinter.ml lang.cmo
	ocamlfind ocamlc -linkpkg lang.cmo -package pprint pprinter.ml

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $<
parser.cmo: parser.ml lang.cmo
	ocamlc -c $<


#### Generic rules

%.cmi: %.mli
	ocamlc -c $<


.PHONY: clean

clean: 
	rm -f *.mli *.cmi *.cmo
