
scoring : scoring.ml
	ocamlc str.cma scoring.ml -o scoring

test : test.ml scoring.ml
	ocamlfind ocamlc -package ounit2 str.cma scoring.ml test.ml -o test

clean:
	rm *.cmi *.cmo
