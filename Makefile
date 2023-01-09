
scoring : scoring.ml
	ocamlc -g str.cma scoring.ml -o scoring
	ocamlfind ocamlc -g -package ounit2 str.cma scoring.ml test.ml -o test
	./test

test : test.ml scoring.ml
	ocamlfind ocamlc -g -package ounit2 str.cma scoring.ml test.ml -o test

clean:
	rm *.cmi *.cmo test scoring
