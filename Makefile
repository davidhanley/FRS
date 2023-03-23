
scoring : scoring.ml
	ocamlfind ocamlc -g -package num -package unix str.cma nums.cma unix.cma load.ml scoring.ml -o scoring
#	ocamlfind ocamlc -g -package ounit2 -package num str.cma nums.cma unix.cma scoring.ml test.ml -o test
#	./test

test : test.ml scoring.ml
	ocamlfind ocamlc -g -package ounit2 -package num str.cma nums.cma unix.cma load.ml scoring.ml test.ml -o test

clean:
	rm -f *.cmi *.cmo test *.html scoring test
