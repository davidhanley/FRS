
scoring : scoring.ml
	ocamlfind ocamlc -g -I +str -package num -package unix str.cma nums.cma unix.cma load.ml filters.ml scoring.ml -o scoring
#	ocamlfind ocamlc -g -package ounit2 -package num str.cma nums.cma unix.cma scoring.ml test.ml -o test
#	./test

load.cmx: load.ml
	ocamlopt -c -I +unix -I +str load.ml

filters.cmx: filters.ml
	ocamlopt -c -I +unix -I +str filters.ml

scoring.cmx: scoring.ml
	ocamlopt -c -I +unix -I +str scoring.ml

scoringfast: load.cmx filters.cmx scoring.cmx
	ocamlopt -I +unix -I +str str.cmxa nums.cmxa unix.cmxa load.cmx filters.cmx scoring.cmx -o scoringfast

test : test.ml scoring.ml
	ocamlfind ocamlc -g -package ounit2 -package num str.cma nums.cma unix.cma load.ml scoring.ml test.ml -o test

clean:
	rm -f *.cmi *.cmo *.cmx test *.html scoring test scoringfast
