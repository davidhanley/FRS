
INCLUDES = -I +unix -I +str
scoring : scoring.ml load.ml filters.ml
	ocamlfind ocamlc -g -I +str -package num -package unix str.cma nums.cma unix.cma load.ml filters.ml scoring.ml -o scoring
#	ocamlfind ocamlc -g -package ounit2 -package num str.cma nums.cma unix.cma scoring.ml test.ml -o test
#	./test

load.cmx: load.ml
	ocamlopt -c $(INCLUDES) load.ml

filters.cmx: filters.ml
	ocamlopt -c $(INCLUDES) filters.ml

scoring.cmx: scoring.ml
	ocamlopt -c $(INCLUDES) scoring.ml

scoringfast: load.cmx filters.cmx scoring.cmx
	ocamlopt $(INCLUDES) str.cmxa nums.cmxa unix.cmxa load.cmx filters.cmx scoring.cmx -o scoringfast

test : test.ml scoring.ml
	ocamlfind ocamlc -g -I +str -package ounit2 -package num str.cma nums.cma unix.cma load.ml filters.ml scoring.ml test.ml -o test

clean:
	rm -f *.cmi *.o *.cmo *.cmx test *.html scoring test scoringfast
