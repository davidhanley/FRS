
scoring : scoring.ml
	ocamlfind ocamlc -package eliom.server str.cma -thread scoring.ml

clean:
	rm *.cmi *.cmo
