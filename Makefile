scoring : scoring.ml
	 ocamlc str.cma scoring.ml -o scoring

clean:
	rm *.cmi *.cmo
