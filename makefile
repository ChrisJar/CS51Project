all: miniml evaluation expr testing

miniml: miniml.ml
	ocamlbuild miniml.byte

evaluation: evaluation.ml
	ocamlbuild evaluation.ml

expr: expr.ml
	ocamlbuild expr.byte

testing: testing.ml
	ocamlbuild testing.byte

clean:
	rm -rf _build *.byte