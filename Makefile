all:
	ocamlbuild -use-ocamlfind -package ezcurl,qcheck,qcstm,yojson,ppx_deriving.show -I src machine.native

clean:
	ocamlbuild -clean