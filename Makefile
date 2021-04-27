all:
	ocamlbuild -use-ocamlfind -tag thread -package cohttp,lwt,cohttp-async,async,ppx_deriving.show,ppx_let test_api.native