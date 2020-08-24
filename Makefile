all: clean
	ocamlc -c Mainfile.ml
	ocamllex Real_lexer.mll
	ocamlyacc Real_parser.mly
	ocamlc -c Real_parser.mli
	ocamlc -c Real_lexer.ml
	ocamlc -c Real_parser.ml
	ocamlc -c main.ml
	ocamlc -o toyprolog Mainfile.cmo  Real_lexer.cmo Real_parser.cmo main.cmo 
	
clean:
	rm -rf toyprolog *.cmo *.cmi *.mli Real_lexer.ml Real_parser.ml 
