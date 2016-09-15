
refmt:
	rebuild src/refmt.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules
	rebuild src/browserTypes.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

ocaml:
	rebuild src/run.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules
	rebuild src/browserTypes.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

web:
	./buckle.sh && webpack

regen-grammar: dump pack

dump:
	./pack.native dump parsable/grammar src/grammarGrammar.ml

pack:
	rebuild src/pack.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

.PHONY: ocaml buckle pack
