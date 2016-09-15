
ocaml:
	rebuild src/run.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules
	rebuild src/browserTypes.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

regen-grammar: dump pack

dump:
	./pack.native dump parsable/grammar src/grammarGrammar.ml

pack:
	rebuild src/pack.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

buckle:
	./buckle.sh
	# ../jenga/buckle/node_modules/.bin/bsc -I bs_build -pp refmt -impl src/packTypes.re -impl src/grammarGrammar.re -impl src/grammarOfGrammar.re -impl src/runtime.re -impl src/run.re

.PHONY: ocaml buckle pack
