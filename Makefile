
refmt:
	rebuild src/refmt.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules
	rebuild src/browserTypes.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

test-pretty:
	./refmt.native pretty parsable/so-far-re parsable/basic-re

test-dump:
	./refmt.native dump parsable/so-far-re parsable/basic-re

test-dround:
	./refmt.native round-dump parsable/so-far-re parsable/basic-re

test-bin:
	./refmt.native bin parsable/so-far-re parsable/basic-re > out.bin
	../Reason/refmt_impl.native -parse binary -print re out.bin

test-round:
	./refmt.native round-pretty parsable/so-far-re parsable/basic-re

ocaml:
	rebuild src/run.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules
	rebuild src/browserTypes.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

web:
	./buckle.sh && webpack

pdump:
	./refmt.native pretty parsable/so-far-re src/pack.my src/pack.my

pback:
	./refmt.native bin parsable/so-far-re src/pack.my > out.bin
	../Reason/refmt_impl.native -parse binary -print re out.bin > src/pack.re

regen-grammar: dump pack

dump:
	./pack.native dump parsable/grammar src/grammarGrammar.ml

debug-regen:
	./pack.native dump parsable/grammar

pack:
	rebuild src/pack.native -use-ocamlfind -X ocaml -X react -X bs_build -X node_modules

.PHONY: ocaml buckle pack
