
bspack.exe _ml/*.ml > out.ml
./node_modules/bs-platform/vendor/ocaml/ocamlc.opt \
-I node_modules/bs-platform/vendor/ocaml/lib/ocaml/compiler-libs/ \
out.ml
