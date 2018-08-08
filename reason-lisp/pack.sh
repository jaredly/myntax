
export PATH=$PATH:./node_modules/.bin
rm -rf publish/_ml
mkdir publish/_ml
for i in src/*.re node_modules/myntax/src/*.re;
do
base=`basename $i`
name=`echo $base | sed -e 's/\\.re/.ml/'`
bsrefmt $i --print binary > publish/_ml/$name.ast
node_modules/myntax/lib/bs/native/ppx_release.native publish/_ml/$name.ast publish/_ml/$name.ast.ppx
bsrefmt publish/_ml/$name.ast.ppx --parse binary --print ml \
| node -e "process.stdout.write(require('fs').readFileSync('/dev/stdin').toString().replace(/\[\@explicit_arity[^\]]*\]/g, ''))" \
| node -e "process.stdout.write(require('fs').readFileSync('/dev/stdin').toString().replace(/\[\@\@ocaml.doc[^\]]*\]/g, ''))" \
 > publish/_ml/$name
done
bspack.exe publish/_ml/*.ml > publish/src/lisp.ml
