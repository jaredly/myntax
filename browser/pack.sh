
# ./node_modules/bs-platform/lib/bsdep.exe -I src -one-line -pp 'node_modules/bs-platform/lib/refmt3.exe --print binary' -ml-synonym .re src/*.re

export PATH=$PATH:./node_modules/.bin
rm -rf _ml
mkdir _ml
for i in src/*.re;
do
base=`basename $i`
name=`echo $base | sed -e 's/\\.re/.ml/'`
bsrefmt $i --print ml | sed -e 's/\[\@explicit_arity \]//g' > _ml/$name
done