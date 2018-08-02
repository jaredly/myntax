#!/usr/bin/env bash
rm -rf bs_build
mkdir bs_build
cp src/*.re bs_build/
cp src/*.ml bs_build/
rm bs_build/pack.re bs_build/run.re bs_build/ocamlOfReason.re bs_build/refmt.re
mv bs_build/json_bs.re bs_build/json.re
mv bs_build/sysop_bs.re bs_build/sysop.re
SOURCES=`ocamldep -pp refmt -ml-synonym .re -sort -I bs_build bs_build/*.re bs_build/*.ml`
echo ${SOURCES}
SOURCES=`echo ${SOURCES}|sed -e 's/ / -impl /g'`
echo ${SOURCES}
./node_modules/.bin/bsc -I bs_build -pp refmt -impl ${SOURCES}
