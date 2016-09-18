set -e

refmt -print binary $1 > out.bin
./pack.native pretty-bin parsable/so-far-re out.bin

