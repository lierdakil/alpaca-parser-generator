#!/bin/bash

#export alpaca=$(cabal exec -- which alpaca)
export alpaca=$(which alpaca)

what="$@"

[ -z "$what" ] && what="cpp csharp py nodejs"

definput="$(echo -e "+ 1 * 2 ^\t3 4")"
export input=${input:-$definput}
echo "$input"

for d in $what; do
  pushd $d
  echo "Testing $d..."
  for parser in recursive ll1 lr0 lr1 slr lalr; do
    echo "Checking $parser parser"
    args="-p $parser" ./test.sh "$@"
  done
  popd
done
