#!/bin/bash

export alpaca=$(cabal exec -- which alpaca)

what="$@"

[ -z "$what" ] && what="cpp csharp py nodejs"

export input=${input:-+ 1 * 2 ^ 3 4}

for d in $what; do
  pushd $d
  echo "Testing $d..."
  for parser in recursive ll1 lr0 lr1 slr lalr; do
    echo "Checking $parser parser"
    args="-p $parser" ./test.sh "$@"
  done
  popd
done
