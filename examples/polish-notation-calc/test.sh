#!/bin/bash

export alpaca=$(cabal exec -- which alpaca)

for d in cpp csharp py; do
  pushd $d
  echo "Testing $d..."
  for parser in recursive ll1 lr0 lr1 slr lalr; do
    echo "Checking $parser parser"
    args="-p $parser" ./test.sh "$@"
  done
  popd
done
