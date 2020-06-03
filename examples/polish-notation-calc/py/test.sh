#!/bin/bash

${alpaca:-alpaca} -l py $args syntax.xy &&\
  python main.py "$@" <<< '+ 1 * 2 ^ 3 4'
