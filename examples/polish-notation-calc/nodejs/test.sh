#!/bin/bash

${alpaca:-alpaca} -l js $args syntax.xy &&\
  node main.js "$@" <<< '+ 1 * 2 ^ 3 4'
