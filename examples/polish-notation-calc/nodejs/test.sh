#!/bin/bash

${alpaca:-alpaca} -l js $args syntax.xy &&\
  node main.js "$@" <<< "$input"
