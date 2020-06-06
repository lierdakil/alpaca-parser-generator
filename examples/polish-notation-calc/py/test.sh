#!/bin/bash

${alpaca:-alpaca} -l py $args syntax.xy &&\
  python main.py "$@" <<< "$input"
