#!/bin/bash

${alpaca:-alpaca} -l cpp $args syntax.xy &&\
  g++ -std=c++17 -O2 main.cpp lexer.cpp parser.cpp -o program &&\
  ./program "$@" <<< "$input"
