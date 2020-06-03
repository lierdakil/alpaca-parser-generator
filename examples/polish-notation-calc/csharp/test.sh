#!/bin/bash

${alpaca:-alpaca} -l cs $args syntax.xy &&\
  dotnet run "$@" <<< '+ 1 * 2 ^ 3 4'
