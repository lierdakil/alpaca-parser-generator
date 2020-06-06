#!/bin/bash

${alpaca:-alpaca} -l cs $args syntax.xy &&\
  dotnet run "$@" <<< "$input"
