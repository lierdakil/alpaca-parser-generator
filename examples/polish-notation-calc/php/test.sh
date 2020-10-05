#!/bin/bash

${alpaca:-alpaca} -l php $args syntax.xy &&\
  php main.php "$@" <<< "$input"
