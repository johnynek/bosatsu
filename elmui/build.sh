#!/bin/bash

if [ ! -z "$TRAVIS" ]
then
  # install elm on travis
  npm install -g elm
fi

make
cd ..
mkdir -p compiler
mkdir -p compiler/out

cp elmui/index.html compiler/
cp elmui/out/main.js compiler/out/
cp elmui/out/bosatsu.js compiler/out/
