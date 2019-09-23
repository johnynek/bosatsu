#!/bin/bash

if [ ! -z "$TRAVIS" ]
then
  # install elm on travis
  npm install -g elm
  echo $DEPLOY_TRAVIS | base64 -d > bosatsu_deploy_key
fi

cd elmui
make
cd ..
mkdir -p compiler
mkdir -p compiler/out

cp elmui/index.html compiler/
cp elmui/out/main.js compiler/out/
cp elmui/out/bosatsu.js compiler/out/
