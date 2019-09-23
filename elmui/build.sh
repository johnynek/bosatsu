#!/bin/bash
set -e

if [ ! -z "$TRAVIS" ]
then
  # install elm on travis
  npm install -g elm
  echo "About to set up deploy key"
  echo $DEPLOY_KEY | base64 -d > bosatsu_deploy_key
  # make sure the key was created correctly (cksum is fine, and don't leak more than we need to)
  cksum bosatsu_deploy_key
fi

cd elmui
make
cd ..
mkdir -p compiler
mkdir -p compiler/out

cp elmui/index.html compiler/
cp elmui/out/main.js compiler/out/
cp elmui/out/bosatsu.js compiler/out/
