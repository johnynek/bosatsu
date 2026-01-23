#!/bin/sh

#sbt cli/assembly
native-image --static \
  --no-fallback \
  --verbose \
  --initialize-at-build-time \
  -jar cli/target/scala-3.8.1/bosatsu-cli-assembly-0.0.7.jar \
  bosatsu
