#!/bin/sh

#sbt cli/assembly
native-image --static \
  --no-fallback \
  --verbose \
  --initialize-at-build-time \
  -jar cli/target/scala-2.12/bosatsu-cli-assembly-0.1.0-SNAPSHOT.jar \
  bosatsu
