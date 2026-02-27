#!/bin/sh

#sbt cli/assembly
jar_path="$(ls -1t cli/target/scala-*/bosatsu-cli-assembly-*.jar 2>/dev/null | head -n 1 || true)"

if [ -z "$jar_path" ]; then
  echo "build_native.sh: no assembly jar found; run sbt cli/assembly first" >&2
  exit 1
fi

native-image --static \
  --no-fallback \
  --verbose \
  --initialize-at-build-time \
  -jar "$jar_path" \
  bosatsu
