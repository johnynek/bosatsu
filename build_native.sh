#!/bin/sh

expected_graalvm_version="23.0.2"
native_image_version="$(native-image --version 2>/dev/null | head -n 1 || true)"

if [ -z "$native_image_version" ]; then
  echo "build_native.sh: native-image not found; install GraalVM ${expected_graalvm_version} first" >&2
  exit 1
fi

case "$native_image_version" in
  *"${expected_graalvm_version}"*)
    ;;
  *)
    echo "build_native.sh: expected GraalVM ${expected_graalvm_version} to match CI/release native-image builds; found: ${native_image_version}" >&2
    exit 1
    ;;
esac

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
