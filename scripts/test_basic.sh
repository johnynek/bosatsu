#!/usr/bin/env bash
set -euo pipefail

export PATH="$(pwd)/scripts/bin:$PATH"

if command -v protoc >/dev/null 2>&1; then
  protoc_path="$(command -v protoc)"
  sbt -batch \
    "set protoJVM / Compile / sbtprotoc.ProtocPlugin.autoImport.PB.protocExecutable := file(\"$protoc_path\")" \
    "cli/testOnly * -- --log=failure; coreJVM/testOnly * -- --log=failure"
else
  sbt -batch "cli/testOnly * -- --log=failure; coreJVM/testOnly * -- --log=failure"
fi
