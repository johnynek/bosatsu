#!/usr/bin/env bash
set -euo pipefail

export PATH="$(pwd)/scripts/bin:$PATH"

# Cold compiler fixtures can exceed their deadlines when suites contend for
# limited worker CPU. Serialize suites without changing tests or their timeouts.
core_test_settings="set coreJVM / Test / parallelExecution := false"

if command -v protoc >/dev/null 2>&1; then
  protoc_path="$(command -v protoc)"
  sbt -batch \
    "$core_test_settings" \
    "set protoJVM / Compile / sbtprotoc.ProtocPlugin.autoImport.PB.protocExecutable := file(\"$protoc_path\")" \
    "cli/testOnly * -- --log=failure; coreJVM/testOnly * -- --log=failure"
else
  sbt -batch "$core_test_settings" "cli/testOnly * -- --log=failure; coreJVM/testOnly * -- --log=failure"
fi
