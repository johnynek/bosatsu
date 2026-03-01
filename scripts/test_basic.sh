#!/usr/bin/env bash
set -euo pipefail

sbt -batch "cli/testOnly * -- --log=failure; coreJVM/testOnly * -- --log=failure"
