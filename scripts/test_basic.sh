#!/usr/bin/env bash
set -euo pipefail

sbt -batch "cli/test; coreJVM/test"
