#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
TS_GRAMMAR_DIR="$REPO_ROOT/syntax/tree-sitter-bosatsu"

if [[ -n "${TREE_SITTER_BIN:-}" ]]; then
  TS_BIN="$TREE_SITTER_BIN"
elif command -v tree-sitter >/dev/null 2>&1; then
  TS_BIN="$(command -v tree-sitter)"
else
  echo "tree-sitter CLI not found. Set TREE_SITTER_BIN or install tree-sitter-cli." >&2
  exit 1
fi

TS_CONFIG_DIR="$TS_GRAMMAR_DIR/.tree-sitter"
mkdir -p "$TS_CONFIG_DIR"

cat > "$TS_CONFIG_DIR/config.json" <<CONFIG
{
  "parser-directories": [
    "$REPO_ROOT/syntax"
  ]
}
CONFIG

export TREE_SITTER_BIN="$TS_BIN"
export TREE_SITTER_DIR="$TS_CONFIG_DIR"
export TREE_SITTER_LIBDIR="$TS_CONFIG_DIR/lib"
export CROSS_RUNNER=1
export BOSATSU_REPO_ROOT="$REPO_ROOT"

"$TS_BIN" --version

(
  cd "$TS_GRAMMAR_DIR"
  "$TS_BIN" generate
  "$TS_BIN" test
)

sbt -batch "coreJVM/Test/runMain dev.bosatsu.TreeSitterDifferential"
