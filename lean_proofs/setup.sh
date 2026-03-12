#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export PATH="$HOME/.elan/bin:$PATH"

if ! command -v elan >/dev/null 2>&1; then
  curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y
fi

if ! command -v lake >/dev/null 2>&1; then
  elan toolchain install "$(cat "$ROOT_DIR/lean-toolchain")"
fi

cd "$ROOT_DIR"
lake update
lake build
