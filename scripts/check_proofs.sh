#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

proof_dirs=()
shopt -s nullglob
for dir in "$ROOT_DIR"/*_proofs; do
  if [ -d "$dir" ] && [ -x "$dir/setup.sh" ]; then
    proof_dirs+=("$dir")
  fi
done

if [ ${#proof_dirs[@]} -eq 0 ]; then
  echo "No proof suites found."
  exit 0
fi

for dir in "${proof_dirs[@]}"; do
  echo "==> Checking proof suite: $(basename "$dir")"
  (cd "$dir" && ./setup.sh)
done
