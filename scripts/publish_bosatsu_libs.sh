#!/usr/bin/env bash
set -euo pipefail

# Optional: REPO_ROOT, OUTDIR, GIT_SHA, URI_BASE can be passed via env.
# Reasonable defaults for local use:

if [[ -n "${REPO_ROOT:-}" ]]; then
  REPO_ROOT="${REPO_ROOT%/}"
else
  REPO_ROOT="$(git rev-parse --show-toplevel)"
fi

OUTDIR="${OUTDIR:-"$REPO_ROOT/.bosatsu_lib_publish"}"
GIT_SHA="${GIT_SHA:-"$(git rev-parse HEAD)"}"

if [[ -z "${URI_BASE:-}" ]]; then
  echo "ERROR: URI_BASE must be set (e.g. https://github.com/OWNER/REPO/releases/download/TAG/)" >&2
  exit 1
fi

mkdir -p "$OUTDIR"

echo "bosatsuj lib publish:"
echo "  repo_root = $REPO_ROOT"
echo "  outdir    = $OUTDIR"
echo "  git_sha   = $GIT_SHA"
echo "  uri-base  = $URI_BASE"

cd "$REPO_ROOT"

# Add --cas_dir/--color here if you want to override defaults
./bosatsuj lib publish \
  --repo_root "$REPO_ROOT" \
  --outdir "$OUTDIR" \
  --git_sha "$GIT_SHA" \
  --uri-base "$URI_BASE"

echo
echo "Generated .bosatsu_lib files:"
ls -1 "$OUTDIR"/*.bosatsu_lib || {
  echo "No .bosatsu_lib files found in $OUTDIR" >&2
  exit 1
}
