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

# Pre-populate CAS so publish can validate semver against previous versions.
echo "bosatsuj lib fetch:"
LIB_NAMES=()
LIBS_JSON="$REPO_ROOT/bosatsu_libs.json"
if [[ -f "$LIBS_JSON" ]]; then
  if command -v python3 >/dev/null 2>&1; then
    PYTHON=python3
  elif command -v python >/dev/null 2>&1; then
    PYTHON=python
  else
    PYTHON=""
  fi

  if [[ -n "$PYTHON" ]]; then
    while IFS= read -r name; do
      LIB_NAMES+=("$name")
    done < <(
      "$PYTHON" - "$LIBS_JSON" <<'PY'
import json,sys
with open(sys.argv[1]) as f:
    data = json.load(f)
for name in sorted(data.keys()):
    print(name)
PY
    )
  else
    echo "WARNING: python not found; falling back to fetch without --name" >&2
  fi
fi

if [[ ${#LIB_NAMES[@]} -gt 0 ]]; then
  for name in "${LIB_NAMES[@]}"; do
    echo "  name = $name"
    ./bosatsuj lib fetch \
      --repo_root "$REPO_ROOT" \
      --name "$name"
  done
else
  ./bosatsuj lib fetch \
    --repo_root "$REPO_ROOT"
fi

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
