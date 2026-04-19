#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

TMPDIR_ROOT="${TMPDIR:-/tmp}"
WORKDIR="$(mktemp -d "${TMPDIR_ROOT%/}/bosatsu-launcher-test.XXXXXX")"
cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

TEST_REPO="$WORKDIR/repo"
mkdir -p "$TEST_REPO"
git -C "$TEST_REPO" init -q

cp "$REPO_ROOT/bosatsu" "$TEST_REPO/bosatsu"
chmod +x "$TEST_REPO/bosatsu"
printf 'test-version\n' > "$TEST_REPO/.bosatsu_version"
printf 'native\n' > "$TEST_REPO/.bosatsu_platform"

FAKE_ARTIFACT="$WORKDIR/fake-bosatsu"
cat > "$FAKE_ARTIFACT" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$PWD/argv.txt"
EOF
chmod +x "$FAKE_ARTIFACT"

(
  cd "$TEST_REPO"
  ./bosatsu \
    --artifact "$FAKE_ARTIFACT" \
    eval --main Zafu/Tool/JsonFormat::main --run -- --compact
)

EXPECTED="$WORKDIR/expected.txt"
cat > "$EXPECTED" <<'EOF'
eval
--main
Zafu/Tool/JsonFormat::main
--run
--
--compact
EOF

if ! cmp -s "$EXPECTED" "$TEST_REPO/argv.txt"; then
  echo "launcher did not preserve the eval passthrough delimiter" >&2
  diff -u "$EXPECTED" "$TEST_REPO/argv.txt" >&2 || true
  exit 1
fi
