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

FAKE_BIN="$WORKDIR/fake-bin"
mkdir -p "$FAKE_BIN"
cat > "$FAKE_BIN/java" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$ARGV_OUTPUT"
EOF
chmod +x "$FAKE_BIN/java"

PARAMS='@params file 雪'
JAR_PATH="$(
  ls -1t "$REPO_ROOT"/cli/target/scala-*/bosatsu-cli-assembly-*.jar 2>/dev/null | head -n 1 || true
)"
if [ -z "$JAR_PATH" ]; then
  echo "launcher passthrough test requires an assembled bosatsu jar" >&2
  exit 1
fi

ARGV_OUTPUT="$WORKDIR/bosatsuj-argv.txt" PATH="$FAKE_BIN:$PATH" \
  "$REPO_ROOT/bosatsuj" tool bazel-worker "$PARAMS"
EXPECTED_BOSATSUJ="$WORKDIR/expected-bosatsuj.txt"
{
  printf '%s\n' -jar "$JAR_PATH" tool bazel-worker "$PARAMS"
} > "$EXPECTED_BOSATSUJ"
if ! cmp -s "$EXPECTED_BOSATSUJ" "$WORKDIR/bosatsuj-argv.txt"; then
  echo "bosatsuj did not preserve the worker parameter-file argument" >&2
  diff -u "$EXPECTED_BOSATSUJ" "$WORKDIR/bosatsuj-argv.txt" >&2 || true
  exit 1
fi

printf 'java-artifact\n' > "$WORKDIR/fake-java-artifact.jar"
printf 'java\n' > "$TEST_REPO/.bosatsu_platform"
ARGV_OUTPUT="$WORKDIR/bosatsu-java-argv.txt" PATH="$FAKE_BIN:$PATH" \
  bash -c 'cd "$1" && ./bosatsu --artifact "$2" tool bazel-worker "$3"' \
  _ "$TEST_REPO" "$WORKDIR/fake-java-artifact.jar" "$PARAMS"
EXPECTED_BOSATSU="$WORKDIR/expected-bosatsu-java.txt"
{
  printf '%s\n' -jar "$TEST_REPO/.bosatsuc/cli/test-version/bosatsu.jar" \
    tool bazel-worker "$PARAMS"
} > "$EXPECTED_BOSATSU"
if ! cmp -s "$EXPECTED_BOSATSU" "$WORKDIR/bosatsu-java-argv.txt"; then
  echo "Java-platform bosatsu did not preserve the worker parameter-file argument" >&2
  diff -u "$EXPECTED_BOSATSU" "$WORKDIR/bosatsu-java-argv.txt" >&2 || true
  exit 1
fi
