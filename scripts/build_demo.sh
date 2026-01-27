#!/bin/bash
# Build demo artifacts for local testing
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

DEMO_DIR="web_deploy/demo/examples"
mkdir -p "$DEMO_DIR"

echo "=== Building demo artifacts ==="

# Copy source file
echo "Copying Bosatsu source..."
cp demo/examples/fibonacci.bosatsu "$DEMO_DIR/"

# Compile to JS
echo "Compiling to JavaScript..."
sbt "cli/run transpile js --input demo/examples/fibonacci.bosatsu --outdir $DEMO_DIR" || {
  echo "Warning: JS compilation failed"
}

# Compile to C
echo "Compiling to C..."
sbt "cli/run transpile c --main Demo/Fibonacci --input demo/examples/fibonacci.bosatsu --output fibonacci.c --outdir $DEMO_DIR" || {
  echo "Warning: C compilation failed"
}

# Compile C to WASM (requires emscripten)
if command -v emcc &> /dev/null; then
  echo "Compiling C to WebAssembly..."
  if [ -f "$DEMO_DIR/fibonacci.c" ]; then
    emcc "$DEMO_DIR/fibonacci.c" \
      -I c_runtime \
      c_runtime/bosatsu_runtime.c \
      c_runtime/bosatsu_ext_Bosatsu_l_Predef.c \
      -sWASM=1 \
      -sEXPORTED_RUNTIME_METHODS='["ccall","cwrap"]' \
      -sEXPORTED_FUNCTIONS='["_main"]' \
      -sMODULARIZE=1 \
      -sEXPORT_NAME='Module' \
      -o "$DEMO_DIR/fibonacci_wasm.js" \
      || echo "Warning: WASM compilation failed"
  else
    echo "Warning: C file not found, skipping WASM compilation"
  fi
else
  echo "Note: emscripten not found, skipping WASM compilation"
fi

echo "=== Done ==="
echo "Demo artifacts are in: $DEMO_DIR"
ls -la "$DEMO_DIR"
