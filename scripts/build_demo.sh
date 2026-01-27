#!/bin/bash
# Build demo artifacts for local testing
#
# Architecture:
# - orchestrator.bosatsu -> JS (validation, sequencing logic)
# - compute.bosatsu -> JS (bundled with orchestrator for fallback)
# - compute.bosatsu -> C -> WASM (fast computation)
# - interop.js patches JS to call WASM at runtime
#
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

DEMO_DIR="web_deploy/demo/examples"
mkdir -p "$DEMO_DIR"

echo "=== Building Bosatsu JS/WASM Interop Demo ==="

# Copy source files
echo "Copying Bosatsu source files..."
cp demo/examples/compute.bosatsu "$DEMO_DIR/"
cp demo/examples/orchestrator.bosatsu "$DEMO_DIR/"

# Step 1: Compile both files to JS bundle
# The orchestrator imports from compute, so we compile them together
echo "Compiling Bosatsu to JavaScript..."
sbt "cli/run transpile js --input demo/examples/compute.bosatsu --input demo/examples/orchestrator.bosatsu --outdir $DEMO_DIR" || {
  echo "Error: JS compilation failed"
  exit 1
}

# Step 2: Compile compute.bosatsu to C (for WASM)
echo "Compiling compute.bosatsu to C..."
sbt "cli/run transpile c --input demo/examples/compute.bosatsu --output compute.c --outdir $DEMO_DIR" || {
  echo "Error: C compilation failed"
  exit 1
}

# Step 3: Compile C to WASM (requires emscripten)
if command -v emcc &> /dev/null; then
  echo "Compiling C to WebAssembly..."
  if [ -f "$DEMO_DIR/compute.c" ]; then
    emcc "$DEMO_DIR/compute.c" \
      demo/wasm_wrapper.c \
      -I c_runtime \
      c_runtime/bosatsu_runtime.c \
      c_runtime/bosatsu_ext_Bosatsu_l_Predef.c \
      c_runtime/bosatsu_generated.c \
      -sWASM=1 \
      -sEXPORTED_RUNTIME_METHODS='["ccall","cwrap"]' \
      -sEXPORTED_FUNCTIONS='["_wasm_init","_wasm_fib","_wasm_factorial"]' \
      -sMODULARIZE=1 \
      -sEXPORT_NAME='createModule' \
      -sALLOW_MEMORY_GROWTH=1 \
      -O2 \
      -o "$DEMO_DIR/compute_wasm.js" \
      || {
        echo "Error: WASM compilation failed"
        exit 1
      }
    echo "WASM module built: $DEMO_DIR/compute_wasm.js"
  else
    echo "Error: compute.c not found"
    exit 1
  fi
else
  echo "Warning: emscripten not found, skipping WASM compilation"
  echo "Install: https://emscripten.org/docs/getting_started/downloads.html"
fi

echo "=== Done ==="
echo "Demo artifacts in: $DEMO_DIR"
ls -la "$DEMO_DIR"
