#!/bin/bash
# Build demo artifacts for local testing
# This builds a JS/WASM interop demo where:
# - compute.bosatsu (fib, factorial) -> C -> WASM
# - orchestrator.bosatsu -> JS (calls into WASM via FFI bridge)
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

DEMO_DIR="web_deploy/demo/examples"
mkdir -p "$DEMO_DIR"

echo "=== Building JS/WASM Interop Demo ==="

# Copy source files
echo "Copying Bosatsu source files..."
cp demo/examples/compute.bosatsu "$DEMO_DIR/"
cp demo/examples/orchestrator.bosatsu "$DEMO_DIR/"

# Step 1: Compile compute.bosatsu to C (will become WASM)
echo "Compiling compute.bosatsu to C..."
sbt "cli/run transpile c --input demo/examples/compute.bosatsu --output compute.c --outdir $DEMO_DIR" || {
  echo "Error: C compilation failed"
  exit 1
}

# Step 2: Compile both files to JS (orchestrator imports from compute)
# The generated JS will have native implementations - FFI bridge will redirect to WASM
echo "Compiling to JavaScript..."
sbt "cli/run transpile js --input demo/examples/compute.bosatsu --input demo/examples/orchestrator.bosatsu --outdir $DEMO_DIR" || {
  echo "Error: JS compilation failed"
  exit 1
}

# Step 3: Compile C to WASM (requires emscripten)
if command -v emcc &> /dev/null; then
  echo "Compiling C to WebAssembly..."
  if [ -f "$DEMO_DIR/compute.c" ]; then
    # Compile the generated compute.c plus the wrapper that exports WASM functions
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
      -sEXPORT_NAME='ComputeModule' \
      -sALLOW_MEMORY_GROWTH=1 \
      -O2 \
      -o "$DEMO_DIR/compute_wasm.js" \
      || {
        echo "Error: WASM compilation failed"
        exit 1
      }
    echo "WASM module built: $DEMO_DIR/compute_wasm.js"
  else
    echo "Error: compute.c not found, cannot compile WASM"
    exit 1
  fi
else
  echo "Warning: emscripten not found, skipping WASM compilation"
  echo "Install emscripten to build WASM: https://emscripten.org/docs/getting_started/downloads.html"
fi

echo "=== Done ==="
echo "Demo artifacts are in: $DEMO_DIR"
ls -la "$DEMO_DIR"
