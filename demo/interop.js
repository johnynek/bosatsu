/**
 * JS/WASM Interop Bridge for Bosatsu Demo
 *
 * This module demonstrates calling WASM-compiled Bosatsu functions from
 * JavaScript-compiled Bosatsu code. It loads the WASM module and patches
 * the generated JS globals to redirect calls to WASM implementations.
 */

// WASM module instance
let wasmModule = null;
let wasmFib = null;
let wasmFactorial = null;

// Track original JS implementations for comparison
let jsFib = null;
let jsFactorial = null;

/**
 * Initialize the WASM module and set up the FFI bridge
 * @param {string} wasmPath - Path to the compute_wasm.js file
 * @returns {Promise<void>}
 */
async function initWasmBridge(wasmPath = './examples/compute_wasm.js') {
  // Dynamically import the emscripten-generated module
  const ComputeModule = (await import(wasmPath)).default || (await import(wasmPath));

  // Initialize the WASM module
  wasmModule = await ComputeModule();

  // Call wasm_init to initialize the Bosatsu runtime (GC, statics)
  wasmModule._wasm_init();

  // Get wrapped WASM functions
  wasmFib = wasmModule.cwrap('wasm_fib', 'number', ['number']);
  wasmFactorial = wasmModule.cwrap('wasm_factorial', 'number', ['number']);

  console.log('WASM bridge initialized');
}

/**
 * Compute fibonacci using the WASM implementation
 * @param {number} n - The index in the fibonacci sequence
 * @returns {number} fib(n)
 */
function computeFibWasm(n) {
  if (!wasmFib) {
    throw new Error('WASM bridge not initialized. Call initWasmBridge() first.');
  }
  return wasmFib(n);
}

/**
 * Compute factorial using the WASM implementation
 * @param {number} n - The number to compute factorial of
 * @returns {number} n!
 */
function computeFactorialWasm(n) {
  if (!wasmFactorial) {
    throw new Error('WASM bridge not initialized. Call initWasmBridge() first.');
  }
  return wasmFactorial(n);
}

/**
 * Patch the generated JS globals to use WASM implementations.
 * This replaces Demo_Compute$fib and Demo_Compute$factorial with
 * versions that call into WASM.
 *
 * Note: In Bosatsu's JS codegen, functions are pure values (lambdas).
 * For functions like fib: Int -> Int, they are represented as JS functions.
 */
function patchJsWithWasm() {
  if (!wasmModule) {
    throw new Error('WASM bridge not initialized. Call initWasmBridge() first.');
  }

  // Save original JS implementations for comparison
  if (typeof Demo_Compute$fib !== 'undefined') {
    jsFib = Demo_Compute$fib;
    // Replace with WASM version
    // Bosatsu functions are just JS functions, so we create a wrapper
    globalThis.Demo_Compute$fib = (n) => wasmFib(n);
  }

  if (typeof Demo_Compute$factorial !== 'undefined') {
    jsFactorial = Demo_Compute$factorial;
    // Replace with WASM version
    globalThis.Demo_Compute$factorial = (n) => wasmFactorial(n);
  }

  console.log('JS globals patched to use WASM implementations');
}

/**
 * Restore original JS implementations
 */
function unpatchJs() {
  if (jsFib) {
    globalThis.Demo_Compute$fib = jsFib;
  }
  if (jsFactorial) {
    globalThis.Demo_Compute$factorial = jsFactorial;
  }
  console.log('JS globals restored to native implementations');
}

/**
 * Run a comparison benchmark between JS and WASM implementations
 * @param {number} n - The input value for fib and factorial
 * @returns {Object} Results and timing comparison
 */
function benchmark(n) {
  const results = {
    input: n,
    fib: { js: null, wasm: null, jsTime: 0, wasmTime: 0 },
    factorial: { js: null, wasm: null, jsTime: 0, wasmTime: 0 }
  };

  // Test fib
  if (jsFib || typeof Demo_Compute$fib !== 'undefined') {
    const fibFn = jsFib || Demo_Compute$fib;
    const start = performance.now();
    results.fib.js = fibFn(n);
    results.fib.jsTime = performance.now() - start;
  }

  if (wasmFib) {
    const start = performance.now();
    results.fib.wasm = wasmFib(n);
    results.fib.wasmTime = performance.now() - start;
  }

  // Test factorial
  if (jsFactorial || typeof Demo_Compute$factorial !== 'undefined') {
    const factFn = jsFactorial || Demo_Compute$factorial;
    const start = performance.now();
    results.factorial.js = factFn(n);
    results.factorial.jsTime = performance.now() - start;
  }

  if (wasmFactorial) {
    const start = performance.now();
    results.factorial.wasm = wasmFactorial(n);
    results.factorial.wasmTime = performance.now() - start;
  }

  return results;
}

// Export for use in demo page
export {
  initWasmBridge,
  computeFibWasm,
  computeFactorialWasm,
  patchJsWithWasm,
  unpatchJs,
  benchmark,
  wasmModule
};
