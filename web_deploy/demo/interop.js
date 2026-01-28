/**
 * JS/WASM Interop Bridge
 *
 * This module:
 * 1. Loads Bosatsu-generated JS (orchestrator + compute)
 * 2. Loads Bosatsu-generated WASM (compute)
 * 3. Patches the JS to redirect compute functions to WASM
 *
 * Result: Bosatsu orchestration logic runs in JS, heavy computation runs in WASM
 */

let wasm = null;
let jsLoaded = false;
let wasmLoaded = false;

// Original JS implementations (saved before patching)
let originalFib = null;
let originalFactorial = null;
let originalCollatz = null;

/**
 * Load the Bosatsu-generated JS modules
 */
export async function loadBosatsuJS() {
  // Load the bundle which includes both Demo/Compute and Demo/Orchestrator
  await import('./examples/_bundle.js');

  // Save references to original JS implementations
  originalFib = globalThis.Demo_Compute$fib;
  originalFactorial = globalThis.Demo_Compute$factorial;

  jsLoaded = true;
  console.log('Bosatsu JS loaded');
}

/**
 * Load the WASM module compiled from Bosatsu
 */
export async function loadBosatsuWASM() {
  try {
    console.log('Loading WASM module...');

    // Emscripten with MODULARIZE outputs a global factory function
    // We need to load it as a script, not as an ES module
    await new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = './examples/compute_wasm.js';
      script.onload = resolve;
      script.onerror = reject;
      document.head.appendChild(script);
    });

    console.log('WASM JS loader script loaded');

    // The factory function should now be available as globalThis.createModule
    if (typeof globalThis.createModule !== 'function') {
      throw new Error('createModule not found after loading script');
    }

    console.log('Calling createModule()...');
    wasm = await globalThis.createModule({
      // Tell emscripten where to find the .wasm file
      locateFile: (path) => {
        console.log('locateFile called for:', path);
        if (path.endsWith('.wasm')) {
          return './examples/' + path;
        }
        return path;
      }
    });
    console.log('WASM module created, calling _wasm_init...');

    wasm._wasm_init();
    wasmLoaded = true;
    console.log('Bosatsu WASM loaded successfully');
  } catch (error) {
    console.error('WASM loading failed:', error);
    throw error;
  }
}

/**
 * Patch the Bosatsu-generated JS to use WASM for compute functions
 * After this, when the JS orchestrator calls fib() or factorial(),
 * it will actually execute WASM code
 */
export function patchJStoUseWASM() {
  if (!wasmLoaded) throw new Error('WASM not loaded');

  // Replace the JS implementations with WASM calls
  globalThis.Demo_Compute$fib = (n) => wasm._wasm_fib(n);
  globalThis.Demo_Compute$factorial = (n) => wasm._wasm_factorial(n);
  globalThis.Demo_Compute$collatz_steps = (n) => wasm._wasm_collatz(n);

  console.log('JS patched to use WASM for compute');
}

/**
 * Restore original JS implementations
 */
export function unpatchJS() {
  if (originalFib) globalThis.Demo_Compute$fib = originalFib;
  if (originalFactorial) globalThis.Demo_Compute$factorial = originalFactorial;
  console.log('JS restored to native implementations');
}

/**
 * Call the Bosatsu-generated orchestrator to validate fib input
 * This demonstrates Bosatsu JS logic working alongside WASM
 */
export function validateFibInput(n) {
  // Bosatsu Bool is [0] for false, [1] for true
  const result = globalThis.Demo_Orchestrator$validate_fib_input(n);
  return result[0] === 1;
}

/**
 * Call the Bosatsu-generated orchestrator to validate factorial input
 */
export function validateFactInput(n) {
  const result = globalThis.Demo_Orchestrator$validate_fact_input(n);
  return result[0] === 1;
}

/**
 * Call the Bosatsu-generated orchestrator to compute fib sequence
 * Returns array of [index, value] pairs
 */
export function computeFibSequence(n) {
  const bosatsuList = globalThis.Demo_Orchestrator$compute_fib_sequence(n);
  return bosatsuListToArray(bosatsuList).map(pair => ({
    index: pair[0],
    value: pair[1]
  }));
}

/**
 * Call the Bosatsu-generated orchestrator to compute factorial table
 * Returns array of [index, value] pairs
 */
export function computeFactorialTable(n) {
  const bosatsuList = globalThis.Demo_Orchestrator$compute_factorial_table(n);
  return bosatsuListToArray(bosatsuList).map(pair => ({
    index: pair[0],
    value: pair[1]
  }));
}

/**
 * Convert Bosatsu list to JS array
 * Bosatsu list: [0] for empty, [1, head, tail] for cons
 */
function bosatsuListToArray(list) {
  const result = [];
  let current = list;
  while (current[0] === 1) {
    result.push(current[1]);
    current = current[2];
  }
  return result;
}

/**
 * Get status of loaded modules
 */
export function getStatus() {
  return { jsLoaded, wasmLoaded };
}
