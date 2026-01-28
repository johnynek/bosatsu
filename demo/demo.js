// Bosatsu Multi-Target Demo
//
// This is a UI shim that displays and executes pre-compiled Bosatsu output.
// The actual generated files are:
//   - examples/_bundle.js - JavaScript output from: transpile js
//   - examples/fibonacci.c - C output from: transpile c
//   - examples/fibonacci_wasm.js - WASM from emscripten (optional)
//
// This file does NOT modify the generated code - it only loads, displays,
// and evaluates it in the browser.

async function init() {
  // Load source files
  const loadFile = async (url, description) => {
    try {
      const response = await fetch(url);
      if (response.ok) {
        return await response.text();
      }
      return `// ${description} not available\n// Run the build script to generate this file.`;
    } catch (e) {
      return `// Failed to load ${description}: ${e.message}`;
    }
  };

  // Load runtime and fibonacci module separately
  // The JS transpiler outputs ES modules to Demo/Package/index.js
  const [source, runtimeCode, fibCode, cCode] = await Promise.all([
    loadFile('examples/fibonacci.bosatsu', 'Bosatsu source'),
    loadFile('examples/_runtime.js', 'Bosatsu runtime'),
    loadFile('examples/Demo/Fibonacci/index.js', 'Generated JavaScript'),
    loadFile('examples/fibonacci.c', 'Generated C code')
  ]);

  // Combine runtime and fibonacci code for display
  const jsCode = `// === Bosatsu Runtime ===\n${runtimeCode}\n\n// === Demo/Fibonacci ===\n${fibCode}`;

  document.getElementById('source-code').textContent = source;
  document.getElementById('js-code').textContent = jsCode;
  document.getElementById('c-code').textContent = cCode;

  hljs.highlightAll();

  // Tab switching
  document.querySelectorAll('.tab').forEach(tab => {
    tab.addEventListener('click', () => {
      document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
      document.querySelectorAll('.tab-content').forEach(c => c.classList.add('hidden'));
      tab.classList.add('active');
      document.getElementById(tab.dataset.target).classList.remove('hidden');
    });
  });

  // Run JS button
  document.getElementById('run-js').addEventListener('click', async () => {
    const resultEl = document.getElementById('js-result');
    resultEl.classList.remove('error');

    try {
      const jsCode = document.getElementById('js-code').textContent;
      const result = await runJS(jsCode);
      resultEl.textContent = `Result: ${result} (fib(20) = 6765)`;
    } catch (err) {
      resultEl.textContent = `Error: ${err.message}`;
      resultEl.classList.add('error');
    }
  });

  // Run WASM button
  document.getElementById('run-wasm').addEventListener('click', async () => {
    const resultEl = document.getElementById('wasm-result');
    const btn = document.getElementById('run-wasm');
    resultEl.classList.remove('error');
    btn.disabled = true;
    btn.textContent = 'Loading...';

    try {
      const result = await runWASM();
      resultEl.textContent = `Result: ${result} (fib(20) = 6765)`;
    } catch (err) {
      resultEl.textContent = `Error: ${err.message}`;
      resultEl.classList.add('error');
    } finally {
      btn.disabled = false;
      btn.textContent = 'Run WebAssembly';
    }
  });
}

async function runJS(code) {
  // The generated JS is an ES module with runtime and fibonacci code
  // We need to strip the ES module syntax and evaluate it as a script

  // Check if code looks like generated output
  if (code.includes('// Generated JavaScript not available') || code.includes('// Run the build script')) {
    throw new Error('JavaScript has not been generated yet. Build the demo artifacts first.');
  }

  try {
    // Remove ES module exports and imports (convert to script-style)
    let scriptCode = code
      .replace(/^export \{[^}]*\};?$/gm, '')  // Remove export statements
      .replace(/^import [^;]*;$/gm, '')        // Remove import statements
      .replace(/if \(typeof require !== 'undefined'\) \{ require\([^)]*\); \}/g, '');  // Remove Node.js require

    // The fibonacci module defines 'fib' as a const - we just need to call it
    // Add a call to fib(20) at the end to get the result
    const fn = new Function(scriptCode + '\nreturn fib(20);');
    const result = fn();

    // Handle Bosatsu Int representation (could be BigInt or object)
    if (typeof result === 'bigint') {
      return Number(result);
    } else if (result && typeof result === 'object' && 'value' in result) {
      return result.value;
    }
    return result;
  } catch (e) {
    console.error('JS execution error:', e);
    throw new Error('Could not execute generated JavaScript: ' + e.message);
  }
}

async function runWASM() {
  try {
    // Try loading the emscripten-generated module
    const moduleScript = await fetch('examples/fibonacci_wasm.js');
    if (!moduleScript.ok) {
      throw new Error('WASM module not found. WASM compilation may not be available in this build.');
    }

    // Load and instantiate the WASM module
    const moduleCode = await moduleScript.text();

    // Create a module factory from the Emscripten-generated code
    // Emscripten generates a Module factory when MODULARIZE=1
    const createModule = new Function(moduleCode + '\nreturn Module;')();

    // Instantiate the module
    const Module = await createModule({
      // Emscripten configuration
      print: (text) => console.log(text),
      printErr: (text) => console.error(text)
    });

    // Call the main function and get result
    if (Module._main) {
      return Module._main();
    } else if (Module.ccall) {
      return Module.ccall('main', 'number', [], []);
    } else {
      throw new Error('Could not find main function in WASM module');
    }
  } catch (e) {
    console.error('WASM error:', e);
    throw new Error('WASM execution failed: ' + e.message);
  }
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}
