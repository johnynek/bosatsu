const fs = require('fs');
const vm = require('vm');

// Read the bundle
const bundleCode = fs.readFileSync('./jsout/_bundle.js', 'utf8');

// Create a context to run the code
const context = { console, _tests: {} };
vm.createContext(context);

try {
  vm.runInContext(bundleCode, context, { timeout: 60000 });
} catch (err) {
  console.error("Error running bundle:", err.message);
  // Show context around the error
  const lines = bundleCode.split('\n');
  const match = err.stack.match(/<anonymous>:(\d+)/);
  if (match) {
    const lineNum = parseInt(match[1]);
    console.error("\nNear line " + lineNum + ":");
    for (let i = Math.max(0, lineNum - 3); i < Math.min(lines.length, lineNum + 3); i++) {
      console.error((i + 1) + ": " + lines[i]);
    }
  }
  process.exit(1);
}

// Helper to convert Bosatsu string to JS string
const _bosatsu_to_js_string = (bstr) => {
  let result = '';
  let current = bstr;
  while (current && current[0] === 1) {
    result += current[1];
    current = current[2];
  }
  return result;
};

console.log("Found " + Object.keys(context._tests).length + " tests\n");

let passed = 0;
let failed = 0;
let errors = 0;

const checkAssertion = (name, assertion) => {
  if (!assertion || !Array.isArray(assertion)) {
    console.log("  ? " + name + ": not a valid assertion");
    errors++;
    return false;
  }

  // Assertion: [0, boolResult, message]
  if (assertion[0] === 0 && Array.isArray(assertion[1])) {
    const boolResult = assertion[1][0] === 1;
    const message = assertion[2] ? _bosatsu_to_js_string(assertion[2]) : "";

    if (boolResult) {
      console.log("  ✓ " + name);
      passed++;
    } else {
      console.log("  ✗ " + name + ": " + message);
      failed++;
    }
    return true;
  }
  return false;
};

const processTestValue = (name, value, indent = "  ") => {
  // Check if it's a single Assertion: [0, boolResult, message]
  if (checkAssertion(name, value)) return;

  // Check if it's a TestSuite: [1, suiteName, assertionsList]
  // where suiteName is a Bosatsu string and assertionsList is a cons list
  if (value && value[0] === 1 && Array.isArray(value[1]) && value[1][0] === 1) {
    // This looks like a TestSuite - value[1] is a Bosatsu string (starts with [1, char, ...])
    const suiteName = _bosatsu_to_js_string(value[1]);
    const assertions = value[2];
    console.log(indent + "Suite: " + suiteName);

    // Process assertions list - may contain assertions or nested suites
    let current = assertions;
    let idx = 0;
    while (current && current[0] === 1) {
      const item = current[1];
      // Recursively process - could be assertion or nested suite
      processTestValue(name + "[" + idx + "]", item, indent + "  ");
      current = current[2];
      idx++;
    }
    return;
  }

  // Check if it's a plain list of assertions
  let current = value;
  let idx = 0;
  while (current && current[0] === 1) {
    processTestValue(name + "[" + idx + "]", current[1], indent);
    current = current[2];
    idx++;
  }
  if (idx === 0) {
    console.log(indent + "? " + name + ": unknown format");
    errors++;
  }
};

for (const [name, value] of Object.entries(context._tests)) {
  processTestValue(name, value);
}

console.log("\nResults: " + passed + " passed, " + failed + " failed, " + errors + " errors");
process.exit(failed > 0 || errors > 0 ? 1 : 0);
