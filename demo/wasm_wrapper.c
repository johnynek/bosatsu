/**
 * WASM Wrapper for Bosatsu Compute Functions
 *
 * This file provides JavaScript-callable wrappers around the generated
 * Bosatsu functions. It converts between native int types and Bosatsu's
 * BValue representation.
 *
 * When compiled with emscripten, these functions are exported for use
 * from JavaScript via Module.ccall or Module._funcname.
 */

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#define WASM_EXPORT EMSCRIPTEN_KEEPALIVE
#else
#define WASM_EXPORT
#endif

#include "gc.h"
#include "bosatsu_runtime.h"

// Forward declarations for generated Bosatsu functions
// These names follow the pattern: ___bsts_g_{Package}_l_{Subpackage}_l_{name}
// where 'l' is the separator
// Note: C codegen generates direct functions, not closures
extern BValue ___bsts_g_Demo_l_Compute_l_fib(BValue n);
extern BValue ___bsts_g_Demo_l_Compute_l_factorial(BValue n);
extern BValue ___bsts_g_Demo_l_Compute_l_collatz__steps(BValue n);

/**
 * Compute fibonacci number
 * @param n - The index in the fibonacci sequence
 * @return fib(n)
 */
WASM_EXPORT
int wasm_fib(int n) {
    // Convert n to BValue
    BValue n_val = bsts_integer_from_int(n);

    // Call the function directly (C codegen produces direct functions, not closures)
    BValue result = ___bsts_g_Demo_l_Compute_l_fib(n_val);

    // Convert result back to int
    return bsts_integer_to_int32(result);
}

/**
 * Compute factorial
 * @param n - The number to compute factorial of
 * @return n!
 */
WASM_EXPORT
int wasm_factorial(int n) {
    // Convert n to BValue
    BValue n_val = bsts_integer_from_int(n);

    // Call the function directly (C codegen produces direct functions, not closures)
    BValue result = ___bsts_g_Demo_l_Compute_l_factorial(n_val);

    // Convert result back to int
    return bsts_integer_to_int32(result);
}

/**
 * Compute Collatz sequence steps
 * @param n - The starting number
 * @return Number of steps to reach 1
 */
WASM_EXPORT
int wasm_collatz(int n) {
    // Convert n to BValue
    BValue n_val = bsts_integer_from_int(n);

    // Call the function directly
    BValue result = ___bsts_g_Demo_l_Compute_l_collatz__steps(n_val);

    // Convert result back to int
    return bsts_integer_to_int32(result);
}

/**
 * Initialize the WASM module
 * Called once when the module loads
 */
WASM_EXPORT
void wasm_init(void) {
    GC_init();
    init_statics();
}
