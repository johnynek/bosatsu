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
extern BValue ___bsts_g_Demo_l_Compute_l_fib();
extern BValue ___bsts_g_Demo_l_Compute_l_factorial();

/**
 * Compute fibonacci number
 * @param n - The index in the fibonacci sequence
 * @return fib(n)
 */
WASM_EXPORT
int wasm_fib(int n) {
    // Get the fib function (it's a closure/function value)
    BValue fib_fn = ___bsts_g_Demo_l_Compute_l_fib();

    // Convert n to BValue
    BValue n_val = bsts_integer_from_int(n);

    // Call the function using the runtime's call_fn1 for unary functions
    BValue result = call_fn1(fib_fn, n_val);

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
    // Get the factorial function
    BValue fact_fn = ___bsts_g_Demo_l_Compute_l_factorial();

    // Convert n to BValue
    BValue n_val = bsts_integer_from_int(n);

    // Call the function using the runtime's call_fn1 for unary functions
    BValue result = call_fn1(fact_fn, n_val);

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
