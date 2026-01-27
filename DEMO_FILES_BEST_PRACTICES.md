# Bosatsu Demo Files: Prevention Strategies and Best Practices

This document provides prevention strategies, testing guidelines, and quick references for creating and maintaining Bosatsu demo files, especially those targeted for C transpilation.

## 1. Prevention Checklist

### Before Adding Demo Files

- [ ] **Main Entry Point Type**: Verify the C transpiler target uses `Main` type from `Bosatsu/Prog`
  - Must import: `from Bosatsu/Prog import Main, Prog, ...`
  - Main expression must wrap a `Prog[List[String], forall e. e, Int]`
  - Pattern: `main = Main(prog_expression)`

- [ ] **All Dependencies Included**: Ensure `Prog.bosatsu` and other shared dependencies are in the project
  - List all imports your demo requires
  - Verify each imported package is available in the workspace
  - Check that transitive dependencies are satisfied

- [ ] **Syntax Validation**: Confirm all Bosatsu language constructs follow correct patterns
  - [ ] No method chaining (`.fst`, `.snd`, `.map`)
  - [ ] All list operations use function calls (`map_List`, `foldl_List`)
  - [ ] Boolean operations use `if/else` or pattern matching
  - [ ] Tuple access uses pattern matching or destructuring

- [ ] **Export Statement**: Verify exports match what is actually defined
  - [ ] All exported names are defined in the file
  - [ ] Main entry point `main` is exported if needed for C transpilation
  - Pattern: `export main, helper_func1, helper_func2`

- [ ] **Package Declaration**: Check package name is appropriate
  - Use hierarchical naming for organization (e.g., `Demo/Fibonacci`)
  - Avoid conflicts with core Bosatsu packages
  - Keep package names descriptive

### Pre-Commit Verification Checklist

- [ ] Run Bosatsu type checker: `bosatsu check <file>`
- [ ] Test with Bosatsu interpreter/transpiler: `bosatsu run <file>`
- [ ] Verify C transpilation if applicable: `bosatsu transpile_to_c <package>`
- [ ] Check that all imports resolve without errors
- [ ] Validate that `Main` struct is properly constructed if targeting C

---

## 2. Testing Strategy

### Local Testing Before Push

#### Step 1: Syntax Validation
```bash
# Check type correctness and syntax
bosatsu check demo/examples/my_demo.bosatsu

# Expected output: No type errors, all imports resolved
```

#### Step 2: Basic Execution Test
```bash
# Run the file through the interpreter
bosatsu run --package Demo/MyDemo

# Should execute without errors
# Should produce expected output if Main is defined
```

#### Step 3: C Transpilation Test (If Applicable)
```bash
# Transpile to C and verify compilation
bosatsu transpile_to_c Demo/MyDemo

# Check:
# - No transpiler errors about type mismatches
# - Main entry point generates valid C code
# - All external references are resolvable
```

#### Step 4: Import Resolution Test
```bash
# Verify all transitive dependencies are available
# Check workspace includes required supporting files like Prog.bosatsu

# For each import statement, verify:
# 1. The package exists in the workspace
# 2. The exported names exist in that package
# 3. Type signatures are compatible
```

#### Step 5: Integration Test
```bash
# If demo is part of a larger workflow:
# 1. Copy demo files to their target location
# 2. Run full workspace type check
# 3. Verify demo compiles in context with other files
# 4. Test C code generation for complete workflow

# Command:
bosatsu check demo/

# Verify no new errors introduced by demo files
```

### Testing Checklist Template

Create a `test_checklist.md` in the PR:

```
## Demo File Testing Checklist

- [ ] Syntax check passes: `bosatsu check`
- [ ] Interpreter execution succeeds: `bosatsu run`
- [ ] C transpilation succeeds (if applicable): `bosatsu transpile_to_c`
- [ ] All imports resolve without errors
- [ ] Main entry point is correct type: `Prog[List[String], forall e. e, Int]`
- [ ] No undefined external references
- [ ] Pattern matching covers all cases
- [ ] List/Bool operations use correct functions (not method chaining)
- [ ] Tested with all dependency files present
```

---

## 3. Common Pitfalls to Avoid

### Pitfall 1: Method Chaining (WRONG)
```bosatsu
# ❌ WRONG - Bosatsu doesn't have method chaining
result = (1, 2).fst
items = data.map(x -> x + 1)
paired = list.zip(other)
```

### Pitfall 2: Correct List Operations (RIGHT)
```bosatsu
# ✓ CORRECT - Use function calls for list operations
items = map_List([1, 2, 3], x -> x + 1)
result = foldl_List(items, 0, add)
sum_val = foldl_List([1, 2, 3], 0, add)

# ✓ CORRECT - Pattern match for tuple access
(first, second) = tuple_pair
(a, b) = acc
```

### Pitfall 3: Boolean Operations (WRONG)
```bosatsu
# ❌ WRONG - No .and, .or methods
result = condition1.and(condition2)
value = option.map(x -> x + 1)
```

### Pitfall 4: Boolean Operations (RIGHT)
```bosatsu
# ✓ CORRECT - Use if/else for boolean logic
result = if condition1: condition2 else: False

# ✓ CORRECT - Use pattern matching for Option
value = match option:
  Some(x): x + 1
  None: 0
```

### Pitfall 5: Prog Type Mismatch (WRONG)
```bosatsu
# ❌ WRONG - Missing or incorrect Main type
my_prog = Prog(println("hello"))  # Wrong struct
main = my_func()  # Not wrapped in Main

# ❌ WRONG - Incorrect type signature
main = Main(pure(0))  # Missing .ignore_env() for env type
```

### Pitfall 6: Correct Main Entry Point (RIGHT)
```bosatsu
# ✓ CORRECT - Proper Main setup for C transpilation
from Bosatsu/Prog import Main, Prog, pure, ignore_env

main = Main(
  (
    result = compute()
    _ <- println("Result: ${int_to_String(result)}").ignore_env().await()
    pure(0)
  ).ignore_env()
)
```

### Pitfall 7: Missing Dependencies (WRONG)
```bosatsu
# ❌ WRONG - Prog.bosatsu not in workspace
# File assumes Bosatsu/Prog exists but it's not included
from Bosatsu/Prog import Main
```

### Pitfall 8: Complete Dependency Chain (RIGHT)
```bash
# ✓ CORRECT - Workspace structure includes all dependencies
demo/
├── Prog.bosatsu          # Required for Main
├── examples/
│   ├── fibonacci.bosatsu
│   ├── compute.bosatsu
│   └── orchestrator.bosatsu
└── predef.bosatsu        # If custom predef needed
```

### Pitfall 9: Pattern Matching Gaps (WRONG)
```bosatsu
# ❌ WRONG - Incomplete pattern matches (if using for logic)
value = match option:
  Some(x): x + 1
  # Missing None case in some contexts

# ❌ WRONG - List patterns without wildcards when needed
process = match items:
  [first]: first
  [first, second]: combine(first, second)
  # Missing empty list and other cases
```

### Pitfall 10: Complete Pattern Matching (RIGHT)
```bosatsu
# ✓ CORRECT - Exhaustive pattern matching
value = match option:
  Some(x): x + 1
  None: 0

# ✓ CORRECT - Handle all list cases
process = match items:
  []: 0
  [single]: single
  [a, b]: combine(a, b)
  [h, *t]: process_recursive(h, t)

# ✓ CORRECT - Use _ for catch-all
default_case = match value:
  Some(x): x
  _: 0
```

---

## 4. Quick Reference: Bosatsu Syntax Patterns

### Package and Imports

```bosatsu
package Demo/MyProject

from Bosatsu/Prog import Main, Prog, pure, println, ignore_env, await
from Bosatsu/Predef import map_List, foldl_List, int_to_String

export main, helper_function
```

### Function Definition

```bosatsu
# Basic function with type signature
def increment(n: Int) -> Int:
  n.add(1)

# Function with multiple parameters
def add_numbers(a: Int, b: Int) -> Int:
  a.add(b)

# Function returning Bool
def is_even(n: Int) -> Bool:
  eq_Int(mod_Int(n, 2), 0)

# Higher-order function (takes function, returns function)
def apply_twice(f: a -> a, x: a) -> a:
  f(f(x))
```

### Tuple Handling

```bosatsu
# Create tuple
pair = (1, 2)
triple = (1, 2, 3)

# Destructure tuple with pattern matching
(first, second) = pair
(a, b, c) = triple

# In function parameters
def get_sum((x, y)):
  x.add(y)

# In loop accumulator
(result, _) = int_loop(10, (0, 1), (i, acc) ->
  (a, b) = acc
  (i.sub(1), (b, a.add(b))))
```

### List Operations

```bosatsu
# Create list
numbers = [1, 2, 3, 4, 5]
empty = []

# List concatenation
combined = [1, 2, *[3, 4]]  # [1, 2, 3, 4]

# Map transformation (NOT .map())
doubled = map_List(numbers, x -> x.times(2))

# Fold/reduce
sum_result = foldl_List(numbers, 0, add)
product = foldl_List(numbers, 1, times)

# Pattern matching on lists
def head_or_default(lst):
  match lst:
    []: 0
    [h, *_]: h

# List comprehension
evens = [x for x in numbers if is_even(x)]
strings = [int_to_String(x) for x in numbers]
```

### Boolean Operations

```bosatsu
# If/else expression (NOT .and() or .or())
result = if condition: value_true else: value_false

# Boolean operators using if/else
def and_op(a, b):
  if a: b else: False

def or_op(a, b):
  if a: True else: b

# Pattern matching for Bool
description = match is_valid:
  True: "valid"
  False: "invalid"

# Multiple conditions
status = if check1: (if check2: "both" else: "first")
         else: "neither"
```

### Pattern Matching

```bosatsu
# Simple value matching
color = match name:
  "red": 255
  "blue": 0
  _: 128

# String interpolation matching
extract = match text:
  "Hello ${greeting}": greeting
  _: ""

# List pattern matching
classify = match items:
  []: "empty"
  [_]: "single"
  [_, _]: "pair"
  [*_]: "many"

# Option/Maybe pattern matching
value = match maybe_val:
  Some(x): x + 1
  None: 0

# Custom type pattern matching
process = match shape:
  Circle(r): r.times(2)
  Square(s): s.times(s)
  _: 0
```

### Prog/Main for C Transpilation

```bosatsu
from Bosatsu/Prog import Main, Prog, pure, println, ignore_env, await

# Simple computation returning 0
main = Main(pure(0).ignore_env())

# With output
main = Main(
  (
    _ <- println("Hello, World!").ignore_env().await()
    pure(0)
  ).ignore_env()
)

# With computation
main = Main(
  (
    result = compute_value()
    _ <- println("Result: ${int_to_String(result)}").ignore_env().await()
    pure(0)
  ).ignore_env()
)

# With multiple steps
main = Main(
  (
    val1 = func1()
    val2 = func2(val1)
    _ <- println("Step 1: ${int_to_String(val1)}").ignore_env().await()
    _ <- println("Step 2: ${int_to_String(val2)}").ignore_env().await()
    pure(0)
  ).ignore_env()
)
```

### Recursive Functions

```bosatsu
# Tail-recursive pattern using recur
def factorial(n: Int) -> Int:
  def fact_helper(n, acc):
    recur n:
      0: acc
      _: fact_helper(n.sub(1), acc.times(n))
  fact_helper(n, 1)

# Recursive list processing
def sum_list(items):
  recur items:
    []: 0
    [h, *t]: h.add(sum_list(t))

# Conditional recursion
def count_down(n):
  if cmp_Int(n, 0) matches (GT | EQ):
    _ <- print("${int_to_String(n)} ").await()
    count_down(n.sub(1))
  else:
    pure(())
```

### int_loop Pattern (Efficient Iteration)

```bosatsu
# int_loop(count, initial_state, step_fn)
# step_fn takes (iteration_counter, current_state) -> new_state

def fibonacci(n: Int) -> Int:
  (result, _) = int_loop(n, (0, 1), (i, acc) ->
    (a, b) = acc
    (i.sub(1), (b, a.add(b))))
  result

def factorial(n: Int) -> Int:
  int_loop(n, 1, (i, acc) ->
    (i.sub(1), acc.times(i)))
```

### Type Annotations

```bosatsu
# Function with full type signature
def process(input: Int) -> String:
  int_to_String(input)

# Generic types
def identity(x: a) -> a:
  x

def apply_to_list(fn: a -> b, items: List[a]) -> List[b]:
  map_List(items, fn)

# Option type
def safe_divide(a: Int, b: Int) -> Option[Int]:
  if eq_Int(b, 0):
    None
  else:
    Some(div(a, b))

# Custom struct types
struct Point(x: Int, y: Int)

def distance((x1, y1), Point(x2, y2)):
  # compute distance
  0
```

### External Functions and Structs

```bosatsu
# External declarations (from Bosatsu/Predef or runtime)
external struct Prog[env: -*, err: +*, res: +*]
external def pure[env, res](a: res) -> Prog[env, forall e. e, res]
external def println(str: String) -> Prog[Unit, forall e. e, Unit]

# These are implemented in the runtime, not Bosatsu
```

---

## 5. C Transpilation Specific Rules

### Main Entry Point Requirements

For C transpilation, the `main` value must have this exact type:

```
Prog[List[String], forall e. e, Int]
```

Breaking this down:
- **Prog[...]**: Wrapped in the `Prog` monad (from `Bosatsu/Prog`)
- **List[String]**: Receives command-line arguments
- **forall e. e**: Error type is polymorphic (never actually errors)
- **Int**: Returns an exit code (typically 0)

### Correct Main Pattern

```bosatsu
from Bosatsu/Prog import Main, Prog, pure, println, ignore_env, await

main = Main(
  prog_expression_here
)
```

Where `prog_expression_here` is of type `Prog[Unit, forall e. e, Int]` (after `.ignore_env()`)

### Common Main Implementations

**Minimal (no side effects):**
```bosatsu
main = Main(pure(0).ignore_env())
```

**With output:**
```bosatsu
main = Main(
  (
    _ <- println("Output here").ignore_env().await()
    pure(0)
  ).ignore_env()
)
```

**With computation:**
```bosatsu
def my_computation() -> Int:
  # compute something
  42

main = Main(
  (
    result = my_computation()
    _ <- println("Result: ${int_to_String(result)}").ignore_env().await()
    pure(0)
  ).ignore_env()
)
```

---

## 6. Workspace Organization Best Practices

### Recommended Structure

```
project/
├── Prog.bosatsu              # Core Prog monad (required for C transpilation)
├── demo/
│   ├── examples/
│   │   ├── fibonacci.bosatsu
│   │   ├── compute.bosatsu
│   │   └── orchestrator.bosatsu
│   └── shared/
│       └── utilities.bosatsu  # Shared code between demos
├── docs/
│   └── DEMO_README.md         # Usage instructions
└── test/
    └── demo_tests.bosatsu     # Test files
```

### Package Naming Convention

- Core packages: `Demo/Core`, `Demo/Utils`
- Example packages: `Demo/Fibonacci`, `Demo/Compute`, `Demo/Orchestrator`
- Shared: `Demo/Shared`

---

## 7. Documentation Requirements

### Each Demo File Should Include

```bosatsu
# 1. Package declaration
package Demo/MyFeature

# 2. Clear imports
from Bosatsu/Prog import Main, Prog, pure, println, ignore_env, await

# 3. Purpose comment
# This demo shows how to [feature description]
# Input: [what it expects]
# Output: [what it produces]

# 4. Implementation with comments
def my_function(input: Type) -> ReturnType:
  # Algorithm explanation
  0

# 5. Main entry point with comments
# Main entry point for C transpiler
main = Main(
  (
    # Computation steps
    result = my_function(input)
    # Output
    _ <- println("Result: ${result}").ignore_env().await()
    pure(0)
  ).ignore_env()
)

# 6. Exports
export main, my_function
```

### Related Documentation Files

- **README.md**: Overview of all demos, how to run them
- **INSTALL.md**: Setup and dependency installation
- **TROUBLESHOOTING.md**: Common errors and solutions
- **ARCHITECTURE.md**: How demos relate to main system

---

## 8. Review Checklist for Code Reviewers

When reviewing demo files:

- [ ] Does the file have correct `package` declaration?
- [ ] Are all imports valid and necessary?
- [ ] Are all exported symbols actually defined?
- [ ] No method chaining (`.map()`, `.fst()`, etc.)?
- [ ] List operations use `map_List`, `foldl_List`, not dot notation?
- [ ] Boolean logic uses `if/else`, not `.and()` or `.or()`?
- [ ] Main entry point exists and has correct type?
- [ ] Pattern matching is exhaustive or uses `_` wildcard?
- [ ] Type signatures are correct and match usage?
- [ ] C transpilation works (if applicable)?
- [ ] All dependencies (especially `Prog.bosatsu`) are included?
- [ ] Comments explain complex logic?
- [ ] No unused imports or exports?

---

## 9. Error Recovery Guide

### Common Errors and Solutions

| Error | Cause | Solution |
|-------|-------|----------|
| "undefined symbol: Main" | Missing import | Add `from Bosatsu/Prog import Main` |
| "method .map not found" | Using method chaining | Use `map_List(items, fn)` instead |
| "type mismatch: expected Prog" | Main not using Prog type | Wrap in `Main(...)` with proper Prog |
| "undefined external reference" | Missing dependency file | Add required `.bosatsu` file to workspace |
| "pattern not exhaustive" | Incomplete match | Add catch-all `_:` case or cover all branches |
| "cyclic dependency" | Circular imports | Refactor to break cycle (move shared code to third file) |
| "cannot find import" | Wrong package name | Check exact package name and path |

---

## Summary

**Before adding demo files:**
1. Verify Main entry point type requirements
2. Ensure all dependencies are included
3. Follow Bosatsu syntax rules (no method chaining, use functions)
4. Write exhaustive pattern matches
5. Test locally with `bosatsu check` and `bosatsu run`

**Syntax essentials:**
- Lists: `map_List()`, `foldl_List()`, pattern match access
- Booleans: `if/else`, pattern match
- Tuples: destructuring with `(a, b) = ...`
- Main: `Main(Prog[List[String], forall e. e, Int])`

**Common mistakes to avoid:**
- Method chaining (`.map()`, `.fst()`)
- Missing `ignore_env()` and `.await()`
- Incomplete pattern matches
- Missing dependency files
- Wrong Main type signature
