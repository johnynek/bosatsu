# Brainstorm: Principled Input Semantics for Bosatsu Simulations

**Date:** 2026-01-28
**Status:** Design needed

## Problem

The current simulation design has a semantic contradiction:

```bosatsu
# Current (WRONG)
principal = 250000  # This IS 250000, always
monthly = principal.times(rate)
```

But then the UI allows users to "change" `principal`. This violates Bosatsu's explicit semantics - if `principal = 250000`, then principal IS 250000. It can't also be "whatever the user enters".

## Key Insight from User

> "bosatsu is very explicit. so it does not make sense to say `inputA = 5` and then do a bunch of stuff with that variable and then later say 'oh, but the ui/config may make inputA not 5' because if bosatsu says a variable is a number and it comes to a certain value, then that's just its value."

## Principled Approaches

### Option A: Functions (Simplest)

The simulation is a **function** from inputs to outputs:

```bosatsu
package LoanCalculator

# The simulation is a pure function
def calculate(principal: Int, annual_rate: Int, years: Int) -> {
  monthly_payment: Int,
  total_interest: Int
}:
  monthly_rate = annual_rate.div(1200)
  num_payments = years.times(12)
  monthly_payment = principal.times(monthly_rate).add(principal.div(num_payments))
  total_paid = monthly_payment.times(num_payments)
  total_interest = total_paid.sub(principal)
  { monthly_payment, total_interest }

# Inputs with defaults are defined in config, not code
# The config specifies: principal default=250000, annual_rate default=7, years default=30
```

**Pros:**
- Semantically correct - `principal` is a parameter, not a fixed value
- Natural Bosatsu idiom
- Clear distinction between inputs and outputs
- Pure functions are easy to reason about

**Cons:**
- Need config file to specify defaults, ranges, labels
- All inputs must be function parameters (no intermediate assumptions)

### Option B: Input Monad/External

```bosatsu
package LoanCalculator

from Bosatsu/Simulation import Input, input

# Input is an external that means "this value comes from outside"
principal: Input[Int] = input("principal", default=250000, min=0, max=1000000)
annual_rate: Input[Int] = input("annual_rate", default=7, min=0, max=100)
years: Input[Int] = input("years", default=30, min=1, max=50)

# Now we use inputs in expressions
monthly_rate = annual_rate.value.div(1200)
...
```

**Pros:**
- Can annotate inputs with metadata (defaults, ranges, labels)
- Works with top-level bindings (not just function params)
- Could support intermediate "assumption" toggles

**Cons:**
- More complex type system
- `Input[T]` vs `T` type distinction adds friction
- Requires new module and semantics

### Option C: Record-Based State (BurritoScript Pattern)

```bosatsu
package LoanCalculator

struct Inputs:
  principal: Int
  annual_rate: Int
  years: Int

struct Outputs:
  monthly_payment: Int
  total_interest: Int

def simulate(inputs: Inputs) -> Outputs:
  monthly_rate = inputs.annual_rate.div(1200)
  ...
```

**Pros:**
- Clear input/output contracts
- Struct fields can have metadata annotations
- Matches BurritoScript pattern

**Cons:**
- More verbose for simple simulations
- Still need config for defaults/ranges

## Recommendation: Option A (Functions)

The simplest principled approach:

1. **Simulation is a function** - Parameters ARE the inputs
2. **Config file defines UI metadata** - Defaults, ranges, labels, widgets
3. **Return type defines outputs** - What gets displayed

### Example

**loan_calculator.bosatsu:**
```bosatsu
package LoanCalculator

def calculate(
  principal: Int,    # Loan amount
  annual_rate: Int,  # Interest rate (percent * 100)
  years: Int         # Loan term
) -> {
  monthly_payment: Int,
  total_interest: Int,
  interest_ratio: Int
}:
  monthly_rate = annual_rate.div(1200)
  num_payments = years.times(12)
  monthly_payment = principal.times(monthly_rate).add(principal.div(num_payments))
  total_paid = monthly_payment.times(num_payments)
  total_interest = total_paid.sub(principal)
  interest_ratio = total_interest.times(100).div(principal)
  { monthly_payment, total_interest, interest_ratio }
```

**loan_calculator.sim.json:**
```json
{
  "name": "Loan Calculator",
  "package": "LoanCalculator",
  "function": "calculate",
  "inputs": {
    "principal": {
      "label": "Loan Principal ($)",
      "default": 250000,
      "min": 10000,
      "max": 1000000,
      "step": 10000,
      "widget": "slider"
    },
    "annual_rate": {
      "label": "Interest Rate (%)",
      "default": 700,
      "min": 0,
      "max": 2000,
      "step": 25,
      "widget": "slider",
      "format": { "divisor": 100, "suffix": "%" }
    },
    "years": {
      "label": "Loan Term",
      "default": 30,
      "min": 5,
      "max": 40,
      "step": 5,
      "widget": "slider",
      "format": { "suffix": " years" }
    }
  },
  "outputs": {
    "monthly_payment": {
      "label": "Monthly Payment",
      "format": "currency"
    },
    "total_interest": {
      "label": "Total Interest",
      "format": "currency"
    },
    "interest_ratio": {
      "label": "Interest Ratio",
      "format": { "suffix": "%" }
    }
  }
}
```

## Open Questions

1. **Intermediate values** - How do we expose `monthly_rate` for "Why?" explanations?
   - Option: Return all values of interest in the output record
   - Option: Static analysis extracts intermediate computations

2. **Branching/assumptions** - How do we represent "what if X were true?"
   - Option: Boolean parameters with If expressions
   - Option: Separate `Assumption` type

3. **Provenance** - Where does "Why?" explanation data come from?
   - Static analysis of the function body
   - Type-preserving IR that keeps the computation graph

## Better Approach: Bosatsu Config Files

Instead of `.json` config files, use `.bosatsu` files for configuration. This has several advantages:

1. **Type-checked config** - Configuration errors caught at compile time
2. **Single language** - No context switching between Bosatsu and JSON
3. **Richer types** - Can use structs, enums, etc.
4. **Comments** - Bosatsu supports documentation comments

### Example Config as Bosatsu

**loan_calculator_func.sim.bosatsu:**
```bosatsu
package LoanCalculator/Config

struct InputConfig(
  label: String,
  default_value: Int,
  min_value: Int,
  max_value: Int,
  step: Int,
  widget: String
)

struct SimConfig(
  name: String,
  package_name: String,
  function_name: String,
  inputs: List[(String, InputConfig)],
  outputs: List[(String, OutputConfig)]
)

config = SimConfig(
  "Loan Calculator",
  "LoanCalculator",
  "calculate",
  [
    ("principal", InputConfig("Loan Principal ($)", 250000, 10000, 1000000, 10000, "slider")),
    ("annual_rate", InputConfig("Interest Rate", 700, 100, 2000, 25, "slider")),
    ("years", InputConfig("Loan Term", 30, 5, 40, 5, "slider"))
  ],
  ...
)
```

## Static Analysis with TypedExpr + IO Monad

The user's insight: In BurritoScript we used async/await heavily. In Bosatsu, we have TypedExpr with a trusted type system, so we can use the IO monad pattern for static analysis.

**Key insight:** TypedExpr already contains all the type information. We don't need runtime dependency tracking - we can extract it statically from TypedExpr.

```scala
// TypedExpr.freeVarsSet already exists!
// This gives us dependencies for any expression.
val deps = TypedExpr.freeVarsSet(List(expr)).intersect(knownBindings)

// The function body's TypedExpr IS the computation graph
// We can extract provenance directly from it
```

## Next Steps

1. Update the plan to use function-based simulations
2. Design the Bosatsu config file format (struct definitions)
3. Update SimulationGen to:
   - Find the simulation function by reading config
   - Compile config alongside simulation
   - Generate JS that calls function with input values
   - Extract computation graph for "Why?" from TypedExpr (using freeVarsSet)
