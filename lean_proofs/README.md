# Lean proof: fuel-free `Eval`

This directory contains a machine-checked Lean proof for a `RunEval` / `RunStack`
interpreter that does **not** take a fuel argument.

It also contains a small proof that extending structural descent with trusted
thunk/Lazy force steps preserves well-foundedness (`TrustedForceProjection`).

There is also a follow-on proof that adding trusted `Eval::eval` steps remains
well-founded, and that the `flat_map` callback binder is just the evaluated
input (`TrustedEvalProjection`).

The proof uses a minimal sound extension: each continuation frame carries a static
output-work bound (`BFn α β wOut wFn` with `wOut < wFn`).

With that bound, the evaluator is accepted by Lean using a well-founded `rank`
measure, proving termination of `runLoop`.

It also contains a second proof over a naive `Eval`:

- all values of this `Eval` shape terminate semantically (`terminates_all`)
- if `Eval` terminates semantically (`Evaluates`), then there exists some finite
  fuel `n` such that `runFuel n` returns `some v`
- this is packaged as `exists_fuel_of_terminates` and
  `exists_fuel_if_total`, with direct corollary `exists_fuel_for_all`

Run locally:

```bash
./setup.sh
```
