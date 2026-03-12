# Lean proof: fuel-free `Eval`

This directory contains a machine-checked Lean proof for a `RunEval` / `RunStack`
interpreter that does **not** take a fuel argument.

The proof uses a minimal sound extension: each continuation frame carries a static
output-work bound (`BFn α β wOut wFn` with `wOut < wFn`).

With that bound, the evaluator is accepted by Lean using a well-founded `rank`
measure, proving termination of `runLoop`.

Run locally:

```bash
./setup.sh
```
