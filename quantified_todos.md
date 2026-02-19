# Quantifier Evidence TODOs

- [x] 1. Add `TypedExpr.QuantifierEvidence` and extend `TypedExpr.Annotation` (no default args), including `Domain.Annotation` signatures.
- [x] 2. Update all `TypedExpr.Annotation` constructors/pattern matches across core code so evidence is preserved or explicitly set (`Some`/`None`).
- [x] 3. Emit quantifier evidence at inference construction sites that already solve instantiation (`subsInstantiate`, apply fast paths).
- [x] 4. Improve `TypedExpr.instantiateTo` to consume available evidence before recomputing; keep sound fallback.
- [x] 5. Improve `TypedExpr.coerceRho` / `coerceFn1` and related Generic paths to leverage the new instantiate behavior without losing evidence.
- [x] 6. Update normalization and lambda-resolution consumption paths to use annotation evidence and avoid redundant solving.
- [x] 7. Update protobuf schema compatibly and wire encode/decode in `ProtoConverter`.
- [x] 8. Update show/EDN printing to distinguish `(widen ...)` vs `(instantiate ...)` and include evidence where present.
- [x] 9. Update tests/generators for constructor changes and add/adjust evidence-related coverage; run targeted validation.
- [x] 10. Add targeted regressions for instantiate interactions with `env`/`err` variable positions plus skolem/meta targets to lock long-standing instantiate behavior.
- [x] 11. Verify `zonk`/`quantify` process quantifier-evidence-carried metas and existential skolems; add direct `TypedExprTest` coverage.
- [ ] 12. Isolate remaining regression root cause in `Infer` annotation-check path (`checkSigma`/`subsUpper`/`unskolemize`) with targeted tests that avoid `await`/extra combinators.
- [ ] 13. Add bisect tests to pinpoint the earliest incorrect type shape for `ignore_err` (`forall a. Prog[a, a, res]`) and distinguish declaration-site vs call-site corruption.
- [ ] 14. Add scoped debug tracing (behind a local flag) for `checkSigma` + `subsUpper` + `Type.instantiate` transitions, including skolem/meta sets before and after `zonk`/`quantify`.
- [ ] 15. Fix the inference bug (compiler-side only) so `ignore_err` preserves distinct `env` and quantified `e` through declaration inference and downstream uses.
- [ ] 16. Enforce and verify invariant: no `Type.TyMeta` and no `Type.Var.Skolem` in final inferred `TypedExpr` across core + cli test paths.
- [ ] 17. Re-run `coreJVM/test` and `cli/test`, remove temporary debug code, and mark all checklist items complete.

## Notes

- Primary goal: retain and leverage quantifier information as far as possible, only falling back to recomputation when evidence is absent or invalid.
- Validation run: `sbt -batch compile`, `sbt -batch 'coreJVM/testOnly dev.bosatsu.TypedExprTest'`, `sbt -batch 'coreJVM/testOnly dev.bosatsu.Issue1633Test dev.bosatsu.Issue1654Test dev.bosatsu.rankn.TypeTest'`, and `sbt -batch 'coreJVM/Test/compile'`.
- Additional targeted validation: `sbt -batch 'coreJVM/testOnly dev.bosatsu.TypedExprTest'` now includes:
  - `TypedExpr.zonkMeta zonks quantifier evidence types`
  - `TypedExpr.quantify handles metas and skolems inside quantifier evidence`
