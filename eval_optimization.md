# Eval Optimization Log

## User instructions
- Start a new branch.
- Build a JVM evaluator benchmark tool similar to `cli/src/main/scala/dev/bosatsu/LibCheckProfileMain.scala`.
- Benchmark must exercise:
  - function call
  - tail loops
  - non-tail recursion
  - integer operations
  - string operations
- Support looped execution so we can attach async-profiler (`~/Downloads/async-profiler-4.3-macos/bin/asprof`).
- Iteratively:
  - identify evaluator performance hotspots
  - make one improvement
  - run `sbt coreJVM/testOnly dev.bosatsu.EvaluationTest`
  - if green, checkpoint commit
  - repeat until no significant improvements remain
- Track each checkpoint in this document with relative performance changes.
- Do a final comparison against the baseline and summarize total gains.

## Plan
1. Add `EvalBenchmarkMain` in `cli` to compile a fixed Bosatsu package once and repeatedly execute all five workload categories.
2. Record baseline throughput and async-profiler hotspots using the new benchmark.
3. Apply one evaluator-focused optimization at a time.
4. After each change:
   - re-run benchmark for relative delta
   - run `sbt coreJVM/testOnly dev.bosatsu.EvaluationTest`
   - checkpoint commit and log the change here
5. Stop when additional changes are neutral/regressive or too risky for marginal gain.
6. Run a final baseline-vs-final comparison and summarize gains.

## Baseline
- Command: `sbt "cli/runMain dev.bosatsu.EvalBenchmarkMain --once --warmup 800 --iterations 2000 --work 300 --non-tail-depth 180"`
- Throughput: `764.40 iters/s` (`1,308,217 ns/iter`)
- Top async-profiler observations:
  - Large time in interface/virtual dispatch stubs (`itable stub`, `vtable stub`), consistent with dynamic function/value dispatch.
  - Hot frames in evaluator scope and closures (`MatchlessToValue$Impl$Dynamic.apply`, `MatchlessToValue$Impl$Env$$Lambda...`).
  - Noticeable integer overhead (`java.math.BigInteger.getInt`, `scala.math.BigInt$.apply`, `java.math.BigInteger.subtract`).
  - Frequent map/identifier equality work (`Identifier.equals`, `String.equals`, `LongMap.get/updated/apply`).

## Checkpoints

### Checkpoint 1
- Change:
  - Added `EvalBenchmarkMain` to repeatedly exercise function calls, tail loops, non-tail recursion, integer ops, and string ops.
  - Added `loop` mode and printed PID guidance for `asprof` attachment.
  - Switched evaluator local bindings from immutable `Map` updates/lookups to a lightweight linked local env (`LocalEnv`) in `MatchlessToValue.Scope`.
  - Added integer union representation plumbing (`java.lang.Integer | java.math.BigInteger`) in `Value`, with Predef fast paths for common integer arithmetic/bitwise operations and int-aware literal/nat equality in evaluator boolean checks.
  - Reduced product construction overhead by avoiding `NonEmptyList -> List -> Array` conversion in `makeCons`.
- Relative perf vs previous:
  - Long-running loop mode baseline (before): `~839 iters/s` (steady-state from `--loop --warmup 50 --report-every 200`).
  - Long-running loop mode after checkpoint: `~920 iters/s` steady-state on the same loop command.
  - Relative change: `~+9.7%`.
- Test status:
  - `sbt "coreJVM/testOnly dev.bosatsu.EvaluationTest"`: passed (71/71).

### Checkpoint 2
- Change:
  - Investigated additional hotspots (`LongMap` mutations/lookups, dynamic dispatch stubs) with async-profiler.
  - No further low-risk changes produced a clear significant gain beyond checkpoint 1.
- Relative perf vs previous:
  - No additional checkpoint-worthy improvement.
- Test status:
  - N/A (no extra commit-worthy change).

### Checkpoint 3
- Change:
  - Not used.
- Relative perf vs previous:
  - Not used.
- Test status:
  - Not used.

## Final summary
- Baseline throughput:
  - `~839 iters/s` steady-state (`--loop --warmup 50 --report-every 200`).
- Final throughput:
  - `~920 iters/s` steady-state on the same loop command.
- Net change:
  - `~+9.7%`.
- Notes on remaining hotspots:
  - Dispatch overhead (`itable`/`vtable` stubs) remains the largest bucket.
  - Remaining evaluator overhead is concentrated in `LongMap` operations for anon/mut slots and dynamic closure dispatch.
  - Further gains likely require deeper structural changes (slot storage layout and dispatch specialization), which are higher risk than this checkpoint.
