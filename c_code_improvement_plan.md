# C Code Improvement Plan

## Goal
Improve generated C quality in low-risk, high-leverage ways using short, measurable iterations.

## Methodology (Read Before Each Backlog Item)
1. Re-read this methodology section before starting the next item.
2. Capture a baseline from current `HEAD`:
   - Transpile full test workspace C corpus with `--emitmode all`.
   - Record wall-clock transpile time.
   - Record output metrics from `output.c`:
     - `lines`, `fn_defs`, `while`, `if`, `call_fn*`, `alloc_closure*`, `alloc_boxed_pure_fn*`, `read_or_build`, `alloc_enum*`, `alloc_struct*`.
3. Implement exactly one backlog item.
4. Rebuild compiler before using `bosatsuj` if core/cli code changed:
   - `sbt cli/assembly`
5. Re-run the same measurement commands and compare against baseline.
6. Run correctness checks relevant to touched code.
   - Required: `./bosatsuj lib test` (compiles/runs C-backed lib tests).
   - Optional supplemental: `./testc`, plus targeted Scala tests if needed.
7. Decide:
   - Keep change if no correctness regressions and quality is neutral-or-better.
   - Revert change if regressive or unclear.
8. Move the item to **Completed** with outcome notes before starting the next item.

## Measurement Commands
- Transpile timing:
  - `/usr/bin/time -p ./bosatsuj tool transpile --input_all_subdir test_workspace/ --package_root test_workspace/ c --test --emitmode all --outdir <OUTDIR>`
- Metrics extraction from `<OUTDIR>/output.c`:
  - `wc -l`
  - `grep -Ec '^BValue [A-Za-z0-9_]+\([^;]*\) \{'`
  - `grep -oF 'while (' | wc -l`
  - `grep -oF 'if (' | wc -l`
  - `grep -oE 'call_fn[0-9]+\(' | wc -l`
  - `grep -oE 'alloc_closure[0-9]+\(' | wc -l`
  - `grep -oE 'alloc_boxed_pure_fn[0-9]+\(' | wc -l`
  - `grep -oF 'read_or_build(' | wc -l`
  - `grep -oE 'alloc_enum[0-9]+\(' | wc -l`
  - `grep -oE 'alloc_struct[0-9]+\(' | wc -l`
- Correctness:
  - `./bosatsuj lib test`
  - `./testc`
  - targeted Scala tests for touched area via `sbt coreJVM/testOnly ...`

## Backlog
- None.

## Completed
1. **CGEN-01 Direct Alias Calls** (kept)
   - Change: ClangGen `handleLet` now preserves direct-call metadata for `let f = Global(...)` aliases.
   - Correctness: `./testc` passed.
   - Measurement note: initial before/after baseline crossed a tool rebuild boundary, so item-level metric delta is not fully attributable.
   - Observed trend after rebuild: no `call_fn*` drop on the current corpus, but change is semantics-preserving and enables direct calls when this alias pattern appears.

2. **MATCH-01 Ortho Threshold Tuning** (reverted)
   - Tried `orthoThreshold: 4 -> 3` in Matchless.
   - Baseline (`tmp/c_codegen_item2_before`): `real 18.70`, `lines 32224`, `fn_defs 596`, `while 145`, `if 1177`, `call_fn 280`, `alloc_closure 117`, `alloc_boxed_pure_fn 270`, `read_or_build 354`, `alloc_enum 5747`, `alloc_struct 397`.
   - Candidate (`tmp/c_codegen_item2_after`): `real 23.49`, all structural metrics unchanged.
   - Correctness: `./bosatsuj lib test` passed (3913/3913), `./testc` passed.
   - Decision: reverted due to no measurable C-output quality gain on corpus.

3. **LOOP-01 Hoist Weight Threshold Tuning** (reverted)
   - Tried `HoistInvariantLoopLetsMinWeight: 4 -> 3` in Matchless.
   - Baseline (`tmp/c_codegen_item3_before`): `real 8.25`, `lines 32224`, `fn_defs 596`, `while 145`, `if 1177`, `call_fn 280`, `alloc_closure 117`, `alloc_boxed_pure_fn 270`, `read_or_build 354`, `alloc_enum 5747`, `alloc_struct 397`.
   - Candidate (`tmp/c_codegen_item3_after`): `real 8.69`, all structural metrics unchanged.
   - Correctness: `./bosatsuj lib test` passed (3913/3913), `./testc` passed.
   - Decision: reverted due to no measurable C-output quality gain on corpus.
