# Cache Benchmark Summary

Date: 2026-03-01

## Scope

Benchmarked Bosatsu CLI cache behavior for:

- `lib check`
- `lib build`

Cache modes:

- `no_cache` (explicit `--no_cache`)
- `cold_cache` (empty cache dir each run)
- `warm_cache` (cache pre-warmed before timed runs)

## Method

- Runner: `./bosatsuj`
- Runs per mode: 3
- Warm mode includes 1 unmeasured warmup run
- Reported numbers below are median wall-clock seconds

Commands:

- `lib check`
  - `./bosatsuj lib check --repo_root . --quiet --color none --no_cache`
  - add `--cache_dir /tmp/bosatsu_bench_cache_check` for cache modes
- `lib build`
  - `./bosatsuj lib build --repo_root . --main_pack Bosatsu/IO/CreateModeMain --outdir /tmp/bosatsu_bench_build_out --color none --no_cache`
  - add `--cache_dir /tmp/bosatsu_bench_cache_build` for cache modes

## Results (Median of 3)

### lib check

- no_cache: `8.245s`
- cold_cache: `8.716s`
- warm_cache: `5.584s`
- warm vs no_cache: `1.48x` faster (`32.3%` lower)
- warm vs cold_cache: `1.56x` faster (`35.9%` lower)

### lib build

- no_cache: `11.360s`
- cold_cache: `11.515s`
- warm_cache: `7.477s`
- warm vs no_cache: `1.52x` faster (`34.2%` lower)
- warm vs cold_cache: `1.54x` faster (`35.1%` lower)

## Raw Artifacts

- `/tmp/bosatsu_bench_results.tsv`
- `/tmp/bosatsu_bench_results_norm.tsv`
