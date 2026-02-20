# Minimal IO Functions for Basic Tools (`core_alpha` + `Prog`)

Status: proposed  
Date: 2026-02-20  
Issue: <https://github.com/johnynek/bosatsu/issues/1744>  
PR: TBD

## Problem
`core_alpha` currently exposes only `Bosatsu/IO/Std` (`print`, `println`, stderr variants, and `read_stdin_utf8_bytes`).

That is enough for simple CLI output/input, but not enough for basic tool-building tasks (files, directories, process spawning, env lookup, time, sleep).

The target API is a minimal 21-primitive "core IO" surface, adapted to Bosatsu naming/style and implemented uniformly on JVM evaluator runtime, transpiled Python runtime, and C runtime.

## Decision Summary
1. Add a new package `Bosatsu/IO/Core` in `core_alpha` containing the 21 primitives and related IO types.
2. Keep `Bosatsu/Prog` as the single effect boundary; every effectful operation returns `Prog[env, IOError, a]`.
3. Keep `Bosatsu/IO/Error::IOError` as the shared error channel.
4. Keep existing `Bosatsu/IO/Std` as a compatibility layer, and re-implement it on top of `Bosatsu/IO/Core` handles.
5. Do not add new Predef builtins for this; put all new API types in `core_alpha` packages.

## Where in `core_alpha`
### New package
1. `test_workspace/Bosatsu/IO/Core.bosatsu`

### Existing package updates
1. `test_workspace/Bosatsu/IO/Std.bosatsu`: wrappers over `Core` (`stdout`, `stderr`, `write_text`, `read_text`, `flush`).
2. `test_workspace/core_alpha_conf.json`: add `Bosatsu/IO/Core` to `exported_packages`.

### Runtime implementation files
1. JVM evaluator: `core/src/main/scala/dev/bosatsu/Predef.scala`
2. Python runtime: `test_workspace/ProgExt.py`, `test_workspace/Prog.bosatsu_externals`
3. C runtime: new `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and `.h`, plus `c_runtime/Makefile`

## Bosatsu API Shape (adapted)
Naming follows current stdlib style (`snake_case`, `*_impl` externals wrapped by exported defs).

```bosatsu
package Bosatsu/IO/Core

from Bosatsu/Prog import Prog, read_env
from Bosatsu/IO/Error import IOError

export (
  Handle,
  Process,
  Instant(),
  Duration(),
  FileKind(),
  FileStat(),
  OpenMode(),
  Stdio(),
  StdioConfig(),
  SpawnResult(),
  stdin,
  stdout,
  stderr,
  read_text,
  write_text,
  flush,
  close,
  open_file,
  list_dir,
  stat,
  mkdir,
  remove,
  rename,
  argv,
  get_env,
  exit,
  spawn,
  wait,
  now_wall,
  now_mono,
  sleep,
)

external struct Handle
external struct Process

# keep these as domain structs (not Predef builtins)
struct Instant(epoch_millis: Int)
struct Duration(millis: Int)

enum FileKind:
  File
  Dir
  Symlink
  Other

struct FileStat(kind: FileKind, size_bytes: Int, mtime: Instant)

enum OpenMode:
  Read
  WriteTruncate
  Append

enum Stdio:
  Inherit
  Pipe
  Null
  UseHandle(handle: Handle)

struct StdioConfig(stdin: Stdio, stdout: Stdio, stderr: Stdio)

struct SpawnResult(
  proc: Process,
  stdin: Option[Handle],
  stdout: Option[Handle],
  stderr: Option[Handle],
)

external stdin: Handle
external stdout: Handle
external stderr: Handle

external def read_text_impl[env](h: Handle, max_chars: Int) -> Prog[env, IOError, Option[String]]
external def write_text_impl[env](h: Handle, s: String) -> Prog[env, IOError, Unit]
external def flush_impl[env](h: Handle) -> Prog[env, IOError, Unit]
external def close_impl[env](h: Handle) -> Prog[env, IOError, Unit]
external def open_file_impl[env](path: String, mode: OpenMode) -> Prog[env, IOError, Handle]
external def list_dir_impl[env](path: String) -> Prog[env, IOError, List[String]]
external def stat_impl[env](path: String) -> Prog[env, IOError, Option[FileStat]]
external def mkdir_impl[env](path: String, recursive: Bool) -> Prog[env, IOError, Unit]
external def remove_impl[env](path: String, recursive: Bool) -> Prog[env, IOError, Unit]
external def rename_impl[env](from: String, to: String) -> Prog[env, IOError, Unit]
external def get_env_impl[env](name: String) -> Prog[env, IOError, Option[String]]
external def exit_impl[env, a](code: Int) -> Prog[env, IOError, a]
external def spawn_impl[env](cmd: String, args: List[String], stdio: StdioConfig) -> Prog[env, IOError, SpawnResult]
external def wait_impl[env](p: Process) -> Prog[env, IOError, Int]
external def now_wall_impl[env]() -> Prog[env, IOError, Instant]
external def now_mono_impl[env]() -> Prog[env, IOError, Duration]
external def sleep_impl[env](d: Duration) -> Prog[env, IOError, Unit]

def read_text[env](h: Handle, max_chars: Int) -> Prog[env, IOError, Option[String]]: read_text_impl(h, max_chars)
def write_text[env](h: Handle, s: String) -> Prog[env, IOError, Unit]: write_text_impl(h, s)
def flush[env](h: Handle) -> Prog[env, IOError, Unit]: flush_impl(h)
def close[env](h: Handle) -> Prog[env, IOError, Unit]: close_impl(h)
def open_file[env](path: String, mode: OpenMode) -> Prog[env, IOError, Handle]: open_file_impl(path, mode)
def list_dir[env](path: String) -> Prog[env, IOError, List[String]]: list_dir_impl(path)
def stat[env](path: String) -> Prog[env, IOError, Option[FileStat]]: stat_impl(path)
def mkdir[env](path: String, recursive: Bool) -> Prog[env, IOError, Unit]: mkdir_impl(path, recursive)
def remove[env](path: String, recursive: Bool) -> Prog[env, IOError, Unit]: remove_impl(path, recursive)
def rename[env](from: String, to: String) -> Prog[env, IOError, Unit]: rename_impl(from, to)
def argv[err]() -> Prog[List[String], err, List[String]]: read_env

def get_env[env](name: String) -> Prog[env, IOError, Option[String]]: get_env_impl(name)
def exit[env, a](code: Int) -> Prog[env, IOError, a]: exit_impl(code)
def spawn[env](cmd: String, args: List[String], stdio: StdioConfig) -> Prog[env, IOError, SpawnResult]: spawn_impl(cmd, args, stdio)
def wait[env](p: Process) -> Prog[env, IOError, Int]: wait_impl(p)
def now_wall[env]() -> Prog[env, IOError, Instant]: now_wall_impl()
def now_mono[env]() -> Prog[env, IOError, Duration]: now_mono_impl()
def sleep[env](d: Duration) -> Prog[env, IOError, Unit]: sleep_impl(d)
```

## Mapping from the 21 requested primitives
1. `stdin` -> `Bosatsu/IO/Core::stdin`
2. `stdout` -> `Bosatsu/IO/Core::stdout`
3. `stderr` -> `Bosatsu/IO/Core::stderr`
4. `readText` -> `read_text`
5. `writeText` -> `write_text`
6. `flush` -> `flush`
7. `close` -> `close`
8. `openFile` -> `open_file`
9. `listDir` -> `list_dir`
10. `stat` -> `stat`
11. `mkdir` -> `mkdir`
12. `remove` -> `remove`
13. `rename` -> `rename`
14. `argv` -> `argv` (derived from `Bosatsu/Prog::read_env`)
15. `getEnv` -> `get_env`
16. `exit` -> `exit`
17. `spawn` -> `spawn`
18. `wait` -> `wait`
19. `nowWall` -> `now_wall`
20. `nowMono` -> `now_mono`
21. `sleep` -> `sleep`

## Type mapping in Bosatsu / Predef / core_alpha
1. `String`, `Int`, `Bool`, `List[a]`, `Option[a]`, `Unit` map to existing `Bosatsu/Predef` builtins.
2. `Path` maps to `String` in v1 (no new alias feature required).
3. `Int64 sizeBytes` maps to `Int` (Bosatsu `Int` is arbitrary precision; runtimes convert native 64-bit values).
4. `Instant` maps to new `struct Instant(epoch_millis: Int)` in `Bosatsu/IO/Core`.
5. `Duration` maps to new `struct Duration(millis: Int)` in `Bosatsu/IO/Core`.
6. `FileKind`, `FileStat`, `OpenMode`, `Stdio`, `StdioConfig`, `SpawnResult` are new `enum`/`struct` types in `Bosatsu/IO/Core`.
7. `Handle` and `Process` map to opaque `external struct` types in `Bosatsu/IO/Core`.
8. `IO[Never]` for `exit` maps to polymorphic result: `forall a. Prog[env, IOError, a]`.

## Runtime semantics (shared contract)
1. `read_text` returns `None` only for EOF; otherwise `Some(chunk)` where chunk length is `1..max_chars`.
2. `read_text(max_chars <= 0)` returns `InvalidArgument`.
3. `list_dir` returns entry names (not absolute paths), sorted lexicographically for deterministic behavior.
4. `stat` returns `None` for missing path, `Some(FileStat(...))` otherwise.
5. `stat.kind` is `Symlink` when path itself is a symlink (`lstat`-style classification).
6. `remove(recursive = true)` removes directory trees without following symlinks.
7. `spawn` never invokes a shell; `cmd` + `args` are executed directly.
8. `wait` is idempotent: once complete, repeated waits return the same exit code.
9. time unit is milliseconds for both `Instant` and `Duration` in v1.
10. `sleep(Duration(ms < 0))` returns `InvalidArgument`.

## Java/Python/C implementation plan

### JVM (`Predef.scala`)
1. Extend `jvmExternals` with all `Bosatsu/IO/Core::*_impl` bindings.
2. Add internal runtime objects:
   1. `HandleValue` (`stdin`, `stdout`, `stderr`, file, child pipe read/write)
   2. `ProcessValue` (wrap `java.lang.Process`, cached exit status)
3. Implement IO effects using `prog_effect` dispatch:
   1. Files: `java.nio.file.Files` + `java.io` streams
   2. Process: `ProcessBuilder` + redirected streams
   3. Env: `System.getenv`
   4. Time: `System.currentTimeMillis`, `System.nanoTime` (converted to ms duration)
   5. Sleep: `Thread.sleep`
4. Keep existing `ProgRunResult` testability by preserving capture-mode behavior for stdio when run under evaluator tests.

### Python (`ProgExt.py` + externals map)
1. Add runtime classes for `Handle` and `Process` wrapper values.
2. Add new `ProgExt` constructors returning `effect(...)` thunks for each primitive.
3. Implement using stdlib:
   1. Files/dirs/stat/remove/rename: `os`, `pathlib`, `shutil`
   2. Spawn/wait/pipes: `subprocess.Popen`
   3. Time/sleep: `time.time_ns`, `time.monotonic_ns`, `time.sleep`
   4. Env: `os.environ.get`
4. Update `test_workspace/Prog.bosatsu_externals` for `Bosatsu/IO/Core` symbol remapping.

### C (`c_runtime`)
1. Add `bosatsu_ext_Bosatsu_l_IO_l_Core.c/.h` and include in build/install targets.
2. Represent opaque runtime handles/processes as `alloc_external(...)` payloads.
3. Implement using POSIX APIs first (same approach as current IO/Error errno mapping):
   1. Files/dirs/stat/remove/rename: `open/fopen`, `readdir`, `lstat`, `mkdir`, `unlink/rmdir`, `rename`
   2. Spawn/wait/pipes: `fork/execvp/pipe/waitpid` (or `posix_spawn` variant)
   3. Env: `getenv`
   4. Time/sleep: `clock_gettime`, `nanosleep`
4. Reuse and extend current errno-to-`IOError` mapper (`bosatsu_ext_Bosatsu_l_IO_l_Std.c`) into shared helpers for `IO/Core`.

## Compatibility and migration
1. `Bosatsu/IO/Std` remains source-compatible.
2. Existing `print`/`println` behavior is preserved.
3. New code can choose either high-level `IO/Std` or low-level `IO/Core`.
4. No changes to `Bosatsu/Prog` type shape are required.

## Tests and conformance
1. Add package-level evaluator tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` for each primitive family.
2. Add Python transpile + execute tests using `Prog.bosatsu_externals` mappings.
3. Add C transpile + runtime tests linked against `c_runtime` for file/process/time operations.
4. Add cross-runtime parity tests for:
   1. EOF semantics
   2. error tag parity (`IOError` variants)
   3. spawn stdio pipe behavior
   4. time/sleep minimum behavior

## Rollout
1. Land this design doc.
2. Open issue: `add minimal set of IO functions for basic tools`.
3. Implement `Bosatsu/IO/Core` + runtime bindings in JVM/Python/C.
4. Rebase `Bosatsu/IO/Std` on `IO/Core` wrappers.
5. Regenerate core_alpha docs and release a new `core_alpha` version.
