# Libuv-Based IO Design for `Bosatsu/Prog`

Status: proposed  
Date: 2026-02-19

## Goal

Add a production IO runtime for Bosatsu centered on libuv (for the C backend), with matching semantics on Python and JVM interpreter backends, while preserving Bosatsu's purity model:

1. user programs build `Prog` values purely,
2. no IO occurs while building those values,
3. all impurity happens only when a runtime executes `Prog`.

This design also adds cross-platform HTTPS + JSON support for writing web service clients and servers in Bosatsu.

## Non-goals

1. Replacing `Prog` with another effect type.
2. Requiring Python or JVM to use libuv internally.
3. HTTP/2 or gRPC in the first phase.

## Current Baseline

`Prog` is an external type with constructors/combinators in `test_workspace/Prog.bosatsu`.  
Execution is performed by runtime interpreters:

1. JVM interpreter runtime in `core/src/main/scala/dev/bosatsu/Predef.scala` (`run_prog`, `runProgMain`).
2. C runtime interpreter in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` (`bsts_Bosatsu_Prog_run_main`).
3. Python runtime shim in `test_workspace/ProgExt.py` (`run`).

These backends already share a tag-level `Prog` encoding (`Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, `ReadEnv`, `RemapEnv`, `Effect`), plus synchronized IO error tags (`Bosatsu/IO/Error`).

## Core Design Principles

## 1. Keep `Prog` as the only impurity boundary

All new IO APIs return `Prog[...]`. External defs must be constructors only.

Forbidden:
1. opening sockets/files during external-def construction,
2. performing DNS/TLS/HTTP work before `Prog` is interpreted.

Required:
1. build an effect description,
2. execute it only when interpreter reaches the `Effect` step.

## 2. One semantic contract, three runtime implementations

Semantics are specified once (operation behavior, errors, cancellation, timeout rules), then implemented in:

1. C + libuv (+ TLS + HTTP parser),
2. Python runtime,
3. JVM interpreter runtime.

## 3. Backward compatibility first

Existing `Prog` programs (`print`, `println`, `read_stdin_utf8_bytes`) keep behavior unchanged.

## Runtime Architecture

## A. Introduce an internal IO operation schema

Define a canonical schema file (for example `runtime/prog_io_schema.toml`) containing:

1. operation ids and argument/result shapes,
2. error enums and tag ids,
3. version number.

Generate backend constants from this schema:

1. Scala constants for `PredefImpl`,
2. C enums/header constants,
3. Python constants.

This removes manual drift of tags across backends.

## B. Keep `ProgTagEffect` but standardize payloads

Do not change `Prog` surface API initially.  
Instead, standardize `Effect` payloads as known runtime operations:

1. each external API constructs an operation payload,
2. interpreter dispatches by operation id,
3. operation handler performs side effects and returns `Pure` or `Raise`.

This keeps current `Prog` execution model and limits migration risk.

## C. Explicit runtime context

Each backend runtime keeps an explicit runtime state:

1. env (`read_env` source),
2. IO handles/resources (loop, sockets, TLS contexts),
3. cancellation/timeouts bookkeeping,
4. stdin/stdout/stderr buffers where needed for tests.

For C, this context owns a `uv_loop_t`.

## Key libuv Functions to Add (C Backend)

## Event loop and lifecycle

1. `uv_loop_init`
2. `uv_run`
3. `uv_stop`
4. `uv_loop_close`
5. `uv_close`
6. `uv_is_closing`
7. `uv_ref` / `uv_unref`
8. `uv_now` / `uv_update_time`

## DNS and address handling

1. `uv_getaddrinfo`
2. `uv_freeaddrinfo`
3. `uv_ip4_addr`
4. `uv_ip6_addr`
5. `uv_inet_ntop`

## TCP and stream IO

1. `uv_tcp_init`
2. `uv_tcp_bind`
3. `uv_listen`
4. `uv_accept`
5. `uv_tcp_connect`
6. `uv_read_start`
7. `uv_read_stop`
8. `uv_write`
9. `uv_shutdown`

## Timers, cancellation, coordination

1. `uv_timer_init`
2. `uv_timer_start`
3. `uv_timer_stop`
4. `uv_async_init`
5. `uv_async_send`
6. `uv_queue_work` (for unavoidable blocking work)
7. `uv_cancel`

## Filesystem (future-proofing and parity with existing IO errors)

1. `uv_fs_open`
2. `uv_fs_read`
3. `uv_fs_write`
4. `uv_fs_close`
5. `uv_fs_stat`
6. `uv_fs_mkdir`
7. `uv_fs_scandir`
8. `uv_fs_req_cleanup`

## Error mapping and diagnostics

1. `uv_strerror`
2. `uv_err_name`

## HTTPS Requires More Than libuv

libuv provides async sockets and loop orchestration, not TLS or HTTP.

C runtime stack recommendation:

1. Transport/eventing: libuv
2. TLS: OpenSSL (or mbedTLS)
3. HTTP parsing/serialization: llhttp (or equivalent)

Python and JVM runtime stacks should provide equivalent semantics:

1. Python: `asyncio` + `ssl` + HTTP implementation (stdlib or lightweight dependency).
2. JVM interpreter: Java `HttpClient` for client and JVM HTTPS server implementation for server mode.

Semantic parity is required even if internal libraries differ.

## Bosatsu API Proposal (HTTPS + JSON)

## Module: `Bosatsu/IO/Http`

```bosatsu
package Bosatsu/IO/Http

from Bosatsu/Prog import Prog

enum Method:
  Get
  Post
  Put
  Patch
  Delete
  Head
  Options

struct Header(name: String, value: String)

struct Request(
  method: Method,
  url: String,
  headers: List[Header] = [],
  body_utf8: String = "",
  timeout_ms: Option[Int] = None,
)

struct Response(
  status: Int,
  headers: List[Header],
  body_utf8: String,
)

enum HttpError:
  Io(err: Bosatsu/IO/Error::IOError)
  InvalidUrl(message: String)
  Tls(message: String)
  Protocol(message: String)
  InvalidResponse(message: String)
  Cancelled

external def client_send_impl[env](req: Request) -> Prog[env, HttpError, Response]

external struct ServerHandle

struct ServerConfig(
  host: String,
  port: Int,
  cert_pem_path: String,
  key_pem_path: String,
)

external def server_start_https_impl[env](
  cfg: ServerConfig,
  handler: Request -> Prog[env, HttpError, Response],
) -> Prog[env, HttpError, ServerHandle]

external def server_stop_impl[env](h: ServerHandle) -> Prog[env, HttpError, Unit]
external def server_await_impl[env](h: ServerHandle) -> Prog[env, HttpError, Unit]
```

## Module: `Bosatsu/IO/Json`

```bosatsu
package Bosatsu/IO/Json

from Bosatsu/Prog import Prog

enum Json:
  Null
  Bool(v: Bool)
  Number(v: String)
  String(v: String)
  Array(items: List[Json])
  Object(fields: Dict[String, Json])

enum JsonError:
  ParseError(message: String)
  EncodeError(message: String)

external def parse_impl[env](raw: String) -> Prog[env, JsonError, Json]
external def render_impl[env](json: Json) -> Prog[env, JsonError, String]
```

## Module: `Bosatsu/IO/HttpJson` (helper layer)

Pure codec helpers on top of `Http` + `Json`:

1. set/validate `Content-Type: application/json`,
2. parse response JSON,
3. helper server adapters for JSON request/response handlers.

This keeps network effects in `Prog` and keeps encoding/decoding composition explicit.

## Keeping Python, C, and JVM Interpreter Backends in Sync

## 1. Single-source schema and codegen

Generate from one schema:

1. `Prog` tag constants (if expanded),
2. IO operation ids,
3. error tag ids (`IOError`, `HttpError`, `JsonError`),
4. operation capability flags.

## 2. Shared conformance suite

Add cross-backend tests that run the same Bosatsu programs on:

1. JVM interpreter (`PredefImpl.runProgMain`),
2. transpiled Python runtime,
3. transpiled C runtime executable.

Assertions:

1. same `result` shape,
2. same stdout/stderr,
3. same error variant for failure cases,
4. same timeout/cancellation behavior.

## 3. Capability gating

Backends report capabilities (for example `https_server`, `https_client`).
Public APIs return `Unsupported` errors when unavailable, instead of undefined behavior.

## 4. Versioned ABI policy

Every schema change bumps an IO ABI version.
CI rejects mixed-version runtime/generated artifacts.

## Execution Semantics and Purity Guarantees

For every external IO constructor:

1. construction allocates data only,
2. runtime `Effect` dispatch executes operation,
3. operation returns `Pure(value)` or `Raise(error)`,
4. no observable side effects occur before step 2.

This is the critical rule that preserves `Prog` as Bosatsu's impurity boundary.

## Rollout Plan

## Phase 1: Runtime contract hardening

1. Introduce schema + generated constants.
2. Keep existing `IO/Std` behavior unchanged.
3. Add conformance tests for current `Prog` + stdio operations.

## Phase 2: HTTPS client

1. Add `Bosatsu/IO/Http::client_send_impl`.
2. Implement in C (libuv + TLS + HTTP parser), Python, JVM.
3. Add JSON parse/render module and client JSON helpers.

## Phase 3: HTTPS server

1. Add server start/stop/await APIs.
2. Support request handler as Bosatsu function returning `Prog`.
3. Add contract tests for request/response, headers, status, and error paths.

## Phase 4: hardening and performance

1. timeouts/cancellation tuning,
2. connection reuse/keep-alive policy,
3. memory/FD leak tests,
4. stress tests across all three backends.

## Risks and Mitigations

1. TLS complexity in C runtime.  
Mitigation: isolate TLS adapter behind narrow interface and start with HTTP/1.1.

2. Backend semantic drift.  
Mitigation: schema generation + conformance suite as merge gate.

3. Hidden eager side effects in externals.  
Mitigation: enforce constructor-only pattern in code review and tests.

4. Performance regressions from strict parity layer.  
Mitigation: preserve semantics first, optimize beneath stable operation contract.

## Open Questions

1. TLS library choice for C backend (OpenSSL vs mbedTLS).
2. Whether first server release should include mutual TLS.
3. Whether `Json.Number` should be decimal string only (current proposal) or richer numeric representation.
4. Whether to add structured concurrency primitives to `Prog` in a follow-up design.
