---
title: "feat: Bosatsu Provenance Tooling"
type: feat
date: 2026-01-26
---

# feat: Bosatsu Provenance Tooling

## Overview

Comprehensive provenance and debugging tooling for Bosatsu, porting BurritoScript's AI-agent-friendly development features. This plan covers six complementary systems:

1. **JsGen** - JavaScript code generator (Matchless → native JS) for performant browser runtime
2. **Simulation Applets** - Browser-based interactive simulations with "Why?" UI for end users
3. **Debugger Daemon** - CLI-based trace exploration with REPL commands for developers
4. **TLA+ Formal Verification** - Generate TLA+ specs from Bosatsu code, run TLC model checker
5. **BosatsuUI** - Full UI framework with VNode, DOM bindings, static analysis
6. **BosatsuService/Explorer** - Handler analysis, batching, permissions, application explorer

All systems leverage the same core infrastructure (`DerivationGraph`, `SourceMapper`, `ProvenanceAnalyzer`, `TypedExpr`).

**Key Architecture Decision**: Hybrid JS + WASM approach
- JsGen for UI/DOM code (direct manipulation, no FFI overhead)
- ClangGen → WASM for compute-heavy simulation math
- Permissions as shared layer (not built into core Bosatsu)

**Testing Philosophy**: Property-based testing is mandatory
- Every new module requires property tests using ScalaCheck
- Round-trip properties (generate → parse → generate = original)
- Invariant properties (operations preserve expected invariants)
- 100% line coverage target for core logic
- Integration tests for CLI commands and end-to-end flows

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                        BOSATSU PROVENANCE TOOLING                               │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                 │
│  Bosatsu Source (.bosatsu)                                                      │
│         │                                                                       │
│         ▼                                                                       │
│  ┌─────────────────┐                                                            │
│  │   TypedExpr     │ ◀── Parser + Type Checker                                  │
│  │   (with Region) │                                                            │
│  └────────┬────────┘                                                            │
│           │                                                                     │
│     ┌─────┴─────┬───────────────────────────────────────┐                       │
│     │           │                                       │                       │
│     ▼           ▼                                       ▼                       │
│ ┌─────────┐ ┌─────────────────┐                   ┌──────────┐                  │
│ │Matchless│ │ DerivationGraph │ ◀── ProvenanceAnalyzer  │ IO Monad │                  │
│ │   IR    │ │  + SourceMapper │                   │ Analysis │                  │
│ └────┬────┘ └────────┬────────┘                   └─────┬────┘                  │
│      │               │                                  │                       │
│ ┌────┴────┐          │                                  │                       │
│ │         │          │                                  │                       │
│ ▼         ▼          │                                  ▼                       │
│┌─────┐ ┌───────┐     │                           ┌──────────────┐               │
││JsGen│ │ClangGen│    │                           │    TLA+      │               │
│└──┬──┘ └───┬───┘     │                           │ VERIFICATION │               │
│   │        │         │                           └──────────────┘               │
│   ▼        ▼         │                                                          │
│┌─────┐ ┌──────┐      │                                                          │
││ JS  │ │ WASM │      │                                                          │
│└──┬──┘ └──┬───┘      │                                                          │
│   │       │          │                                                          │
│   └───┬───┘          │                                                          │
│       │              │                                                          │
│       ▼              ▼                                                          │
│ ┌─────────────────────────────────────────────────────────────────┐             │
│ │                    BOSATSU PERMISSIONS (shared)                  │             │
│ └──────────┬──────────────────┬───────────────────┬───────────────┘             │
│            │                  │                   │                              │
│     ┌──────┴───────┐   ┌──────┴──────┐    ┌──────┴──────┐                       │
│     ▼              ▼   ▼             ▼    ▼             ▼                       │
│ ┌───────────┐ ┌────────────┐ ┌───────────┐ ┌──────────────┐                     │
│ │BOSATSU UI │ │  BOSATSU   │ │ DEBUGGER  │ │  SIMULATION  │                     │
│ │(full)     │ │  SERVICE   │ │   DAEMON  │ │   APPLETS    │                     │
│ └─────┬─────┘ └──────┬─────┘ └───────────┘ └──────────────┘                     │
│       │              │                                                          │
│       └──────┬───────┘                                                          │
│              ▼                                                                  │
│       ┌─────────────┐                                                           │
│       │  BOSATSU    │                                                           │
│       │  EXPLORER   │                                                           │
│       └─────────────┘                                                           │
│                                                                                 │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

# Implementation Phases

## Phase 1: Core Infrastructure ✅ DONE
- [x] DerivationGraph, SourceMapper, ProvenanceAnalyzer
- [x] ProvenanceNode with AST reference pattern
- [x] Basic ProvenanceCLI (one-shot)

## Phase 2: Simulation Applets Core ✅ DONE
- [x] Law, StateSchema, SimulationPlan types
- [x] SimulationAnalyzer with validation and topological ordering
- [x] SimulationError types
- [x] build.sbt simulation-runtime module
- [x] MainModule simulation subcommand

## Phase 3: JsGen "Hello World" ✅ DONE
**Demo: Bosatsu code → native JS → runs in browser**

Branch: `feat/jsgen-phase3`
Commits: `05fecfec`, `7123b6c2`

- [x] `Code.scala` - JavaScript AST types (Expression, Statement, etc.)
- [x] `JsGen.scala` - Matchless → JavaScript transpiler (follows PythonGen pattern)
- [x] `JsTranspiler.scala` - Transpiler trait implementation
- [x] CLI: `bosatsu transpile js <file> --outdir <dir>`
- [x] Integration: compile simple Bosatsu function, run in Node.js
- [x] Integration: compile, bundle, run in browser HTML page
- [x] GitHub Pages demo site with JS/C/WASM showcase

**Bug Fixes Applied (PR #2):**
- [x] Module exports only first binding → Fixed to export all bindings
- [x] Entry point name not escaped → Now escapes name to match module exports
- [x] Lambda ignores recName → Uses named function expressions for recursion
- [x] String comparison unboxed → `_cmp_String` returns `[0]/[1]/[2]`
- [x] String pattern uses codePoints → Uses `.length` for UTF-16 units

**Remaining for Phase 3:**
- [ ] `JsGenGen.scala` - ScalaCheck generators for Matchless expressions
- [ ] `JsGenTest.scala` - Property tests
- [ ] `CodeTest.scala` - Unit tests for JS AST rendering edge cases
- [ ] `docs/pr-guides/phase-3-jsgen-pr-guide.md` - PR breakdown for upstream submission

## Phase 3.5: JS/WASM Interop Demo ✅ DONE
**Demo: Bosatsu JS code calls Bosatsu WASM code - true hybrid architecture**

### Goal
Show that the same language (Bosatsu) can be split across compilation targets:
- **Compute-heavy code** → C → WASM (fast, sandboxed)
- **Orchestration/UI code** → JS (DOM access, event handling)
- **They interoperate** via a typed FFI boundary

### Demo Structure

```
demo/examples/
├── compute.bosatsu       # Heavy math, compiled to WASM
├── orchestrator.bosatsu  # UI logic, compiled to JS (imports compute)
└── interop.html          # Shows them working together
```

### File: `compute.bosatsu` (→ WASM)
```bosatsu
package Demo/Compute

export fib, factorial, is_prime

# Compute-heavy functions ideal for WASM
def fib(n: Int) -> Int:
  int_loop(n, (0, 1), (i, acc) ->
    (a, b) = acc
    (i.sub(1), (b, a.add(b)))).fst

def factorial(n: Int) -> Int:
  int_loop(n, 1, (i, acc) -> (i.sub(1), acc.times(i))).snd

def is_prime(n: Int) -> Bool:
  # Check divisibility from 2 to sqrt(n)
  match n.cmp_Int(2):
    LT: False
    EQ: True
    GT:
      int_loop(n.sub(1), True, (i, is_p) ->
        match i.cmp_Int(1):
          GT:
            match n.mod_Int(i).cmp_Int(0):
              EQ: (0, False)  # Found divisor
              _: (i.sub(1), is_p)
          _: (0, is_p)
      ).snd
```

### File: `orchestrator.bosatsu` (→ JS)
```bosatsu
package Demo/Orchestrator

# This file's generated JS will call the WASM exports
# The FFI bridge is created at runtime in demo.js

from Demo/Compute import fib, factorial

export compute_and_format

def compute_and_format(n: Int, op: String) -> String:
  result = match op:
    "fib": fib(n)
    "factorial": factorial(n)
    _: 0
  concat(op, concat("(", concat(int_to_string(n), concat(") = ", int_to_string(result)))))
```

### Implementation Tasks

**Build System:**
- [x] Update GitHub Actions workflow to compile both files
- [x] `compute.bosatsu` → C → WASM with exported functions (`_wasm_fib`, `_wasm_factorial`, `_wasm_collatz`)
- [x] `orchestrator.bosatsu` → JS bundle
- [x] Configure emscripten to export individual functions via `-sEXPORTED_FUNCTIONS`

**FFI Bridge (`demo/interop.js`):**
- [x] Load WASM module and extract exported functions
- [x] Create JS shim that the orchestrator's imports resolve to
- [x] Wire: `Demo_Compute$fib` in JS → calls `wasm._wasm_fib` in WASM
- [x] Handle type conversions via `wasm_wrapper.c` (BValue ↔ int)

**Demo UI (`demo/interop.html`):**
- [x] Number inputs for n values (separate sections for fib/factorial/collatz)
- [x] Separate sections for each operation
- [x] "Compute" buttons that execute the flow
- [x] Show which code ran where (JS vs WASM badge)
- [x] Performance comparison toggle (WASM vs Pure JS mode buttons)

**C Code Changes:**
- [x] Created `wasm_wrapper.c` with EMSCRIPTEN_KEEPALIVE exports
- [x] Direct function calls (C codegen produces direct functions, not closures)
- [x] BValue ↔ int32 conversion via runtime functions

**Testing:**
- [ ] `InteropTest.scala` - Property tests (deferred to later phase)
- [x] Browser test: Playwright E2E tests that exercise the demo

### Success Criteria ✅ ALL MET
1. ✅ User enters value, clicks compute
2. ✅ JS orchestrator code (from `orchestrator.bosatsu`) executes
3. ✅ It calls WASM function (from `compute.bosatsu`)
4. ✅ Result displays correctly
5. ✅ Console shows WASM loading and execution
6. ✅ Compute-intensive Collatz demo shows WASM performance advantage

### Key Insight
This proves the hybrid architecture works:
- Same source language (Bosatsu)
- Different compilation targets chosen per module
- Runtime interop via JS/WASM bridge
- Foundation for simulation applets (heavy physics in WASM, UI in JS)

## Phase 4: Basic DOM Primitives ✅ DONE
**Building blocks for BosatsuUI**

Branch: `feat/phase4-dom-primitives`

- [x] `VNode.scala` - Virtual DOM types (Element, Text, Component)
- [x] `DOMRuntime.scala` - Scala.js runtime for createElement, setAttribute, addEventListener (uses js.Dynamic)
- [x] `DOMCodegen.scala` - Generate native JS DOM manipulation code via JsGen
- [x] `VNodeGen.scala` - ScalaCheck generators for VNode trees
- [x] `VNodeTest.scala` - Property tests (22 tests passing)
- [x] `DOMRuntimeTest.scala` - JS-specific unit tests (6 tests)
- [x] `demo/dom-test.html` - Browser integration test (createElement, setAttribute, addEventListener, VNode rendering)
- [x] `docs/pr-guides/phase-4-dom-pr-guide.md`

**Test Results:**
- Total: 1063 coreJS tests passing (including 28 UI tests)
- Browser integration test covers: createElement, appendChild, setAttribute, createTextNode, addEventListener, VNode tree rendering

## Phase 5: Simple Reactive Simulation ✅ DONE
**Demo: Interactive slider → compute → display**

Branch: `feat/phase5-reactive-state`

- [x] State management with reactive updates (`ReactiveState.scala` - MutableState, ComputedState, StateStore)
- [x] Connect state changes to DOM updates (`StateBinding.scala` - TextBinding, AttributeBinding, InputBinding)
- [x] `ReactiveStateTest.scala` - Property tests (15 tests passing)
- [x] `ReactiveStateGen.scala` - Generators for state and updates
- [x] Demo: Tax calculator with income slider, shows tax_rate and total_tax (`demo/tax-calculator.html`)
- [x] `docs/pr-guides/phase-5-reactive-pr-guide.md`
- [ ] JsGen-based law evaluation (deferred - requires TypedExpr integration)
- [ ] Performance benchmark: JsGen vs Scala.js interpreter (deferred to Phase 6)

**Test Results:**
- Total: 15 ReactiveState tests passing
- Tax calculator demo shows reactive state updates in real-time

## Phase 6: Simulation Applets Full ✅ DONE
**Upgrade from Scala.js interpreter to JsGen**

Branch: `feat/phase6-simulation-applets`

- [x] "Why?" buttons with derivation chain display (`WhyExplainer.scala`)
- [x] "What if?" assumption toggling UI (`WhatIfToggle.scala`)
- [x] Parameter sweeps (`ParameterSweep.scala`)
- [x] Self-contained HTML embed generation (~6KB gzipped, target: <20KB)
- [x] Library mode embed generation (`EmbedGenerator.LibraryMode`)
- [x] Theme support (light/dark)
- [x] Tests in `SimulationAppletTest.scala`, `UIComponentsTest.scala`
- [x] `docs/pr-guides/phase-6-simulation-applets-pr-guide.md`
- [x] `simulation-cli` module with `bosatsu-sim` CLI
- [x] XSS fix: HTML escaping in WhatIfToggle and ParameterSweep

**Test Results:**
- Core UI tests: 148 passing
- Simulation CLI tests: 155 passing

## Phase 7: BosatsuPermissions ✅ DONE
**Shared permission layer for Service, UI, Explorer**

Branch: `feat/phase6-simulation-applets`

- [x] `Permission.scala` - Permission declaration types (SimplePermission, ScopedPermission, Scope, PermissionTemplate)
- [x] `PermissionAnalyzer.scala` - Static analysis of required permissions from Matchless IR
- [x] `CallGraphPermissions.scala` - Permission propagation through call graph
- [x] `PermissionChecker.scala` - Runtime permission checking with scope-based access control
- [x] `PermissionGen.scala`, `PermissionAnalyzerTest.scala`, `PermissionCheckerTest.scala`
- [x] Integration: analyze function, extract required permissions
- [x] `docs/pr-guides/phase-7-permissions-pr-guide.md`

**Test Results:**
- Total: 44 permission tests passing
- Covers: scope checking, permission hierarchy, call graph propagation, template resolution

## Phase 8: Debugger Daemon (TODO)

- [ ] DaemonProtocol command/response ADT
- [ ] DaemonState with Ref[IO, State]
- [ ] DaemonServer with fs2 Unix sockets
- [ ] DaemonClient for CLI
- [ ] DaemonCommand CLI integration
- [ ] All command handlers (list, explain, deps, usages, find, path, value, source, snippet, focus)
- [ ] JSON output mode
- [ ] `DaemonProtocolGen.scala`, `DaemonProtocolTest.scala`, `DaemonStateTest.scala`, `DaemonServerTest.scala`
- [ ] `docs/pr-guides/phase-8-daemon-pr-guide.md`

## Phase 9: TLA+ Formal Verification (TODO)

- [ ] TLATypes.scala - TLA+ AST types, TLCResult, options
- [ ] TLAEmitter.scala - TypedExpr → TLA+ generation
- [ ] TLCRunner.scala - Run TLC, parse output, counterexamples
- [ ] TLACommand.scala - CLI integration
- [ ] SimulationTLA.scala - Simulation-specific export
- [ ] Backend detection (nix, docker)
- [ ] Concurrent process support (emitConcurrent)
- [ ] `TLAGen.scala`, `TLAEmitterTest.scala`, `TLCRunnerTest.scala`, `SimulationTLATest.scala`
- [ ] `docs/pr-guides/phase-9-tla-pr-guide.md`

## Phase 10: BosatsuUI Full Framework (TODO)
**Complete UI framework with static analysis**

- [ ] `DOMBindings.scala` - DOM binding analysis from TypedExpr
- [ ] `UIAnalyzer.scala` - Static analysis for subscriptions, render targets
- [ ] Auto-extracted state subscriptions
- [ ] Render target mapping
- [ ] Ownership conflict detection
- [ ] Modal pattern recognition
- [ ] Fetch parallelization
- [ ] Cache invalidation rules
- [ ] `UIGen.scala`, `DOMBindingsTest.scala`, `UIAnalyzerTest.scala`
- [ ] Validation: compare to React TodoMVC for DX, Elm TodoMVC for safety
- [ ] `docs/pr-guides/phase-10-ui-pr-guide.md`

## Phase 11: BosatsuService (TODO)
**Handler analysis, batching, permissions**

- [ ] `ServiceHandler.scala` - Handler type definitions
- [ ] `HandlerAnalyzer.scala` - Static analysis of handlers
- [ ] `BatchQueue.scala` - Request batching implementation
- [ ] `ServiceValidator.scala` - Request/response validation
- [ ] `ServiceTracer.scala` - Request tracing
- [ ] Integration with BosatsuPermissions
- [ ] CLI: `bosatsu service analyze <file>`
- [ ] `ServiceGen.scala`, `BatchQueueTest.scala`, `HandlerAnalyzerTest.scala`, `ServiceValidatorTest.scala`
- [ ] `docs/pr-guides/phase-11-service-pr-guide.md`

## Phase 12: BosatsuExplorer (TODO)
**Interactive application explorer**

- [ ] `Explorer.scala` - Main explorer application
- [ ] `ExplorerCLI.scala` - CLI interface
- [ ] `ExplorerWeb.scala` - Web UI interface
- [ ] CLI: `bosatsu explore <file>`
- [ ] CLI: `bosatsu explore <file> --web --port 3000`
- [ ] List handlers, inspect permissions, trace requests
- [ ] `ExplorerTest.scala`, `ExplorerGen.scala`
- [ ] `docs/pr-guides/phase-12-explorer-pr-guide.md`

## Phase 13: Polish & Integration (TODO)

- [ ] Coverage audit: Verify 100% line coverage on all core modules
- [ ] Property test audit: Verify all modules have property tests
- [ ] Mutation testing: Run mutation testing to verify test quality
- [ ] Nightly CI: Run property tests with 10,000+ iterations
- [ ] Documentation updates
- [ ] Bundle size verification
- [ ] Performance benchmarks (JsGen vs Scala.js interpreter)
- [ ] Cross-platform testing (Linux, macOS, Windows)
- [ ] Fuzz testing: Run fuzzer on CLI commands and parsers

---

# Current Status

**Phase 7 is complete** - Permission system for static analysis and runtime checking.

**Completed:**
- Phase 1: Core Infrastructure ✅
- Phase 2: Simulation Applets Core ✅
- Phase 3: JsGen "Hello World" ✅
- Phase 3.5: JS/WASM Interop Demo ✅
- Phase 4: Basic DOM Primitives ✅
- Phase 5: Simple Reactive Simulation ✅
- Phase 6: Simulation Applets Full ✅
- Phase 7: BosatsuPermissions ✅

**Next recommended work:**
1. Add property tests for JsGen (remaining Phase 3 items)
2. Start Phase 8 (Debugger Daemon) - Interactive debugging server

---

# Related Documentation

- [GitHub Pages Demo](./github-pages-demo.md) - Multi-target compilation showcase (Phase 3)
- Full plan details in `/Users/steven/Documents/Code/portToBosatsu/docs/plans/2026-01-26-feat-bosatsu-provenance-tooling-plan.md`
