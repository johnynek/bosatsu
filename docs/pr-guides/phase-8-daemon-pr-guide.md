# Phase 8: Debugger Daemon - PR Guide

## Overview

Phase 8 implements a provenance debugger daemon for Bosatsu, ported from BurritoScript's provenance-daemon. This daemon holds a provenance trace in memory and handles exploration commands, enabling stateful debugging across multiple CLI invocations.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Debugger Daemon                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           DaemonProtocol.scala                       │   │
│  │  - NodeId, SourceLocation, SourceType               │   │
│  │  - TraceNode, ProvenanceTrace                       │   │
│  │  - DaemonCommand ADT (16 commands)                  │   │
│  │  - DaemonResponse, ResponseData                     │   │
│  │  - DaemonState                                      │   │
│  └─────────────────────────────────────────────────────┘   │
│                          │                                  │
│                          ▼                                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           CommandHandler.scala                       │   │
│  │  - handle(command, state) → CommandResult           │   │
│  │  - Pure logic, no I/O                               │   │
│  │  - Tested via property tests                        │   │
│  └─────────────────────────────────────────────────────┘   │
│                          │                                  │
│                          ▼                                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           DaemonServer.scala (TODO)                  │   │
│  │  - Unix socket listener                             │   │
│  │  - JSON command/response protocol                   │   │
│  │  - State persistence via Ref[IO, State]             │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Files Created

### Main Source Files

| File | Purpose | Lines |
|------|---------|-------|
| `core/src/main/scala/dev/bosatsu/daemon/DaemonProtocol.scala` | Protocol types (commands, responses, state) | ~250 |
| `core/src/main/scala/dev/bosatsu/daemon/CommandHandler.scala` | Command processing logic | ~220 |

### Test Files

| File | Purpose | Tests |
|------|---------|-------|
| `core/src/test/scala/dev/bosatsu/daemon/DaemonGen.scala` | ScalaCheck generators | - |
| `core/src/test/scala/dev/bosatsu/daemon/CommandHandlerTest.scala` | Property tests | 27 |

## Commands Implemented

| Command | Description |
|---------|-------------|
| `List(showValues)` | List all nodes in the trace |
| `Explain(nodeId?)` | Show derivation tree for a node |
| `Find(value)` | Find nodes with matching value |
| `Deps(nodeId)` | Show dependencies of a node |
| `Usages(nodeId)` | Show what uses a node |
| `Focus(nodeId)` | Set focus to a node |
| `Unfocus` | Clear focus |
| `Path(nodeId)` | Show path from root to node |
| `Value(nodeId)` | Show full value of a node |
| `Source(nodeId)` | Show source location |
| `Snippet(nodeId, context)` | Show code snippet with scope |
| `Eval(nodeId, expr)` | Evaluate expression with node's scope |
| `Status` | Get daemon status |
| `Shutdown` | Stop the daemon |

## Test Coverage

All 27 property tests passing:

```
+ CommandHandler.list returns all nodes: OK, passed 100 tests.
+ CommandHandler.list preserves state: OK, passed 100 tests.
+ CommandHandler.list with showValues includes values: OK, passed 100 tests.
+ CommandHandler.list without showValues excludes values: OK, passed 100 tests.
+ CommandHandler.explain with valid node succeeds: OK, passed 100 tests.
+ CommandHandler.explain without nodeId uses effective node: OK, passed 100 tests.
+ CommandHandler.explain with focus uses focused node: OK, passed 100 tests.
+ CommandHandler.explain with invalid node fails: OK, passed 100 tests.
+ CommandHandler.find returns nodes with matching value: OK, passed 100 tests.
+ CommandHandler.find returns empty for non-matching value: OK, passed 100 tests.
+ CommandHandler.deps with valid node succeeds: OK, passed 100 tests.
+ CommandHandler.deps returns correct dependencies: OK, passed 100 tests.
+ CommandHandler.deps with invalid node fails: OK, passed 100 tests.
+ CommandHandler.usages with valid node succeeds: OK, passed 100 tests.
+ CommandHandler.usages returns correct usages: OK, passed 100 tests.
+ CommandHandler.focus updates state: OK, passed 100 tests.
+ CommandHandler.unfocus clears focus: OK, passed 100 tests.
+ CommandHandler.focus with invalid node fails: OK, passed 100 tests.
+ CommandHandler.path with valid node succeeds: OK, passed 100 tests.
+ CommandHandler.path includes target node: OK, passed 100 tests.
+ CommandHandler.value with valid node returns value: OK, passed 100 tests.
+ CommandHandler.source with valid node succeeds: OK, passed 100 tests.
+ CommandHandler.source returns correct location: OK, passed 100 tests.
+ CommandHandler.status returns correct info: OK, passed 100 tests.
+ CommandHandler.shutdown sets shouldShutdown flag: OK, passed 100 tests.
+ CommandHandler.non-shutdown commands don't set shutdown flag: OK, passed 100 tests.
+ CommandHandler.read-only commands preserve state: OK, passed 100 tests.
```

## Design Decisions

### 1. Separation of Protocol and Handler

The protocol types and command handling logic are kept separate:
- `DaemonProtocol.scala` - Pure data types with no logic
- `CommandHandler.scala` - Pure functions, no I/O

This enables easy testing via property tests without mocking I/O.

### 2. Focus Node Support

Commands like `Explain` can operate on:
1. An explicit node ID (if provided)
2. The focused node (if set via `Focus` command)
3. The result node (default fallback)

This matches BurritoScript's behavior for interactive exploration.

### 3. Value Representation

Values are stored as JSON strings for flexibility. The `formatValueBrief` function provides concise display for listings.

## Remaining Work (Phase 8 continuation)

1. **DaemonServer** - Unix socket server using fs2
2. **DaemonClient** - Client for CLI to communicate with daemon
3. **DaemonCommand CLI** - Integration with Bosatsu CLI
4. **JSON serialization** - Proper JSON encoding/decoding
5. **Source file reading** - For `Snippet` command

## Usage Example (planned)

```bash
# Start daemon with a trace file
bosatsu daemon start trace.json

# Explore the trace
bosatsu list              # List all nodes
bosatsu explain           # Explain result node
bosatsu deps n5           # Show dependencies of n5
bosatsu snippet n5        # Show code with scope

# Interactive mode
bosatsu debug file.bosatsu
debug> explain
debug> deps n3
debug> quit
```

## Verification

```bash
# Compile
nix-shell --run "sbt coreJVM/compile"

# Run tests
nix-shell --run "sbt 'coreJVM/testOnly dev.bosatsu.daemon.*'"
```
