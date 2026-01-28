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
│  │  - DaemonCommand ADT (14 commands)                  │   │
│  │  - DaemonResponse, ResponseData                     │   │
│  │  - DaemonState                                      │   │
│  └─────────────────────────────────────────────────────┘   │
│                          │                                  │
│                          ▼                                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           DaemonJson.scala                          │   │
│  │  - JSON serialization for protocol types            │   │
│  │  - parseCommand / renderResponse                    │   │
│  │  - Uses existing Json AST                           │   │
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
│  │           DaemonServer.scala                         │   │
│  │  - Unix socket listener (fs2-io)                    │   │
│  │  - JSON command/response protocol                   │   │
│  │  - State persistence via Ref[IO, State]             │   │
│  └─────────────────────────────────────────────────────┘   │
│                          │                                  │
│                          ▼                                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │           DaemonCli.scala                            │   │
│  │  - CLI for daemon commands                          │   │
│  │  - start/stop/status subcommands                    │   │
│  │  - list/explain/deps/etc subcommands               │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Files Created

### Core Source Files

| File | Purpose | Lines |
|------|---------|-------|
| `core/src/main/scala/dev/bosatsu/daemon/DaemonProtocol.scala` | Protocol types (commands, responses, state) | ~306 |
| `core/src/main/scala/dev/bosatsu/daemon/CommandHandler.scala` | Command processing logic | ~340 |
| `core/src/main/scala/dev/bosatsu/daemon/DaemonJson.scala` | JSON serialization | ~310 |

### CLI Source Files

| File | Purpose | Lines |
|------|---------|-------|
| `cli/src/main/scala/dev/bosatsu/daemon/DaemonServer.scala` | Unix socket server | ~130 |
| `cli/src/main/scala/dev/bosatsu/daemon/DaemonCli.scala` | CLI command parsing | ~220 |

### Test Files

| File | Purpose | Tests |
|------|---------|-------|
| `core/src/test/scala/dev/bosatsu/daemon/DaemonGen.scala` | ScalaCheck generators | - |
| `core/src/test/scala/dev/bosatsu/daemon/CommandHandlerTest.scala` | Property tests | 27 |
| `core/src/test/scala/dev/bosatsu/daemon/DaemonJsonTest.scala` | JSON roundtrip tests | 18 |

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

All 45 tests passing (27 CommandHandler + 18 JSON):

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
+ DaemonJson.parseCommand roundtrips for all commands: OK, passed 100 tests.
+ DaemonJson.renderResponse produces parseable JSON: OK, passed 100 tests.
+ DaemonJson.parseCommand parses list command
+ DaemonJson.parseCommand parses explain with nodeId
+ ... (and 13 more JSON tests)
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

### 3. JSON Using Existing Infrastructure

Uses the project's existing `Json` AST with `Reader/Writer` type classes for consistency. No external JSON library needed.

### 4. Unix Socket Protocol

The daemon uses Unix domain sockets for IPC:
- Line-based protocol (one JSON command/response per line)
- Server maintains state via cats-effect `Ref`
- Clean shutdown via `SignallingRef`

## Remaining Work (Future Enhancement)

1. **Trace File Parsing** - Parse actual trace files (currently uses empty trace)
2. **Source File Reading** - Read source files for `Snippet` command
3. **Interactive Mode** - REPL-style interface

## Usage

```bash
# Start daemon with a trace file
bosatsu daemon start trace.json

# Check status
bosatsu daemon status

# Explore the trace
bosatsu daemon list              # List all nodes
bosatsu daemon list --values     # List with values
bosatsu daemon explain           # Explain result node
bosatsu daemon explain n5        # Explain specific node
bosatsu daemon deps n5           # Show dependencies of n5
bosatsu daemon usages n5         # Show what uses n5
bosatsu daemon focus n5          # Set focus to n5
bosatsu daemon unfocus           # Clear focus
bosatsu daemon value n5          # Show full value
bosatsu daemon source n5         # Show source location
bosatsu daemon find "42"         # Find nodes with value "42"

# Stop daemon
bosatsu daemon stop
```

## Verification

```bash
# Compile
nix-shell --run "sbt cli/compile"

# Run tests
nix-shell --run "sbt 'coreJVM/testOnly dev.bosatsu.daemon.*'"

# View help
nix-shell --run "sbt 'cli/runMain dev.bosatsu.Main daemon --help'"
```
