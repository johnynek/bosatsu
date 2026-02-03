# BurritoScript Feature Port Brainstorm

**Date:** 2026-02-02
**Status:** Ready for planning

## What We're Building

Port three major features from BurritoScript to Bosatsu with **full parity or better**:

1. **Debugger (Provenance)** - "Why?" explanations via daemon + CLI
2. **Service** - HTTP server + MCP tools for exposing handlers
3. **TLA+/TLC** - Formal verification via model checking

Goal: Comprehensive one-time port. Don't come back to this.

## Why This Approach

### Sequencing: Debugger → Service → TLA+

**Technical dependencies drive this order:**

1. **Debugger first** because:
   - Already in progress (Phase 8 - protocol done, server skeleton exists)
   - Foundational - provenance traces are reused by Service and TLA+
   - Daemon architecture becomes shared infrastructure

2. **Service second** because:
   - Immediately benefits from debugger (provenance in API responses)
   - Reuses daemon socket architecture
   - High value: MCP tools let AI assistants use Bosatsu

3. **TLA+ third** because:
   - Most specialized use case
   - Can use provenance traces to explain counterexamples
   - Independent of Service, benefits from debugger

### Single Daemon Architecture

One unified server handles:
- Provenance queries (debugger)
- HTTP endpoints (service)
- MCP tool interface (service)

Benefits: Shared state, single process, simpler deployment.

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Sequencing | Debugger → Service → TLA+ | Technical dependencies, reuse |
| Architecture | Single daemon | Shared infrastructure, simpler |
| Debugger scope | Full CLI | explain, deps, usages, path, value, focus, etc. |
| Service scope | Full BurritoService | analyze, validate, build, serve, worker, mcp |
| TLA+ scope | Full + improvements | Everything BurritoScript does, plus better if possible |

## Scope Details

### Debugger (Provenance) - Full CLI

**Commands to implement:**
- `list` - List all traced nodes
- `explain` - Show provenance chain for a value
- `deps` - Show dependencies of a node
- `usages` - Show what uses a node
- `focus`/`unfocus` - Set default node for commands
- `path` - Show path from one node to another
- `value` - Show computed value
- `source` - Show source location
- `snippet` - Show source code snippet
- `eval` - Evaluate expression in trace context
- `status` - Daemon status
- `shutdown` - Stop daemon

**Existing Bosatsu work:**
- `DaemonProtocol.scala` - Protocol types complete
- `DaemonServer.scala` - fs2 Unix socket server skeleton

### Service - Full BurritoService

**Commands to implement:**
- `analyze` - Static analysis (I/O ops, batching opportunities, complexity)
- `validate` - Syntax validation
- `build` - Compile to deployable code (targets: hono, standalone, vercel, aws-lambda)
- `serve` - HTTP server with `POST /api/<handler>` endpoints
- `worker` - Background worker mode
- `mcp` - MCP server exposing handlers as AI tools

**Features:**
- Parallel I/O detection
- OpenAPI documentation generation
- Provenance in responses ("Why?" for computed values)

### TLA+/TLC - Full + Improvements

**Capabilities:**
- `generateTLA` - Convert Bosatsu code to TLA+ specifications
- `runTLC` - Execute TLC model checker
- Specification generation: variables, operators, temporal properties, fairness
- Counterexample parsing and explanation
- **Improvement:** Integrate with provenance to explain failing traces

## Open Questions

1. **Deployment targets for Service** - Keep all four (hono, standalone, vercel, aws-lambda) or focus on subset?

2. **TLA+ improvements** - What specific improvements over BurritoScript? Better counterexample visualization?

3. **Demo integration** - Should the loan/tax/carbon simulation demos get "Why?" buttons from the debugger?

## Existing Plan Reference

The comprehensive plan at `docs/plans/bosatsu-provenance-tooling-plan.md` tracks phases 8-11 covering these features. This brainstorm refines priorities and confirms scope.

## Next Steps

Run `/workflows:plan` to create implementation plans for each feature in sequence:
1. Plan: Complete Debugger (Phase 8)
2. Plan: Service (Phase 11)
3. Plan: TLA+/TLC (Phase 9)
