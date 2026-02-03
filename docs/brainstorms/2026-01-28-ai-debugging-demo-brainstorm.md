---
title: AI Debugging Demo with Browser Interface
date: 2026-01-28
status: decided
chosen_approach: Complete DaemonServer + ScalaJS browser demo
---

# AI Debugging Demo with Browser Interface

## What We're Building

An interactive browser-based debugging experience for Bosatsu programs that showcases static analysis capabilities. The demo will:

1. Allow users to explore provenance chains ("Why does this value equal X?")
2. Support "What if?" scenarios by toggling assumptions
3. Run entirely in the browser via ScalaJS
4. Demonstrate bosatsu's static analysis superpower for AI debugging workflows

## Why This Approach

### Research Findings

**Existing infrastructure (Phases 1-7 complete):**
- `DerivationAnalyzer` extracts dependencies from Matchless IR
- `DaemonProtocol` has 14 commands already implemented
- `ProvenanceNode`/`ProvenanceTrace` types for tracking
- `WhyExplainer` generates "Why?" UI explanations
- `CommandHandler` processes all daemon commands

**What's missing for a working demo:**
- HTTP/WebSocket API layer (currently Unix socket only)
- Browser UI to visualize provenance
- ScalaJS cross-compilation of core + daemon modules

**Why this over alternatives:**
- BurritoUI port (Approach 2): Larger scope, requires porting entire rendering system
- Existing plan (Approach 3): Phases 9-14 cover more but slower to tangible result
- AI Debugging Demo: Builds on 7 completed phases, shortest path to demonstrable value

### YAGNI Applied

The AI debugging demo focuses on one compelling use case rather than trying to port everything at once. We can always add BurritoUI and BurritoService later once the debugging experience proves valuable.

## Key Decisions

1. **Target: Browser-based demo** - Not just CLI, but interactive web UI
2. **ScalaJS for browser** - Cross-compile existing Scala code rather than rewrite in JS
3. **HTTP/WebSocket API** - Add web-friendly transport alongside Unix socket
4. **Focus on provenance visualization** - "Why?" explanations are the killer feature
5. **Single-page demo** - One `.bosatsu` file that showcases all capabilities

## Technical Approach

### Architecture

```
Browser UI (ScalaJS)
     │
     ▼
HTTP/WebSocket API
     │
     ▼
CommandHandler (existing)
     │
     ▼
DaemonProtocol commands (14 existing)
     │
     ▼
ProvenanceTrace / DerivationAnalyzer
```

### Modules Affected

1. **core (cross-compile to ScalaJS)**
   - DaemonProtocol, ProvenanceTrace, DerivationAnalyzer
   - Already platform-agnostic Scala

2. **New: daemon-http (JVM)**
   - HTTP/WebSocket server using http4s
   - Wraps existing CommandHandler

3. **New: browser-ui (ScalaJS)**
   - Minimal UI for provenance exploration
   - Calls daemon via fetch/WebSocket

### Demo Flow

1. User loads `demo.bosatsu` in browser
2. UI shows computed results with "Why?" buttons
3. Clicking "Why?" reveals derivation chain
4. "What if?" toggles let user change assumptions
5. Results recompute and re-explain automatically

## Open Questions

1. **HTTP framework choice** - http4s? Armeria? Keep simple?
2. **UI framework** - Raw DOM? Laminar? Keep minimal?
3. **Demo content** - Tax calculator? Physics simulation? What showcases provenance best?

## Success Criteria

- [ ] Can run demo entirely in browser (no server required for basic mode)
- [ ] "Why?" buttons show human-readable derivation chains
- [ ] "What if?" toggles recompute values
- [ ] Demo loads in < 3 seconds
- [ ] Works with AI coding assistants (Claude, Copilot) for debugging

## Next Steps

1. Run `/workflows:plan` to create detailed implementation plan
2. Focus on Phase 8 completion (DaemonServer integration)
3. Add HTTP layer
4. Create ScalaJS browser module
5. Build minimal demo UI

## References

- Existing plan: `docs/plans/2026-01-26-feat-bosatsu-provenance-tooling-plan.md`
- DaemonProtocol: `core/src/main/scala/dev/bosatsu/daemon/DaemonProtocol.scala`
- CommandHandler: `cli/src/main/scala/dev/bosatsu/daemon/CommandHandler.scala`
- WhyExplainer: `core/src/main/scala/dev/bosatsu/ui/WhyExplainer.scala`
