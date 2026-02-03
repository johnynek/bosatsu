# BosatsuUI: Making Demos Use Real Compiled Bosatsu

**Date:** 2026-02-03
**Status:** Brainstorm complete
**Next:** `/workflows:plan` when ready to implement

## What We're Building

A roadmap to ensure all BosatsuUI demos compile from `.bosatsu` source files rather than being hand-written JavaScript. The goal is full parity where:

1. Every demo has a `.bosatsu` source file
2. Demos are generated via `bosatsu-sim ui input.bosatsu -o output.html`
3. Benchmarks test real compiled Bosatsu vs React (not hand-written JS vs React)

## Current State

**Demos with .bosatsu source (3):**
- `counter.bosatsu` → compiles to `counter.html`
- `todo-list.bosatsu` → compiles to `todo-list.html`
- `conditional.bosatsu` → compiles to `conditional.html`

**Demos that are hand-written JS (8):**
- `dashboard.html` - needs nested state
- `spreadsheet.html` - needs 2D data structures, cell updates
- `shopping-cart.html` - needs dynamic list operations
- `kanban-board.html` - needs drag events, list reordering
- `nested-comments.html` - needs recursive templates
- `drag-drop.html` - needs drag events
- `shopping-cart-dynamic.html` - needs dynamic mutations
- `shopping-cart-maps.html` - needs Map data structures

**Benchmarks:**
- `drag-animation/index.html` - uses BosatsuUI runtime but not compiled Bosatsu
- `ui-performance/index.html` - same issue

## Why This Approach

**Incremental strategy chosen because:**

1. **Quick wins build momentum** - Fixing easy things (style bindings, events) delivers visible progress
2. **Hard problems need design** - Dynamic lists require Bosatsu language changes
3. **Benchmarks are high value** - A real compiled benchmark proves the concept
4. **Existing demos work** - 3 demos already compile, just need to ensure they're actually used

## Key Decisions

### Phase 1: Ensure existing demos are truly generated
- [x] Verify `counter.html` is generated from `counter.bosatsu` (done - uses UICommand)
- [x] Verify `todo-list.html` is generated
- [ ] Update `scripts/regenerate_demos.sh` to include UI demos
- [ ] Remove `*_generated.html` duplicates, use generated files as primary

### Phase 2: Add style.transform bindings for benchmarks
- [ ] Create `particles.bosatsu` with particle state using new Math functions
- [ ] Extend UIAnalyzer to extract `style.transform` bindings from expressions
- [ ] Generate benchmark from real Bosatsu source

### Phase 3: Add more event types
- [ ] Add `on_drag`, `on_dragover`, `on_drop` to Bosatsu/UI
- [ ] Add `on_keydown`, `on_keyup` for keyboard input
- [ ] Add `on_focus`, `on_blur` for form handling
- [ ] Implement in UIExternal in JsGen.scala

### Phase 4: Tackle dynamic lists (requires deeper work)
- [ ] Design mutable list API for Bosatsu (`append`, `remove`, `update_at`)
- [ ] Implement list state tracking in UIAnalyzer
- [ ] Handle list item keys for efficient updates
- [ ] Port shopping-cart to Bosatsu

### Phase 5: Nested state and recursive templates (future)
- [ ] Design nested state path syntax (`user.profile.name`)
- [ ] Implement template recursion for nested comments
- [ ] Consider component abstraction

## Open Questions

1. **How should dynamic list bindings work?**
   - Per-item bindings (current approach) require knowing items at compile time
   - Need a way to generate bindings for dynamically added items

2. **Should Bosatsu support mutable data structures?**
   - Current: Purely functional, immutable
   - Option A: Add mutable Arrays/Maps as external types
   - Option B: Use persistent data structures with copy-on-write

3. **How to handle event data (clientX, key, etc.)?**
   - Option A: Pass structured record to handler
   - Option B: Make event data available via global accessor
   - Option C: Define Event type in Bosatsu/UI

4. **Template recursion for nested structures?**
   - Trees (comments) need recursive rendering
   - Not possible with current h() function approach
   - May need component/template abstraction

## Missing Features Summary

| Feature | Complexity | Blocks |
|---------|-----------|--------|
| Style bindings (transform) | Low | Benchmarks |
| More event types | Low | Drag-drop, Kanban |
| Event data access | Medium | All complex demos |
| Dynamic list ops | High | Shopping cart, Todo CRUD |
| Nested state | Medium | Dashboard |
| Recursive templates | High | Nested comments |
| 2D data structures | High | Spreadsheet |

## Success Criteria

1. All demos in `demos/ui/` have corresponding `.bosatsu` source files
2. `scripts/regenerate_demos.sh` regenerates all demos from source
3. Benchmark tests real compiled Bosatsu code, not hand-written JS
4. No `*_generated.html` duplicates - only one source of truth
