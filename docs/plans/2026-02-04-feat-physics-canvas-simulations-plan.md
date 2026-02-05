---
title: Physics Canvas Simulations
type: feat
date: 2026-02-04
---

# Physics Canvas Simulations (Built on BosatsuUI)

## Overview

Extend BosatsuUI to support canvas-based physics simulations. Canvas rendering is just another binding type - we reuse the existing UIAnalyzer, VNode, and state infrastructure.

**Key constraints:**
- No hand-written JS - all code generated from Bosatsu source
- Physics laws in `.bosatsu` files, not platform
- Build ON TOP of BosatsuUI, not beside it

## Technical Approach

### Canvas as a VNode Binding

Canvas is just an HTML element with a special render prop:

```bosatsu
from Bosatsu/UI import h, state, read
from Bosatsu/Canvas import circle, fill, sequence, canvas_render

ball = state(BallState(200.0, 50.0, 100.0, 0.0))

def render_ball(s: BallState) -> CanvasCommands:
  sequence([fill("#ff6b8a"), circle(s.x, s.y, 20.0)])

main = h("canvas", [
  ("width", "400"),
  ("height", "300"),
  canvas_render(ball, render_ball)  # special prop like on_click
], [])
```

### What We Reuse from BosatsuUI

| Component | How It's Reused |
|-----------|-----------------|
| VNode | Canvas is `h("canvas", props, [])` |
| UIAnalyzer | Already walks TypedExpr, add canvas_render detection |
| State bindings | Canvas render depends on state, same as textContent |
| Event handlers | on_click for reset button, sliders for params |
| Runtime | Add canvas executor alongside DOM updater |

### What We Add

| New Component | Purpose |
|---------------|---------|
| `Bosatsu/Canvas` library | Commands: circle, rect, fill, etc. |
| `canvas_render` prop | Bind state to canvas render function |
| `on_frame` hook | Animation loop for physics updates |
| Canvas executor | Interprets CanvasCommands in runtime |

### Static Optimization (Automatic)

UIAnalyzer already extracts state dependencies. For canvas:

```bosatsu
def render_ball(s: BallState) -> CanvasCommands:
  sequence([
    fill("#1a1a2e"),           # static - no state deps
    circle(s.x, s.y, 20.0)     # depends on s.x, s.y
  ])
```

UIAnalyzer detects: `ball.x` and `ball.y` affect canvas, but `ball.gravity` doesn't.
Runtime only re-renders when those fields change.

## Implementation Phases

### Phase 1: Bosatsu/Canvas Library
- [ ] Create `core/src/main/resources/bosatsu/canvas.bosatsu`
  ```bosatsu
  package Bosatsu/Canvas

  export (CanvasCommands, circle, rect, line, text, fill, stroke,
          sequence, clear, canvas_render, on_frame)

  external struct CanvasCommands
  external def circle(x: Float, y: Float, radius: Float) -> CanvasCommands
  external def rect(x: Float, y: Float, w: Float, h: Float) -> CanvasCommands
  external def fill(color: String) -> CanvasCommands
  external def sequence(cmds: List[CanvasCommands]) -> CanvasCommands
  external def canvas_render[a](state: State[a], render: a -> CanvasCommands) -> (String, String)
  external def on_frame(update: Float -> Unit) -> Unit
  ```

### Phase 2: CanvasExternal Intrinsics
- [ ] Add to `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala`
  - CanvasExternal object with intrinsics
  - Commands compile to JS objects: `{type: "circle", x, y, r}`
  - `canvas_render` compiles to special prop marker
  - `on_frame` compiles to requestAnimationFrame registration

### Phase 3: UIAnalyzer Extension
- [ ] Extend `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
  - Detect `canvas_render` in VNode props
  - Extract render function and its state dependencies
  - Add CanvasBinding to analysis output
  - Reuse existing dependency tracking

### Phase 4: Runtime Extension
- [ ] Extend canvas executor in UIGen runtime
  ```javascript
  function _executeCanvas(cmds, ctx) {
    cmds.forEach(cmd => {
      switch(cmd.type) {
        case 'fill': ctx.fillStyle = cmd.color; break;
        case 'circle':
          ctx.beginPath();
          ctx.arc(cmd.x, cmd.y, cmd.r, 0, Math.PI*2);
          ctx.fill();
          break;
        // ...
      }
    });
  }
  ```
- [ ] Hook canvas re-render to state changes (like DOM updates)
- [ ] Add animation loop for on_frame callbacks

### Phase 5: Demo - Bouncing Ball
- [ ] Create `demos/physics/bouncing-ball.bosatsu`
  ```bosatsu
  package BouncingBall

  from Bosatsu/UI import h, state, read, write, on_click
  from Bosatsu/Canvas import circle, fill, clear, sequence, canvas_render, on_frame

  struct BallState(x: Float, y: Float, vx: Float, vy: Float, gravity: Float)

  ball = state(BallState(200.0, 50.0, 100.0, 0.0, 500.0))

  # Pure physics - laws in Bosatsu!
  def update_physics(s: BallState, dt: Float) -> BallState:
    vy1 = add(s.vy, times(s.gravity, dt))
    y1 = add(s.y, times(vy1, dt))
    # ... collision detection
    BallState(s.x, y1, s.vx, vy1, s.gravity)

  def render_ball(s: BallState) -> CanvasCommands:
    sequence([clear("#1a1a2e"), fill("#ff6b8a"), circle(s.x, s.y, 20.0)])

  frame_handler = on_frame(\dt -> write(ball, update_physics(read(ball), dt)))

  main = h("div", [], [
    h("canvas", [("width", "400"), ("height", "300"), canvas_render(ball, render_ball)], []),
    h("button", [on_click(\_ -> write(ball, BallState(200.0, 50.0, 100.0, 0.0, 500.0)))],
      [text("Reset")])
  ])
  ```
- [ ] Generate HTML with `bosatsu-sim ui`
- [ ] Verify no hand-written JS in output
- [ ] Verify physics runs at 60fps

### Phase 6: Demo - Pendulum
- [ ] Create `demos/physics/pendulum.bosatsu`
  - Angular motion physics
  - Trail visualization (list of past positions)
  - Energy display
- [ ] Add gradient commands if needed

## Design Decisions (from SpecFlow Analysis)

### Animation Loop
- **Single shared loop**: All `on_frame` callbacks are called from one requestAnimationFrame loop
- **dt capping**: Cap delta time at 33ms (1/30s) to prevent physics explosions after tab switch
- **State batching**: All state changes within a frame callback are batched; canvas re-renders once at end

### Error Handling
- **Render function throws**: Wrap in try/catch, log to console, continue animation
- **Invalid state (NaN/Infinity)**: Physics functions should guard against this; no runtime validation
- **Canvas element missing**: Guard with `if (!ctx) return` - fail silently (existing pattern)

### Type System
- **Coordinates**: Use `Double` for consistency with `Bosatsu/Numeric`
- **CanvasCommands**: External opaque struct (like VNode)

### Tab Visibility
- **Hidden tab**: requestAnimationFrame pauses naturally
- **Tab returns**: dt is capped, so large time gaps are ignored (simulation "pauses" with user)

### Multiple Canvases
- Each `canvas_render` binding is tracked separately
- All share the same animation loop
- Each canvas has its own state and render function

## Acceptance Criteria

- [ ] Bouncing ball demo works (smooth 60fps animation)
- [ ] All physics code is in .bosatsu files (grep confirms)
- [ ] No hand-written JS in generated HTML
- [ ] UIAnalyzer correctly detects canvas state dependencies
- [ ] Only affected canvas regions re-render on state change
- [ ] All existing BosatsuUI tests still pass
- [ ] Tab switch doesn't cause physics explosion (dt capping works)
- [ ] Render function errors are caught and logged (animation continues)

## Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `core/src/main/resources/bosatsu/canvas.bosatsu` | Create | Canvas command library |
| `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` | Modify | Add CanvasExternal intrinsics |
| `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` | Modify | Add canvas_render detection |
| `core/src/main/scala/dev/bosatsu/ui/UIGen.scala` | Modify | Add canvas executor to runtime |
| `simulation-cli/.../UICommand.scala` | Modify | Wire canvas support |
| `demos/physics/bouncing-ball.bosatsu` | Create | Demo |
| `demos/physics/pendulum.bosatsu` | Create | Demo |

## Implementation Details (from Research)

### UIAnalyzer Extension Pattern

The existing UIAnalyzer extracts bindings via `extractUIConstruction`. To add canvas_render:

```scala
// In extractUIConstruction, detect canvas_render calls
case "canvas_render" =>
  val statePath = traceStateDependency(args(0), ctx)
  val renderFn = args(1)
  statePath.foreach { path =>
    ctx.recordBinding(CanvasBinding(
      elementId = currentElementId,
      statePath = path,
      renderFn = renderFn
    ))
  }
```

### JsGen Intrinsic Pattern

Follow the existing UIExternal pattern in JsGen.scala:

```scala
// In CanvasExternal.results
Identifier.Name("circle") -> ((args: List[Code.Expression]) =>
  Code.ObjectLiteral(List(
    "type" -> Code.StringLiteral("circle"),
    "x" -> args(0),
    "y" -> args(1),
    "r" -> args(2)
  )), 3)

Identifier.Name("canvas_render") -> ((args: List[Code.Expression]) =>
  Code.Call(Code.Ident("_ui_canvas_render"), List(args(0), args(1))), 2)
```

### DOM Selector Safety (from learnings)

Define canvas selectors as constants to prevent drift:
```javascript
const CANVAS_ID_PREFIX = '_canvas_';
// Use consistent naming: _canvas_0, _canvas_1, etc.
```

## References

- Brainstorm: `docs/brainstorms/2026-02-04-physics-canvas-simulations-brainstorm.md`
- BosatsuUI library: `core/src/main/resources/bosatsu/ui.bosatsu`
- UIAnalyzer: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- JsGen externals pattern: `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` (UIExternal object, lines 593-768)
- Burritoscript examples: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/simulation-applets/framework/`
- Learnings: `docs/solutions/runtime-errors/canvas-visualization-container-selector-fixes.md`
