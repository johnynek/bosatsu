# Physics Canvas Simulations in Bosatsu

**Date:** 2026-02-04
**Status:** Brainstorm

## What We're Building

Physics simulations (bouncing ball, pendulum, car navigation) rendered on canvas that are:
1. **As pretty as burritoscript demos** - smooth animations, gradients, trails, shadows
2. **Physics laws in .bosatsu files** - NOT hardcoded in the platform
3. **Leverages existing BosatsuUI** - reactive state, event handlers, DOM bindings
4. **No hand-written JS** - all code generated from Bosatsu source

## Analysis of Burritoscript Pattern

Looking at `bouncing-ball.burrito.ts` and `pendulum.burrito.ts`:

```typescript
// State is a simple record
interface BallState {
  x: number; y: number; vx: number; vy: number;  // physics
  gravity: number; bounciness: number;            // user params
  running: boolean;                                // control
  width: number; height: number; ballRadius: number; // canvas
}

// Physics is a PURE function - no side effects
const updatePhysics = (state: BallState, dt: number): Partial<BallState> => {
  // Apply gravity, update position, detect collisions
  // Returns only the changed fields
}

// Rendering is a PURE function - takes ctx and state, draws
function renderBall(ctx: CanvasRenderingContext2D, state: BallState): void {
  // Draw background, grid, shadow, ball with gradient
}
```

**Key insight:** The platform handles the animation loop (`requestAnimationFrame`) and calls these pure functions. The physics laws are in the user code.

## Current BosatsuUI Architecture

```
Bosatsu/UI library (ui.bosatsu):
- VNode: Virtual DOM nodes
- State[a], read, write: Reactive state
- h, text, fragment: DOM construction
- on_click, on_input, etc.: Event handlers
- ListState[a]: Dynamic lists

UIAnalyzer (compile-time):
- Extracts state→DOM bindings from TypedExpr
- Identifies event handlers and their effects
- Produces binding map for O(1) DOM updates

UIGen (code generation):
- Generates HTML with embedded JS runtime
- State management runtime
- Direct DOM update functions
```

## Proposed Extension: Canvas Module

### New Bosatsu/Canvas Library

```bosatsu
package Bosatsu/Canvas

export (
  CanvasCommands,
  clear, fill, stroke, line_width,
  rect, circle, line, text,
  arc, polygon, rounded_rect,
  save, restore, translate, rotate, scale,
  gradient_bar, axis
)

# List of drawing commands - pure data, no side effects
external struct CanvasCommands

# Colors and styles
external def fill(color: String) -> CanvasCommands
external def stroke(color: String) -> CanvasCommands
external def line_width(width: Float) -> CanvasCommands

# Shapes
external def rect(x: Float, y: Float, w: Float, h: Float) -> CanvasCommands
external def circle(x: Float, y: Float, radius: Float) -> CanvasCommands
external def line(x1: Float, y1: Float, x2: Float, y2: Float) -> CanvasCommands
external def text(content: String, x: Float, y: Float) -> CanvasCommands

# Transforms
external def save() -> CanvasCommands
external def restore() -> CanvasCommands
external def translate(x: Float, y: Float) -> CanvasCommands
external def rotate(angle: Float) -> CanvasCommands

# Composition
external def sequence(commands: List[CanvasCommands]) -> CanvasCommands
```

### Animation Hook

```bosatsu
package Bosatsu/Animation

# Called by platform each frame with delta time
# User implements this to update physics
external def on_frame(update: Float -> Unit) -> Unit
```

### Example: Bouncing Ball in Bosatsu

```bosatsu
package BouncingBall

from Bosatsu/UI import State, state, read, write, h, text, on_click
from Bosatsu/Canvas import (
  CanvasCommands, clear, fill, stroke, circle, rect, line, sequence
)
from Bosatsu/Animation import on_frame
from Bosatsu/Predef import add, sub, times, div
from Bosatsu/Numeric import lt, gt, abs, neg

struct BallState(
  x: Float, y: Float, vx: Float, vy: Float,
  gravity: Float, bounciness: Float,
  running: Bool
)

ball = state(BallState(200.0, 50.0, 100.0, 0.0, 500.0, 0.8, True))

# PURE physics function - laws are in Bosatsu, not platform!
def update_physics(s: BallState, dt: Float) -> BallState:
  match s.running:
    case False: s
    case True:
      # Apply gravity
      vy1 = add(s.vy, times(s.gravity, dt))
      # Update position
      x1 = add(s.x, times(s.vx, dt))
      y1 = add(s.y, times(vy1, dt))
      # Bounce off floor (y > 280)
      (y2, vy2) = match gt(y1, 280.0):
        case True: (280.0, neg(times(vy1, s.bounciness)))
        case False: (y1, vy1)
      # Bounce off walls
      (x2, vx2) = match lt(x1, 20.0):
        case True: (20.0, neg(times(s.vx, s.bounciness)))
        case False:
          match gt(x1, 380.0):
            case True: (380.0, neg(times(s.vx, s.bounciness)))
            case False: (x1, s.vx)
      BallState(x2, y2, vx2, vy2, s.gravity, s.bounciness, s.running)

# PURE render function - returns commands, platform executes
def render_ball(s: BallState) -> CanvasCommands:
  sequence([
    clear("#1a1a2e"),
    fill("#ff6b8a"),
    circle(s.x, s.y, 20.0)
  ])

# Frame handler - called by platform
frame_update = on_frame(
  \dt -> write(ball, update_physics(read(ball), dt))
)

# UI for controls
main = h("div", [], [
  h("canvas", [("id", "simulation"), ("width", "400"), ("height", "300")], []),
  h("button", [on_click(\_ -> write(ball,
    BallState(200.0, 50.0, 100.0, 0.0, 500.0, 0.8, True)
  ))], [text("Reset")])
])
```

## Why This Approach

### 1. Physics Laws in Bosatsu
- `update_physics` is pure Bosatsu code
- Laws (gravity, collision) are explicit and reviewable
- No JS hidden in the platform

### 2. Leverages Existing BosatsuUI
- Same `State`, `read`, `write` patterns
- Same event handlers (`on_click`)
- Same VNode construction (`h`, `text`)

### 3. Canvas as Data
- `CanvasCommands` is pure data (like VNode)
- Render function is pure - returns commands
- Platform interprets commands via canvas API

### 4. Animation Hook
- `on_frame` registers update function
- Platform calls with delta time each frame
- State updates trigger re-render

## Implementation Strategy

### Phase 1: Canvas Commands DSL
1. Add `Bosatsu/Canvas` library with command types
2. Add `CanvasExternal` intrinsics in JsGen
3. Generate JS that builds command arrays

### Phase 2: Canvas Renderer
1. Add canvas runtime that executes commands
2. Hook into existing BosatsuUI runtime
3. Auto-redraw when canvas-bound state changes

### Phase 3: Animation Loop
1. Add `Bosatsu/Animation` library
2. Add `on_frame` external that registers callback
3. Platform runs `requestAnimationFrame` loop

### Phase 4: Pretty Rendering
1. Gradient support in canvas commands
2. Shadow/glow effects
3. Trail history for pendulum-style visualizations

## Decisions Made

1. **Canvas API:** Commands as data (like VNode) - pure, testable, matches existing patterns
2. **Animation loop:** `on_frame` callback - platform handles requestAnimationFrame, calls user function with dt
3. **Render triggering:** Experiment to find best approach (auto on state change vs combined with on_frame)

## Open Questions

1. **How to handle gradients?** Linear vs radial, color stops
2. **Trail visualization?** State needs history list, or separate trail state?
3. **Parameter sweeps?** Same as simulation-cli, or different?

## Key Decision: Commands vs Direct Calls

**Option A: Command Arrays (chosen)**
- Render returns `List[CanvasCommand]`
- Platform iterates and calls canvas API
- Pure, testable, inspectable

**Option B: Direct Canvas Effects**
- Each draw call is an IO effect
- Platform interprets effect stream
- More flexible but harder to analyze

We choose Option A because:
- Matches VNode pattern (pure data)
- Can statically analyze for optimizations
- Easier to serialize/debug
- No IO in user code

## Next Steps

1. Create `Bosatsu/Canvas` library
2. Implement `CanvasExternal` intrinsics
3. Build bouncing ball demo from Bosatsu source
4. Verify no hand-written JS in output
