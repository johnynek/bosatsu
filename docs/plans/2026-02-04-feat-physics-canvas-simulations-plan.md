---
title: Physics Canvas Simulations
type: feat
date: 2026-02-04
---

# Physics Canvas Simulations for BosatsuUI

## Overview

Add canvas-based physics simulations to Bosatsu that are as visually appealing as burritoscript demos, with physics laws written in pure Bosatsu (not hardcoded in platform).

**Key constraints:**
- No hand-written JS - all code generated from Bosatsu source
- Physics laws in `.bosatsu` files, not platform
- Leverage existing BosatsuUI patterns (State, VNode, events)

## Technical Approach

### Architecture

```
.bosatsu file
    │
    ▼
┌────────────────────────────────────────────────────┐
│  Bosatsu Compiler + UIAnalyzer                     │
│                                                    │
│  1. Parse Bosatsu/Canvas imports                   │
│  2. Type-check canvas command expressions          │
│  3. Analyze state→canvas bindings                  │
│  4. Compile to JS with CanvasExternal intrinsics   │
└────────────────────────────────────────────────────┘
    │
    ▼
┌────────────────────────────────────────────────────┐
│  Generated HTML                                    │
│                                                    │
│  - Canvas element with id                          │
│  - State management runtime (existing)             │
│  - Canvas command executor runtime                 │
│  - Animation loop runtime (requestAnimationFrame)  │
│  - Event handlers (existing)                       │
└────────────────────────────────────────────────────┘
```

### Static Optimization (Key Innovation)

Just like BosatsuUI detects state→DOM bindings at compile time for O(1) updates, we can detect state→canvas bindings:

```bosatsu
def render(s: BallState) -> CanvasCommands:
  sequence([
    clear("#1a1a2e"),           # static - never changes
    fill("#ff6b8a"),            # static
    circle(s.x, s.y, 20.0)      # depends on s.x, s.y only
  ])
```

**Compile-time analysis extracts:**
```javascript
// CanvasAnalyzer output
const _canvasBindings = {
  "ball.x": ["circle_0.x"],    // ball.x affects circle position
  "ball.y": ["circle_0.y"],
  // gravity, bounciness don't affect rendering!
};
```

**Runtime uses this for:**
1. **Partial re-render** - only redraw commands affected by changed state
2. **Layered canvas** - static elements on background layer, animated on foreground
3. **Dirty region tracking** - only clear/redraw the bounding box that changed

This means:
- User writes simple, readable Bosatsu code
- Compiler extracts optimizations automatically
- No need for user to write optimized (complex) code

### Canvas Commands as Data

Canvas rendering returns pure data (like VNode), not effects:

```bosatsu
# Returns List[CanvasCommand] - pure data
def render(s: BallState) -> CanvasCommands:
  sequence([
    clear("#1a1a2e"),
    fill("#ff6b8a"),
    circle(s.x, s.y, 20.0)
  ])
```

Platform interprets commands:
```javascript
function executeCommands(cmds, ctx) {
  cmds.forEach(cmd => {
    switch(cmd.type) {
      case 'clear': ctx.fillStyle = cmd.color; ctx.fillRect(0,0,w,h); break;
      case 'circle': ctx.beginPath(); ctx.arc(cmd.x, cmd.y, cmd.r, 0, 2*PI); ctx.fill(); break;
      // ...
    }
  });
}
```

## Implementation Phases

### Phase 1: Bosatsu/Canvas Library
- [ ] Create `core/src/main/resources/bosatsu/canvas.bosatsu`
  - CanvasCommands struct (external)
  - Shape commands: rect, circle, line, text, arc, polygon
  - Style commands: fill, stroke, line_width, font
  - Transform commands: save, restore, translate, rotate, scale
  - Composition: sequence, clear
- [ ] Create `CanvasExternal.scala` in codegen/js
  - Intrinsics for canvas commands
  - Compile commands to JS objects

### Phase 2: Bosatsu/Animation Library
- [ ] Create `core/src/main/resources/bosatsu/animation.bosatsu`
  - on_frame: register update callback
  - Receives delta time (Float) each frame
- [ ] Add `AnimationExternal.scala` intrinsics
  - Generate requestAnimationFrame loop
  - Track last timestamp for dt calculation

### Phase 3: CanvasAnalyzer (Static Optimization)
- [ ] Create `CanvasAnalyzer.scala` in ui package
  - Walk TypedExpr of render function
  - Extract state→command dependencies
  - Identify static vs dynamic commands
- [ ] Generate optimization hints
  - Which commands depend on which state fields
  - Bounding boxes for dirty region tracking
  - Layering opportunities (static background vs animated foreground)

### Phase 4: Canvas Runtime
- [ ] Add canvas executor to UIGen runtime
  - executeCanvasCommands(cmds, ctx)
  - Handle all command types
- [ ] Add optimized rendering path
  - Use CanvasAnalyzer output for partial re-render
  - Background/foreground layer separation
  - Dirty region clearing

### Phase 5: Integration
- [ ] Wire animation loop to canvas re-render
  - on_frame callback updates state
  - State change triggers canvas re-render (using CanvasAnalyzer hints)
- [ ] Add canvas config to UICommand
  - --canvas flag enables canvas runtime
  - Canvas dimensions from h() props

### Phase 6: Demo - Bouncing Ball
- [ ] Create `demos/physics/bouncing-ball.bosatsu`
  - BallState struct with physics values
  - Pure update_physics function
  - Pure render_ball function
  - UI controls (reset button, sliders)
- [ ] Verify no hand-written JS
  - Check generated HTML source
  - All physics code from Bosatsu
- [ ] Verify optimizations applied
  - Check that only ball position triggers redraw
  - Background layer is static

### Phase 7: Demo - Pendulum
- [ ] Create `demos/physics/pendulum.bosatsu`
  - PendulumState with angular motion
  - Energy calculation
  - Trail visualization (uses list for history)
- [ ] Add gradient support to canvas commands
  - linear_gradient, radial_gradient
  - Multiple color stops

### Phase 8: Pretty Rendering
- [ ] Gradient commands
  - linear_gradient(x1, y1, x2, y2, stops)
  - radial_gradient(x, y, r, stops)
- [ ] Shadow/glow effects
  - shadow_blur, shadow_color, shadow_offset
- [ ] Bezier curves
  - bezier_to, quadratic_to

## Acceptance Criteria

### Functional Requirements
- [ ] Bouncing ball simulation runs smoothly (60fps)
- [ ] Physics laws are in .bosatsu source (grep confirms no hardcoded physics in platform)
- [ ] Canvas renders correctly (ball bounces, pendulum swings)
- [ ] User controls work (reset, parameter sliders)
- [ ] No hand-written JS in generated HTML

### Quality Gates
- [ ] All existing tests pass
- [ ] New tests for CanvasExternal intrinsics
- [ ] New tests for animation loop
- [ ] Demo HTML files generated from Bosatsu source only

## Files to Create/Modify

| File | Purpose |
|------|---------|
| `core/src/main/resources/bosatsu/canvas.bosatsu` | Canvas command library |
| `core/src/main/resources/bosatsu/animation.bosatsu` | Animation loop library |
| `core/src/main/scala/dev/bosatsu/codegen/js/CanvasExternal.scala` | JS intrinsics for canvas |
| `core/src/main/scala/dev/bosatsu/codegen/js/AnimationExternal.scala` | JS intrinsics for animation |
| `core/src/main/scala/dev/bosatsu/ui/CanvasAnalyzer.scala` | **Static analysis for canvas optimizations** |
| `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` | Add canvas binding detection |
| `core/src/main/scala/dev/bosatsu/ui/UIGen.scala` | Add canvas runtime to output |
| `demos/physics/bouncing-ball.bosatsu` | Bouncing ball demo |
| `demos/physics/pendulum.bosatsu` | Pendulum demo |

## References

- Brainstorm: `docs/brainstorms/2026-02-04-physics-canvas-simulations-brainstorm.md`
- Burritoscript bouncing ball: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/simulation-applets/framework/bouncing-ball.burrito.ts`
- Burritoscript pendulum: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/simulation-applets/framework/pendulum.burrito.ts`
- Existing canvas API: `core/src/main/scala/dev/bosatsu/ui/CanvasVisualization.scala`
- Existing UI library: `core/src/main/resources/bosatsu/ui.bosatsu`
