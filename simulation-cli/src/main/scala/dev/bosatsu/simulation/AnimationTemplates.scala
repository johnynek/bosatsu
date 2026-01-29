package dev.bosatsu.simulation

/**
 * Generic animation template generator.
 *
 * Physics expressions are defined in the .bosatsu file.
 * State type (cartesian vs angular) determines the state structure.
 * This template provides the animation loop and canvas infrastructure.
 */
object AnimationTemplates {

  def generate(
      title: String,
      theme: SimulationCommand.Theme,
      params: Map[String, String],
      canvasWidth: Int,
      canvasHeight: Int
  ): String = {
    val themeColors = theme match {
      case SimulationCommand.Theme.Light =>
        ("--bg-color: #f5f5f5", "--text-color: #333333", "--surface-color: #ffffff")
      case SimulationCommand.Theme.Dark =>
        ("--bg-color: #1a1a2e", "--text-color: #e0e0e0", "--surface-color: #16162a")
    }

    def clean(s: String): String = s.replace("\"", "")

    val stateType = clean(params.getOrElse("state_type", "cartesian"))

    stateType match {
      case "angular" => generateAngular(title, themeColors, params, canvasWidth, canvasHeight, clean)
      case _ => generateCartesian(title, themeColors, params, canvasWidth, canvasHeight, clean)
    }
  }

  private def generateCartesian(
      title: String,
      themeColors: (String, String, String),
      params: Map[String, String],
      canvasWidth: Int,
      canvasHeight: Int,
      clean: String => String
  ): String = {
    // Read physics expressions from .bosatsu
    val physicsX = clean(params.getOrElse("physics_x", "x"))
    val physicsY = clean(params.getOrElse("physics_y", "y"))
    val physicsVy = clean(params.getOrElse("physics_vy", "vy"))
    val physicsVx = clean(params.getOrElse("physics_vx", "vx"))

    val bounceFloor = clean(params.getOrElse("bounce_floor", "false"))
    val bounceCeiling = clean(params.getOrElse("bounce_ceiling", "false"))
    val bounceLeft = clean(params.getOrElse("bounce_left", "false"))
    val bounceRight = clean(params.getOrElse("bounce_right", "false"))
    val bounceFactor = clean(params.getOrElse("bounce_factor", "1"))

    val shapeColor = clean(params.getOrElse("shape_color", "#e94560"))
    val shapeHighlight = clean(params.getOrElse("shape_highlight", "#ff6b8a"))

    val initialX = params.getOrElse("initial_x", "200")
    val initialY = params.getOrElse("initial_y", "50")
    val initialVx = params.getOrElse("initial_vx", "0")
    val initialVy = params.getOrElse("initial_vy", "0")
    val ballRadius = params.getOrElse("ball_radius", "20")
    val gravity = params.getOrElse("gravity", "500")
    val bounciness = params.getOrElse("bounciness", "80")

    s"""<!DOCTYPE html>
<html lang="en" data-theme="${theme(themeColors)}">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>$title</title>
  ${styles(canvasWidth, themeColors)}
</head>
<body>
  <div class="applet-container">
    <h1 class="applet-title">$title</h1>
    <p class="applet-subtitle">Generated from Bosatsu</p>
    <canvas id="simulation-canvas" class="simulation-canvas" width="$canvasWidth" height="$canvasHeight"></canvas>
    <div class="controls-section">
      <div class="control-group">
        <div class="control-header"><span class="control-label">Gravity</span><span class="control-value" id="gravity-display">$gravity</span></div>
        <input type="range" id="gravity-slider" min="0" max="1000" value="$gravity">
      </div>
      <div class="control-group">
        <div class="control-header"><span class="control-label">Bounciness</span><span class="control-value" id="bounciness-display">$bounciness%</span></div>
        <input type="range" id="bounciness-slider" min="0" max="100" value="$bounciness">
      </div>
      <div class="stats-row"><span class="stats-label">Position:</span><span class="stats-value">(<span id="x-display">$initialX</span>, <span id="y-display">$initialY</span>)</span></div>
      <div class="stats-row"><span class="stats-label">Velocity:</span><span class="stats-value">(<span id="vx-display">$initialVx</span>, <span id="vy-display">$initialVy</span>)</span></div>
      <div class="button-row">
        <button class="btn" onclick="resetSimulation()">Reset</button>
        <button class="btn btn-secondary" id="pause-btn" onclick="togglePause()">Pause</button>
      </div>
    </div>
  </div>

  <script>
    const state = {
      x: $initialX, y: $initialY, vx: $initialVx, vy: $initialVy,
      gravity: $gravity, bounciness: $bounciness,
      width: $canvasWidth, height: $canvasHeight, ballRadius: $ballRadius,
      running: true
    };

    const canvas = document.getElementById('simulation-canvas');
    const ctx = canvas.getContext('2d');

    function updatePhysics(dt) {
      if (!state.running) return;
      const { x, y, vx, vy, gravity, bounciness, width, height, ballRadius } = state;

      let newVy = $physicsVy;
      let newY = $physicsY;
      let newX = $physicsX;
      let newVx = $physicsVx;
      const b = $bounceFactor;

      if ($bounceFloor) { newY = height - ballRadius; newVy = -Math.abs(newVy) * b; }
      if ($bounceCeiling) { newY = ballRadius; newVy = Math.abs(newVy) * b; }
      if ($bounceLeft) { newX = ballRadius; newVx = Math.abs(newVx) * b; }
      if ($bounceRight) { newX = width - ballRadius; newVx = -Math.abs(newVx) * b; }

      state.x = newX; state.y = newY; state.vx = newVx; state.vy = newVy;
    }

    function render() {
      const { x, y, width, height, ballRadius } = state;
      const bgGradient = ctx.createLinearGradient(0, 0, 0, height);
      bgGradient.addColorStop(0, '#1a1a2e'); bgGradient.addColorStop(1, '#16162a');
      ctx.fillStyle = bgGradient; ctx.fillRect(0, 0, width, height);

      ctx.strokeStyle = 'rgba(255,255,255,0.03)'; ctx.lineWidth = 1;
      for (let gx = 0; gx <= width; gx += 40) { ctx.beginPath(); ctx.moveTo(gx, 0); ctx.lineTo(gx, height); ctx.stroke(); }
      for (let gy = 0; gy <= height; gy += 40) { ctx.beginPath(); ctx.moveTo(0, gy); ctx.lineTo(width, gy); ctx.stroke(); }

      ctx.beginPath(); ctx.ellipse(x, height - 5, ballRadius * 0.8, ballRadius * 0.2, 0, 0, Math.PI * 2);
      ctx.fillStyle = 'rgba(0,0,0,0.3)'; ctx.fill();

      const shapeGradient = ctx.createRadialGradient(x - ballRadius * 0.3, y - ballRadius * 0.3, 0, x, y, ballRadius);
      shapeGradient.addColorStop(0, '$shapeHighlight'); shapeGradient.addColorStop(1, '$shapeColor');
      ctx.beginPath(); ctx.arc(x, y, ballRadius, 0, Math.PI * 2); ctx.fillStyle = shapeGradient; ctx.fill();
    }

    function updateDisplays() {
      document.getElementById('x-display').textContent = state.x.toFixed(0);
      document.getElementById('y-display').textContent = state.y.toFixed(0);
      document.getElementById('vx-display').textContent = state.vx.toFixed(0);
      document.getElementById('vy-display').textContent = state.vy.toFixed(0);
    }

    document.getElementById('gravity-slider').addEventListener('input', e => {
      state.gravity = parseInt(e.target.value);
      document.getElementById('gravity-display').textContent = e.target.value;
    });
    document.getElementById('bounciness-slider').addEventListener('input', e => {
      state.bounciness = parseFloat(e.target.value);
      document.getElementById('bounciness-display').textContent = e.target.value + '%';
    });

    function resetSimulation() {
      state.x = $initialX; state.y = $initialY; state.vx = $initialVx; state.vy = $initialVy;
      state.running = true; document.getElementById('pause-btn').textContent = 'Pause';
    }
    function togglePause() {
      state.running = !state.running;
      document.getElementById('pause-btn').textContent = state.running ? 'Pause' : 'Resume';
    }

    let lastTime = performance.now();
    function animate() {
      const now = performance.now();
      const dt = Math.min((now - lastTime) / 1000, 0.1);
      lastTime = now;
      updatePhysics(dt); render(); updateDisplays();
      requestAnimationFrame(animate);
    }
    animate();
  </script>
</body>
</html>"""
  }

  private def generateAngular(
      title: String,
      themeColors: (String, String, String),
      params: Map[String, String],
      canvasWidth: Int,
      canvasHeight: Int,
      clean: String => String
  ): String = {
    // Read physics expressions from .bosatsu
    val physicsAngularAccel = clean(params.getOrElse("physics_angular_accel", "0"))
    val physicsAngularVelocity = clean(params.getOrElse("physics_angular_velocity", "angularVelocity"))
    val physicsAngle = clean(params.getOrElse("physics_angle", "angle"))
    val physicsBobX = clean(params.getOrElse("physics_bob_x", "pivotX"))
    val physicsBobY = clean(params.getOrElse("physics_bob_y", "pivotY + pendulumLength"))

    val bobColor = clean(params.getOrElse("bob_color", "#667eea"))
    val bobHighlight = clean(params.getOrElse("bob_highlight", "#764ba2"))
    val rodColor = clean(params.getOrElse("rod_color", "#4a5568"))

    val initialAngle = params.getOrElse("initial_angle", "785")
    val initialAngularVelocity = params.getOrElse("initial_angular_velocity", "0")
    val pendulumLength = params.getOrElse("pendulum_length", "200")
    val bobRadius = params.getOrElse("bob_radius", "20")
    val gravity = params.getOrElse("gravity", "10")
    val damping = params.getOrElse("damping", "99")
    val pivotX = params.getOrElse("pivot_x", (canvasWidth / 2).toString)
    val pivotY = params.getOrElse("pivot_y", "50")

    s"""<!DOCTYPE html>
<html lang="en" data-theme="${theme(themeColors)}">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>$title</title>
  ${styles(canvasWidth, themeColors)}
</head>
<body>
  <div class="applet-container">
    <h1 class="applet-title">$title</h1>
    <p class="applet-subtitle">Generated from Bosatsu</p>
    <canvas id="simulation-canvas" class="simulation-canvas" width="$canvasWidth" height="$canvasHeight"></canvas>
    <div class="controls-section">
      <div class="control-group">
        <div class="control-header"><span class="control-label">Length</span><span class="control-value" id="length-display">${pendulumLength}px</span></div>
        <input type="range" id="length-slider" min="50" max="250" value="$pendulumLength">
      </div>
      <div class="control-group">
        <div class="control-header"><span class="control-label">Gravity</span><span class="control-value" id="gravity-display">$gravity</span></div>
        <input type="range" id="gravity-slider" min="1" max="30" value="$gravity">
      </div>
      <div class="control-group">
        <div class="control-header"><span class="control-label">Damping</span><span class="control-value" id="damping-display">$damping%</span></div>
        <input type="range" id="damping-slider" min="90" max="100" value="$damping">
      </div>
      <div class="button-row">
        <button class="btn" onclick="resetSimulation()">Reset</button>
        <button class="btn btn-secondary" id="pause-btn" onclick="togglePause()">Pause</button>
      </div>
    </div>
  </div>

  <script>
    const state = {
      angle: $initialAngle, angularVelocity: $initialAngularVelocity,
      pendulumLength: $pendulumLength, bobRadius: $bobRadius,
      gravity: $gravity, damping: $damping,
      pivotX: $pivotX, pivotY: $pivotY,
      width: $canvasWidth, height: $canvasHeight,
      running: true, trail: []
    };

    const canvas = document.getElementById('simulation-canvas');
    const ctx = canvas.getContext('2d');

    function updatePhysics(dt) {
      if (!state.running) return;
      const { angle, angularVelocity, pendulumLength, gravity, damping, pivotX, pivotY } = state;

      const angularAccel = $physicsAngularAccel;
      const newAngularVelocity = $physicsAngularVelocity;
      const newAngle = $physicsAngle;
      const bobX = $physicsBobX;
      const bobY = $physicsBobY;

      state.angle = newAngle;
      state.angularVelocity = newAngularVelocity;
      state.trail = [{ x: bobX, y: bobY, age: 0 }, ...state.trail.map(p => ({ ...p, age: p.age + 1 })).filter(p => p.age < 50)];
    }

    function render() {
      const { angle, pendulumLength, pivotX, pivotY, bobRadius, width, height, trail } = state;
      const bobX = pivotX + pendulumLength * Math.sin(angle / 1000);
      const bobY = pivotY + pendulumLength * Math.cos(angle / 1000);

      const gradient = ctx.createLinearGradient(0, 0, 0, height);
      gradient.addColorStop(0, '#0f0f1a'); gradient.addColorStop(1, '#1a1a2e');
      ctx.fillStyle = gradient; ctx.fillRect(0, 0, width, height);

      trail.forEach(point => {
        const alpha = 1 - point.age / 50;
        ctx.beginPath(); ctx.arc(point.x, point.y, 3, 0, Math.PI * 2);
        ctx.fillStyle = 'rgba(102, 126, 234, ' + (alpha * 0.5) + ')'; ctx.fill();
      });

      ctx.beginPath(); ctx.moveTo(pivotX, pivotY); ctx.lineTo(bobX, bobY);
      ctx.strokeStyle = '$rodColor'; ctx.lineWidth = 3; ctx.stroke();

      ctx.beginPath(); ctx.arc(pivotX, pivotY, 8, 0, Math.PI * 2);
      ctx.fillStyle = '#2d3748'; ctx.fill();
      ctx.strokeStyle = '$rodColor'; ctx.lineWidth = 2; ctx.stroke();

      const bobGradient = ctx.createRadialGradient(bobX - 5, bobY - 5, 0, bobX, bobY, bobRadius);
      bobGradient.addColorStop(0, '$bobHighlight'); bobGradient.addColorStop(1, '$bobColor');
      ctx.beginPath(); ctx.arc(bobX, bobY, bobRadius, 0, Math.PI * 2);
      ctx.fillStyle = bobGradient; ctx.fill();
    }

    function updateDisplays() {}

    document.getElementById('length-slider').addEventListener('input', e => {
      state.pendulumLength = parseFloat(e.target.value);
      document.getElementById('length-display').textContent = e.target.value + 'px';
    });
    document.getElementById('gravity-slider').addEventListener('input', e => {
      state.gravity = parseFloat(e.target.value);
      document.getElementById('gravity-display').textContent = e.target.value;
    });
    document.getElementById('damping-slider').addEventListener('input', e => {
      state.damping = parseFloat(e.target.value);
      document.getElementById('damping-display').textContent = e.target.value + '%';
    });

    function resetSimulation() {
      state.angle = $initialAngle; state.angularVelocity = $initialAngularVelocity;
      state.trail = []; state.running = true;
      document.getElementById('pause-btn').textContent = 'Pause';
    }
    function togglePause() {
      state.running = !state.running;
      document.getElementById('pause-btn').textContent = state.running ? 'Pause' : 'Resume';
    }

    let lastTime = performance.now();
    function animate() {
      const now = performance.now();
      const dt = Math.min((now - lastTime) / 1000, 0.1);
      lastTime = now;
      updatePhysics(dt); render(); updateDisplays();
      requestAnimationFrame(animate);
    }
    animate();
  </script>
</body>
</html>"""
  }

  private def theme(themeColors: (String, String, String)): String =
    if (themeColors._1.contains("#f5f5f5")) "light" else "dark"

  private def styles(canvasWidth: Int, themeColors: (String, String, String)): String = s"""
  <style>
    :root { ${themeColors._1}; ${themeColors._2}; --accent-color: #667eea; ${themeColors._3}; }
    * { box-sizing: border-box; }
    body { font-family: system-ui, -apple-system, sans-serif; max-width: ${canvasWidth + 48}px; margin: 0 auto; padding: 1rem; background: var(--bg-color); color: var(--text-color); }
    .applet-container { background: var(--surface-color); border-radius: 12px; padding: 1.5rem; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }
    .applet-title { margin: 0 0 0.5rem 0; font-size: 1.5rem; }
    .applet-subtitle { margin: 0 0 1rem 0; font-size: 0.875rem; color: #666; }
    .simulation-canvas { width: 100%; border-radius: 8px; }
    .controls-section { margin-top: 1rem; padding-top: 1rem; border-top: 1px solid rgba(0,0,0,0.1); }
    .control-group { margin: 1rem 0; }
    .control-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem; }
    .control-label { font-size: 0.875rem; font-weight: 500; }
    .control-value { font-family: monospace; font-weight: bold; color: var(--accent-color); }
    input[type="range"] { width: 100%; height: 8px; -webkit-appearance: none; background: #ddd; border-radius: 4px; outline: none; }
    input[type="range"]::-webkit-slider-thumb { -webkit-appearance: none; width: 20px; height: 20px; background: var(--accent-color); border-radius: 50%; cursor: pointer; }
    .button-row { display: flex; gap: 0.5rem; margin-top: 1rem; }
    .btn { padding: 0.5rem 1rem; background: var(--accent-color); color: white; border: none; border-radius: 6px; cursor: pointer; font-size: 0.875rem; }
    .btn:hover { opacity: 0.9; }
    .btn-secondary { background: #6c757d; }
    .stats-row { display: flex; justify-content: space-between; padding: 0.5rem 0; border-bottom: 1px solid #eee; font-size: 0.875rem; }
    .stats-label { color: #666; }
    .stats-value { font-family: monospace; font-weight: 500; }
  </style>"""
}
