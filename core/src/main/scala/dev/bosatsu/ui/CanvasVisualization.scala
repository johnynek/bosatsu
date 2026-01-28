package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._

/**
 * Canvas visualization component for interactive simulations.
 *
 * This provides a simple drawing API that simulations can use to
 * visualize their state. Unlike external charting libraries, this
 * is self-contained and works with Bosatsu's reactive runtime.
 *
 * Key features:
 * - Basic shapes: rect, circle, line, text, arc
 * - Coordinate transforms: translate, scale, rotate
 * - Styling: fill, stroke, lineWidth, font
 * - Automatic redraw on state changes
 */
object CanvasVisualization {

  /**
   * Configuration for canvas visualization.
   */
  case class CanvasConfig(
      width: Int = 500,
      height: Int = 300,
      backgroundColor: String = "#f8f9fa",
      autoRedraw: Boolean = true
  )

  /**
   * Generate JavaScript for the canvas drawing API.
   * This provides helper functions that simulations can call.
   */
  def generateCanvasAPI(canvasId: String = "simulation-canvas"): String = {
    s"""
// Canvas Visualization API
const _canvas = document.getElementById('$canvasId');
const _ctx = _canvas ? _canvas.getContext('2d') : null;

// Drawing state stack for save/restore
const _canvasState = {
  fillStyle: '#333',
  strokeStyle: '#333',
  lineWidth: 1,
  font: '14px system-ui'
};

// Clear the canvas
function _clear(color = '#f8f9fa') {
  if (!_ctx) return;
  _ctx.fillStyle = color;
  _ctx.fillRect(0, 0, _canvas.width, _canvas.height);
  _ctx.fillStyle = _canvasState.fillStyle;
}

// Set fill color
function _fill(color) {
  if (!_ctx) return;
  _canvasState.fillStyle = color;
  _ctx.fillStyle = color;
}

// Set stroke color
function _stroke(color) {
  if (!_ctx) return;
  _canvasState.strokeStyle = color;
  _ctx.strokeStyle = color;
}

// Set line width
function _lineWidth(width) {
  if (!_ctx) return;
  _canvasState.lineWidth = width;
  _ctx.lineWidth = width;
}

// Set font
function _font(font) {
  if (!_ctx) return;
  _canvasState.font = font;
  _ctx.font = font;
}

// Draw a filled rectangle
function _rect(x, y, width, height) {
  if (!_ctx) return;
  _ctx.fillRect(x, y, width, height);
}

// Draw a stroked rectangle
function _strokeRect(x, y, width, height) {
  if (!_ctx) return;
  _ctx.strokeRect(x, y, width, height);
}

// Draw a filled circle
function _circle(x, y, radius) {
  if (!_ctx) return;
  _ctx.beginPath();
  _ctx.arc(x, y, radius, 0, Math.PI * 2);
  _ctx.fill();
}

// Draw a stroked circle
function _strokeCircle(x, y, radius) {
  if (!_ctx) return;
  _ctx.beginPath();
  _ctx.arc(x, y, radius, 0, Math.PI * 2);
  _ctx.stroke();
}

// Draw a line
function _line(x1, y1, x2, y2) {
  if (!_ctx) return;
  _ctx.beginPath();
  _ctx.moveTo(x1, y1);
  _ctx.lineTo(x2, y2);
  _ctx.stroke();
}

// Draw text
function _text(text, x, y) {
  if (!_ctx) return;
  _ctx.fillText(text, x, y);
}

// Draw centered text
function _textCenter(text, x, y) {
  if (!_ctx) return;
  _ctx.textAlign = 'center';
  _ctx.fillText(text, x, y);
  _ctx.textAlign = 'left';
}

// Draw an arc (for pie charts, gauges)
function _arc(x, y, radius, startAngle, endAngle, fill = true) {
  if (!_ctx) return;
  _ctx.beginPath();
  _ctx.arc(x, y, radius, startAngle, endAngle);
  if (fill) {
    _ctx.lineTo(x, y);
    _ctx.fill();
  } else {
    _ctx.stroke();
  }
}

// Draw a filled polygon from points
function _polygon(points) {
  if (!_ctx || points.length < 3) return;
  _ctx.beginPath();
  _ctx.moveTo(points[0][0], points[0][1]);
  for (let i = 1; i < points.length; i++) {
    _ctx.lineTo(points[i][0], points[i][1]);
  }
  _ctx.closePath();
  _ctx.fill();
}

// Draw a stroked polygon
function _strokePolygon(points) {
  if (!_ctx || points.length < 3) return;
  _ctx.beginPath();
  _ctx.moveTo(points[0][0], points[0][1]);
  for (let i = 1; i < points.length; i++) {
    _ctx.lineTo(points[i][0], points[i][1]);
  }
  _ctx.closePath();
  _ctx.stroke();
}

// Save canvas state
function _save() {
  if (!_ctx) return;
  _ctx.save();
}

// Restore canvas state
function _restore() {
  if (!_ctx) return;
  _ctx.restore();
}

// Translate origin
function _translate(x, y) {
  if (!_ctx) return;
  _ctx.translate(x, y);
}

// Scale
function _scale(sx, sy) {
  if (!_ctx) return;
  _ctx.scale(sx, sy !== undefined ? sy : sx);
}

// Rotate (in radians)
function _rotate(angle) {
  if (!_ctx) return;
  _ctx.rotate(angle);
}

// Draw a rounded rectangle
function _roundedRect(x, y, width, height, radius) {
  if (!_ctx) return;
  _ctx.beginPath();
  _ctx.moveTo(x + radius, y);
  _ctx.lineTo(x + width - radius, y);
  _ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
  _ctx.lineTo(x + width, y + height - radius);
  _ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
  _ctx.lineTo(x + radius, y + height);
  _ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
  _ctx.lineTo(x, y + radius);
  _ctx.quadraticCurveTo(x, y, x + radius, y);
  _ctx.closePath();
  _ctx.fill();
}

// Draw a gradient bar (for visualizing ranges)
function _gradientBar(x, y, width, height, value, min, max, lowColor, highColor) {
  if (!_ctx) return;
  const normalized = Math.max(0, Math.min(1, (value - min) / (max - min)));
  const gradient = _ctx.createLinearGradient(x, y, x + width, y);
  gradient.addColorStop(0, lowColor);
  gradient.addColorStop(1, highColor);

  // Background
  _ctx.fillStyle = '#e0e0e0';
  _ctx.fillRect(x, y, width, height);

  // Fill based on value
  _ctx.fillStyle = gradient;
  _ctx.fillRect(x, y, width * normalized, height);

  // Border
  _ctx.strokeStyle = '#999';
  _ctx.strokeRect(x, y, width, height);

  // Restore fill
  _ctx.fillStyle = _canvasState.fillStyle;
}

// Draw an axis with labels
function _axis(x, y, length, isVertical, min, max, ticks = 5, label = '') {
  if (!_ctx) return;

  _ctx.strokeStyle = '#333';
  _ctx.fillStyle = '#666';
  _ctx.font = '12px system-ui';

  if (isVertical) {
    // Vertical axis
    _line(x, y, x, y - length);
    for (let i = 0; i <= ticks; i++) {
      const tickY = y - (length * i / ticks);
      const value = min + (max - min) * i / ticks;
      _line(x - 5, tickY, x, tickY);
      _ctx.textAlign = 'right';
      _ctx.fillText(value.toFixed(0), x - 8, tickY + 4);
    }
    if (label) {
      _ctx.save();
      _ctx.translate(x - 40, y - length / 2);
      _ctx.rotate(-Math.PI / 2);
      _ctx.textAlign = 'center';
      _ctx.fillText(label, 0, 0);
      _ctx.restore();
    }
  } else {
    // Horizontal axis
    _line(x, y, x + length, y);
    for (let i = 0; i <= ticks; i++) {
      const tickX = x + (length * i / ticks);
      const value = min + (max - min) * i / ticks;
      _line(tickX, y, tickX, y + 5);
      _ctx.textAlign = 'center';
      _ctx.fillText(value.toFixed(0), tickX, y + 18);
    }
    if (label) {
      _ctx.textAlign = 'center';
      _ctx.fillText(label, x + length / 2, y + 35);
    }
  }

  // Restore state
  _ctx.fillStyle = _canvasState.fillStyle;
  _ctx.strokeStyle = _canvasState.strokeStyle;
  _ctx.textAlign = 'left';
}

// Visualization redraw callback - set this in your simulation
let _visualize = null;

// Register a visualization function
function _onVisualize(fn) {
  _visualize = fn;
}

// Called after each recompute to update visualization
function _redrawVisualization() {
  if (_visualize && _ctx) {
    _visualize();
  }
}
"""
  }

  /**
   * CSS for canvas visualization.
   */
  val canvasCSS: String =
    """.simulation-canvas {
      |  display: block;
      |  width: 100%;
      |  border-radius: 8px;
      |  background: #f8f9fa;
      |  margin-bottom: 16px;
      |}
      |.canvas-container {
      |  position: relative;
      |  margin: 12px 0;
      |}
      |.canvas-overlay {
      |  position: absolute;
      |  top: 8px;
      |  right: 8px;
      |  background: rgba(255,255,255,0.9);
      |  padding: 4px 8px;
      |  border-radius: 4px;
      |  font-size: 12px;
      |  color: #666;
      |}""".stripMargin
}
