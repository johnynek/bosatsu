package dev.bosatsu.ui

import munit.FunSuite

class CanvasVisualizationTest extends FunSuite {

  // =========================================================================
  // CanvasConfig
  // =========================================================================

  test("CanvasConfig has correct defaults") {
    val config = CanvasVisualization.CanvasConfig()
    assertEquals(config.width, 500)
    assertEquals(config.height, 300)
    assertEquals(config.backgroundColor, "#f8f9fa")
    assertEquals(config.autoRedraw, true)
  }

  test("CanvasConfig can override all fields") {
    val config = CanvasVisualization.CanvasConfig(
      width = 800,
      height = 600,
      backgroundColor = "#000000",
      autoRedraw = false
    )
    assertEquals(config.width, 800)
    assertEquals(config.height, 600)
    assertEquals(config.backgroundColor, "#000000")
    assertEquals(config.autoRedraw, false)
  }

  // =========================================================================
  // generateCanvasAPI
  // =========================================================================

  test("generateCanvasAPI produces JavaScript string") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.nonEmpty)
    assert(api.contains("Canvas Visualization API"))
  }

  test("generateCanvasAPI uses default canvas ID") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("simulation-canvas"))
  }

  test("generateCanvasAPI uses custom canvas ID") {
    val api = CanvasVisualization.generateCanvasAPI("my-custom-canvas")
    assert(api.contains("my-custom-canvas"))
    assert(!api.contains("simulation-canvas"))
  }

  test("generateCanvasAPI includes _clear function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _clear"))
    assert(api.contains("fillRect"))
  }

  test("generateCanvasAPI includes _fill function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _fill"))
    assert(api.contains("fillStyle"))
  }

  test("generateCanvasAPI includes _stroke function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _stroke"))
    assert(api.contains("strokeStyle"))
  }

  test("generateCanvasAPI includes _lineWidth function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _lineWidth"))
    assert(api.contains("lineWidth"))
  }

  test("generateCanvasAPI includes _font function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _font"))
    assert(api.contains(".font"))
  }

  test("generateCanvasAPI includes _rect function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _rect"))
    assert(api.contains("fillRect"))
  }

  test("generateCanvasAPI includes _strokeRect function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _strokeRect"))
    assert(api.contains("strokeRect"))
  }

  test("generateCanvasAPI includes _circle function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _circle"))
    assert(api.contains("beginPath"))
    assert(api.contains("arc"))
  }

  test("generateCanvasAPI includes canvas state object") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("_canvasState"))
    assert(api.contains("fillStyle: '#333'"))
    assert(api.contains("strokeStyle: '#333'"))
    assert(api.contains("lineWidth: 1"))
  }

  test("generateCanvasAPI gets 2D context") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("getContext('2d')"))
  }

  test("generateCanvasAPI handles null context gracefully") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("if (!_ctx) return"))
  }

  test("generateCanvasAPI includes default font") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("14px system-ui"))
  }

  test("generateCanvasAPI includes default background color") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("#f8f9fa"))
  }

  // =========================================================================
  // Additional drawing functions in generateCanvasAPI
  // =========================================================================

  test("generateCanvasAPI includes _strokeCircle function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _strokeCircle"))
  }

  test("generateCanvasAPI includes _line function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _line"))
    assert(api.contains("moveTo"))
    assert(api.contains("lineTo"))
  }

  test("generateCanvasAPI includes _text function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _text"))
    assert(api.contains("fillText"))
  }

  test("generateCanvasAPI includes _textCenter function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _textCenter"))
    assert(api.contains("textAlign"))
  }

  test("generateCanvasAPI includes _arc function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _arc"))
  }

  test("generateCanvasAPI includes _polygon function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _polygon"))
    assert(api.contains("closePath"))
  }

  test("generateCanvasAPI includes _strokePolygon function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _strokePolygon"))
  }

  test("generateCanvasAPI includes _save function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _save"))
    assert(api.contains(".save()"))
  }

  test("generateCanvasAPI includes _restore function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _restore"))
    assert(api.contains(".restore()"))
  }

  test("generateCanvasAPI includes _translate function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _translate"))
    assert(api.contains(".translate("))
  }

  test("generateCanvasAPI includes _scale function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _scale"))
    assert(api.contains(".scale("))
  }

  test("generateCanvasAPI includes _rotate function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _rotate"))
    assert(api.contains(".rotate("))
  }

  test("generateCanvasAPI includes _roundedRect function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _roundedRect"))
    assert(api.contains("quadraticCurveTo"))
  }

  test("generateCanvasAPI includes _gradientBar function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _gradientBar"))
    assert(api.contains("createLinearGradient"))
  }

  test("generateCanvasAPI includes _axis function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _axis"))
    assert(api.contains("isVertical"))
  }

  test("generateCanvasAPI includes _onVisualize function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _onVisualize"))
    assert(api.contains("_visualize"))
  }

  test("generateCanvasAPI includes _redrawVisualization function") {
    val api = CanvasVisualization.generateCanvasAPI()
    assert(api.contains("function _redrawVisualization"))
  }

  // =========================================================================
  // canvasCSS
  // =========================================================================

  test("canvasCSS produces valid CSS") {
    val css = CanvasVisualization.canvasCSS
    assert(css.nonEmpty)
  }

  test("canvasCSS includes simulation-canvas class") {
    val css = CanvasVisualization.canvasCSS
    assert(css.contains(".simulation-canvas"))
    assert(css.contains("display: block"))
    assert(css.contains("border-radius"))
  }

  test("canvasCSS includes canvas-container class") {
    val css = CanvasVisualization.canvasCSS
    assert(css.contains(".canvas-container"))
    assert(css.contains("position: relative"))
  }

  test("canvasCSS includes canvas-overlay class") {
    val css = CanvasVisualization.canvasCSS
    assert(css.contains(".canvas-overlay"))
    assert(css.contains("position: absolute"))
  }
}
