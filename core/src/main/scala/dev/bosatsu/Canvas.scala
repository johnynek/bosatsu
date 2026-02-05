package dev.bosatsu

import Value._

/** Canvas module for BosatsuUI canvas rendering (with marker functions at end).
  *
  * Extends BosatsuUI with canvas-based graphics for physics simulations.
  * Canvas commands are pure data - render functions return CanvasCommands
  * that the runtime executes on a canvas context.
  *
  * This module integrates with UIAnalyzer to detect canvas_render bindings
  * at compile time, enabling efficient state->canvas updates.
  */
object Canvas {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings. This lets us avoid resources which complicate matters for
    * scalajs.
    */
  private[bosatsu] inline def loadFileInCompile(file: String): String =
    ${ Macro.loadFileInCompileImpl('file) }

  /** String representation of the Canvas module */
  val canvasString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/canvas.bosatsu")

  val packageName: PackageName =
    PackageName.parse("Bosatsu/Canvas").get

  /** Canvas command representation for JVM execution */
  sealed trait CanvasCommand derives CanEqual
  case class Circle(x: Double, y: Double, radius: Double) extends CanvasCommand
  case class Rect(x: Double, y: Double, w: Double, h: Double) extends CanvasCommand
  case class Line(x1: Double, y1: Double, x2: Double, y2: Double) extends CanvasCommand
  case class TextCmd(content: String, x: Double, y: Double) extends CanvasCommand
  case class Arc(x: Double, y: Double, radius: Double, startAngle: Double, endAngle: Double) extends CanvasCommand
  case class Fill(color: String) extends CanvasCommand
  case class Stroke(color: String) extends CanvasCommand
  case class LineWidth(width: Double) extends CanvasCommand
  case class Clear(color: String) extends CanvasCommand
  case class Sequence(commands: List[CanvasCommand]) extends CanvasCommand
  case object Save extends CanvasCommand
  case object Restore extends CanvasCommand
  case class Translate(x: Double, y: Double) extends CanvasCommand
  case class Rotate(angle: Double) extends CanvasCommand
  case class Scale(sx: Double, sy: Double) extends CanvasCommand

  /** Helper to extract Double from Value */
  private def d(v: Value): Double = v match {
    case ExternalValue(d: java.lang.Double) => d.doubleValue
    case other =>
      sys.error(s"expected Double, got ${other.getClass.getSimpleName}")
  }

  /** Helper to extract String from Value */
  private def s(v: Value): String = v match {
    case Str(str) => str
    case other =>
      sys.error(s"expected String, got ${other.getClass.getSimpleName}")
  }

  /** Helper to extract List[CanvasCommand] from Value */
  private def cmdList(v: Value): List[CanvasCommand] = {
    VList.unapply(v) match {
      case Some(items) =>
        items.flatMap { item =>
          item.asExternal.toAny match {
            case cmd: CanvasCommand => Some(cmd)
            case _ => None
          }
        }
      case None => Nil
    }
  }

  /** JVM externals for Canvas operations */
  val jvmExternals: Externals =
    Externals.empty
      // Drawing commands
      .add(packageName, "circle", FfiCall.Fn3 { (x, y, radius) =>
        ExternalValue(Circle(d(x), d(y), d(radius)))
      })
      .add(packageName, "rect", FfiCall.Fn4 { (x, y, w, h) =>
        ExternalValue(Rect(d(x), d(y), d(w), d(h)))
      })
      .add(packageName, "line", FfiCall.Fn4 { (x1, y1, x2, y2) =>
        ExternalValue(Line(d(x1), d(y1), d(x2), d(y2)))
      })
      .add(packageName, "text_cmd", FfiCall.Fn3 { (content, x, y) =>
        ExternalValue(TextCmd(s(content), d(x), d(y)))
      })
      .add(packageName, "arc", FfiCall.Fn5 { (x, y, radius, startAngle, endAngle) =>
        ExternalValue(Arc(d(x), d(y), d(radius), d(startAngle), d(endAngle)))
      })
      // Style commands
      .add(packageName, "fill", FfiCall.Fn1 { color =>
        ExternalValue(Fill(s(color)))
      })
      .add(packageName, "stroke", FfiCall.Fn1 { color =>
        ExternalValue(Stroke(s(color)))
      })
      .add(packageName, "line_width", FfiCall.Fn1 { width =>
        ExternalValue(LineWidth(d(width)))
      })
      // Canvas operations
      .add(packageName, "clear", FfiCall.Fn1 { color =>
        ExternalValue(Clear(s(color)))
      })
      .add(packageName, "sequence", FfiCall.Fn1 { commands =>
        ExternalValue(Sequence(cmdList(commands)))
      })
      // Transform commands
      .add(packageName, "save", FfiCall.Fn1 { _ => ExternalValue(Save) })
      .add(packageName, "restore", FfiCall.Fn1 { _ => ExternalValue(Restore) })
      .add(packageName, "translate", FfiCall.Fn2 { (x, y) =>
        ExternalValue(Translate(d(x), d(y)))
      })
      .add(packageName, "rotate", FfiCall.Fn1 { angle =>
        ExternalValue(Rotate(d(angle)))
      })
      .add(packageName, "scale", FfiCall.Fn2 { (sx, sy) =>
        ExternalValue(Scale(d(sx), d(sy)))
      })
      // Canvas binding - returns a prop tuple for the canvas element
      // The actual binding happens at compile time via UIAnalyzer
      .add(packageName, "canvas_render", FfiCall.Fn2 { (state, renderFn) =>
        // Return a prop tuple that marks this canvas as state-bound
        val bindingId = s"canvas_render_${System.identityHashCode(state)}"
        ProductValue(Array(Str("data-canvas-render"), Str(bindingId)))
      })
}
