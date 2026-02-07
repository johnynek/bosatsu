package dev.bosatsu

import cats.data.{Validated, ValidatedNel}
import com.monovore.decline.{Argument, Opts}
import org.typelevel.paiges.Doc
import cats.parse.{LocationMap => CPLocationMap}

import cats.implicits._

import LocationMap.Colorize

/** Build a cache of the rows and columns in a given string. This is for showing
  * error messages to users
  */
case class LocationMap(fromString: String) extends CPLocationMap(fromString) {

  private def lineRange(start: Int, end: Int): List[(Int, String)] =
    (start to end).iterator
      .filter(_ >= 0)
      .map { r =>
        val liner = getLine(r).get // should never throw
        // lines are usually 1 offset labeled
        (r + 1, liner)
      }
      .toList

  /** convert tab to tab, but otherwise space return the white space before this
    * column
    */
  private def spaceOf(row: Int, col: Int): Option[String] =
    getLine(row)
      .map { line =>
        val bldr = new java.lang.StringBuilder
        var idx = 0
        while (idx < col) {
          val c = if (line.length > idx) line.charAt(idx) else ' '
          if (c == '\t') bldr.append('\t')
          else bldr.append(' ')
          idx = idx + 1
        }
        bldr.toString()
      }

  def showContext(
      offset: Int,
      previousLines: Int,
      color: Colorize
  ): Option[Doc] =
    toLineCol(offset)
      .map { case (r, c) =>
        val lines = lineRange(r - previousLines, r)

        val toLineStr = lines match {
          case Nil      => { (i: Int) => Doc.str(i) }
          case notEmpty =>
            val maxLine = notEmpty.iterator.map(_._1).max
            LocationMap.lineNumberToString(maxLine)
        }

        // here is how much extra we need for the pointer
        val pointerPad = Doc.spaces(toLineStr(r).render(0).length)
        val lineDocs = lines.map { case (no, l) => toLineStr(no) + Doc.text(l) }
        val ctx = Doc.intercalate(Doc.hardLine, lineDocs)
        // convert to spaces
        val colPad = spaceOf(r, c).get
        ctx + Doc.hardLine + pointerPad + LocationMap.pointerTo(
          colPad,
          color
        ) + Doc.hardLine
      }

  def showRegion(
      region: Region,
      previousLines: Int,
      color: Colorize
  ): Option[Doc] =
    (toLineCol(region.start), toLineCol(region.end - 1))
      .mapN { case ((l0, c0), (l1, c1)) =>
        val lines = lineRange(l0 - previousLines, l1)
        val toLineStr = lines match {
          case Nil      => { (i: Int) => Doc.str(i) }
          case notEmpty =>
            val maxLine = notEmpty.iterator.map(_._1).max
            LocationMap.lineNumberToString(maxLine)
        }

        if (l0 == l1) {
          // same line
          // here is how much extra we need for the pointer
          val pointerPad = Doc.spaces(toLineStr(l0).render(0).length)
          val lineDocs = lines.map { case (no, l) =>
            toLineStr(no) + Doc.text(l)
          }
          val ctx = Doc.intercalate(Doc.hardLine, lineDocs)
          val c0Pad = spaceOf(l0, c0).get
          // we go one more to cover the column
          val c1Pad = spaceOf(l0, c1 + 1).get
          ctx + Doc.hardLine + pointerPad + LocationMap.pointerRange(
            c0Pad,
            c1Pad,
            color
          ) + Doc.hardLine
        } else {
          // we span multiple lines, show the start and the end:
          val newPrev = l1 - l0
          showContext(region.start, previousLines, color).get +
            Doc.hardLine + Doc.text("to:") + Doc.hardLine +
            showContext(region.end - 1, newPrev, color).get
        }
      }

}

object LocationMap {
  sealed trait Colorize {
    def red(d: Doc): Doc
    def green(d: Doc): Doc
  }

  object Colorize {
    object None extends Colorize {
      def red(d: Doc) = d
      def green(d: Doc) = d
    }

    object Console extends Colorize {
      def red(d: Doc) =
        Doc.zeroWidth(scala.Console.RED) + d.unzero + Doc.zeroWidth(
          scala.Console.RESET
        )

      def green(d: Doc) =
        Doc.zeroWidth(scala.Console.GREEN) + d.unzero + Doc.zeroWidth(
          scala.Console.RESET
        )
    }

    object HmtlFont extends Colorize {
      def red(d: Doc) =
        Doc.zeroWidth("<font color=\"red\">") + d.unzero + Doc.zeroWidth(
          "</font>"
        )

      def green(d: Doc) =
        Doc.zeroWidth("<font color=\"green\">") + d.unzero + Doc.zeroWidth(
          "</font>"
        )
    }

    implicit val argColor: Argument[Colorize] =
      new Argument[Colorize] {
        def defaultMetavar: String = "color"
        def read(str: String): ValidatedNel[String, Colorize] =
          str.toLowerCase match {
            case "none" => Validated.valid(Colorize.None)
            case "ansi" => Validated.valid(Colorize.Console)
            case "html" => Validated.valid(Colorize.HmtlFont)
            case other  =>
              Validated.invalidNel(
                s"unknown colorize: $other, expected: none, ansi or html"
              )
          }
      }

    val opts: Opts[Colorize] =
      Opts.option[Colorize]("color", help = "colorize mode: none, ansi or html")

    val optsConsoleDefault: Opts[Colorize] = opts.orElse(Opts(Colorize.Console))
  }

  /** Provide a string that points with a carat to a given column with 0 based
    * indexing: e.g. pointerTo(2) == " ^"
    */
  def pointerTo(colStr: String, color: Colorize): Doc = {
    val col = Doc.text(colStr)
    val pointer = Doc.char('^')
    col + color.red(pointer)
  }

  def pointerRange(startPad: String, endPad: String, color: Colorize): Doc = {
    val col = Doc.text(startPad)
    // just use tab for any tabs
    val pointerStr = endPad.drop(startPad.length).map {
      case '\t' => '\t'
      case _    => '^'
    }
    val pointer = Doc.text(pointerStr)
    col + color.red(pointer)
  }

  def charsLineNumber(i: Int): Int = {
    Require(i >= 0, s"expect line > 0, found $i")
    def go(i: Int, acc: Int): Int =
      if (i < 10) acc
      else go(i / 10, acc + 1)

    go(i, 1)
  }

  def lineNumberToString(maxLine: Int): Int => Doc = {
    val padding = LocationMap.charsLineNumber(maxLine)

    { (i: Int) =>
      val istr = i.toString
      val pad = padding - istr.length
      Doc.spaces(pad) + (Doc.text(istr) + Doc.char('|'))
    }
  }
}
