package dev.bosatsu.jsui

import scala.concurrent.duration.Duration
import io.circe.{Json, Encoder, Decoder, DecodingFailure}
import io.circe.syntax._
import io.circe.parser.decode

import cats.syntax.all._

sealed trait State derives CanEqual

object State {
  sealed trait HasText extends State {
    def editorText: String
  }
  case object Init extends State
  case class WithText(
      editorText: String
  ) extends HasText

  case class Compiling(previousState: HasText) extends State

  case class Compiled(
      editorText: String,
      output: String,
      compilationTime: Duration
  ) extends HasText

  def init: State = Init

  // Custom encoder for Duration to handle it as milliseconds
  implicit val encodeDuration: Encoder[Duration] =
    Encoder.instance(duration => Json.fromLong(duration.toNanos))

  // Custom decoder for Duration from milliseconds
  implicit val decodeDuration: Decoder[Duration] =
    Decoder.instance(cursor => cursor.as[Long].map(Duration.fromNanos(_)))
  // Encoder for the State trait
  implicit val encodeState: Encoder[State] = Encoder.instance {
    case Init                 => Json.obj("type" -> Json.fromString("Init"))
    case wt: WithText         => wt.asJson(using encodeWithText)
    case compiling: Compiling => compiling.asJson(using encodeCompiling)
    case compiled: Compiled   => compiled.asJson(using encodeCompiled)
  }

  // Decoders for HasText and its subtypes
  implicit val decodeHasText: Decoder[HasText] = {
    val decodeWithText: Decoder[WithText] = Decoder.instance { cursor =>
      cursor.downField("editorText").as[String].map(WithText(_))
    }
    val decodeCompiled: Decoder[Compiled] = Decoder.instance { cursor =>
      (
        cursor.downField("editorText").as[String],
        cursor.downField("output").as[String],
        cursor.downField("compilationTime").as[Duration]
      ).mapN(Compiled(_, _, _))
    }
    Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "WithText" => cursor.as(using decodeWithText)
        case "Compiled" => cursor.as(using decodeCompiled)
        case unexpected =>
          val msg =
            s"expected \"WithText\" or \"Compiled\", got: \"$unexpected\""
          Left(DecodingFailure(msg, cursor.history))
      }
    }
  }

  // Decoders for the State trait and its implementations
  implicit val decodeState: Decoder[State] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Init"      => Right(Init)
      case "Compiling" =>
        cursor.downField("previousState").as[HasText].map(Compiling(_))
      case _ => decodeHasText(cursor)
    }
  }

  // Manual encoders for distinguishing types
  implicit val encodeWithText: Encoder[WithText] =
    Encoder.forProduct2("type", "editorText") { wt =>
      ("WithText", wt.editorText)
    }

  implicit val encodeCompiled: Encoder[Compiled] =
    Encoder.forProduct4("type", "editorText", "output", "compilationTime") {
      compiled =>
        (
          "Compiled",
          compiled.editorText,
          compiled.output,
          compiled.compilationTime
        )
    }

  implicit val encodeCompiling: Encoder[Compiling] =
    Encoder.forProduct2("type", "previousState")(compiling =>
      (
        "Compiling",
        compiling.previousState match {
          case comp @ Compiled(_, _, _) => encodeCompiled(comp)
          case wt @ WithText(_)         => encodeWithText(wt)
        }
      )
    )

  def stateToJsonString(s: State): String = s.asJson.spaces2SortKeys
  def stringToState(str: String): Either[Throwable, State] =
    decode[State](str)
}
