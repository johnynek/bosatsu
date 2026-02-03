package dev.bosatsu.tla

import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import dev.bosatsu.tla.TlaValue._

/**
 * JSON encoders and decoders for TLA+ types.
 */
object TlaJson {

  // TlaValue encoders - need inner function to handle recursive types
  given tlaValueEncoder: Encoder[TlaValue] = Encoder.instance { v =>
    def encode(value: TlaValue): Json = value match {
      case TlaInt(i) => Json.obj("type" -> "int".asJson, "value" -> i.asJson)
      case TlaString(s) => Json.obj("type" -> "string".asJson, "value" -> s.asJson)
      case TlaBool(b) => Json.obj("type" -> "bool".asJson, "value" -> b.asJson)
      case TlaSeq(vs) => Json.obj("type" -> "seq".asJson, "values" -> Json.arr(vs.map(encode)*))
      case TlaSet(vs) => Json.obj("type" -> "set".asJson, "values" -> Json.arr(vs.toList.map(encode)*))
      case TlaRecord(fs) => Json.obj("type" -> "record".asJson, "fields" -> Json.obj(fs.view.mapValues(encode).toSeq*))
      case TlaFunction(d, m) => Json.obj("type" -> "function".asJson, "domain" -> d.asJson, "mapping" -> m.asJson)
    }
    encode(v)
  }

  given tlaValueDecoder: Decoder[TlaValue] = Decoder.instance { c =>
    def decode(cursor: HCursor): Decoder.Result[TlaValue] =
      cursor.downField("type").as[String].flatMap {
        case "int" => cursor.downField("value").as[Int].map(TlaInt.apply)
        case "string" => cursor.downField("value").as[String].map(TlaString.apply)
        case "bool" => cursor.downField("value").as[Boolean].map(TlaBool.apply)
        case "seq" =>
          cursor.downField("values").as[List[Json]].flatMap { jsons =>
            jsons.traverse(j => decode(j.hcursor)).map(TlaSeq.apply)
          }
        case "set" =>
          cursor.downField("values").as[List[Json]].flatMap { jsons =>
            jsons.traverse(j => decode(j.hcursor)).map(vs => TlaSet(vs.toSet))
          }
        case "record" =>
          cursor.downField("fields").as[Map[String, Json]].flatMap { fields =>
            fields.toList.traverse { case (k, v) =>
              decode(v.hcursor).map(k -> _)
            }.map(pairs => TlaRecord(pairs.toMap))
          }
        case "function" => for {
          d <- cursor.downField("domain").as[String]
          m <- cursor.downField("mapping").as[String]
        } yield TlaFunction(d, m)
        case other => Left(DecodingFailure(s"Unknown TlaValue type: $other", cursor.history))
      }
    decode(c)
  }

  // TlaOptions
  given Encoder[TlaOptions] = Encoder.instance { o =>
    Json.obj(
      "initialState" -> o.initialState.asJson,
      "invariant" -> o.invariant.asJson,
      "stateVariable" -> o.stateVariable.asJson,
      "checkDeadlock" -> o.checkDeadlock.asJson
    )
  }

  given Decoder[TlaOptions] = Decoder.instance { c =>
    for {
      initial <- c.downField("initialState").as[Map[String, TlaValue]]
      invariant <- c.downField("invariant").as[Option[String]]
      stateVar <- c.downField("stateVariable").as[Option[String]]
      checkDeadlock <- c.downField("checkDeadlock").as[Option[Boolean]]
    } yield TlaOptions(
      initialState = initial,
      invariant = invariant,
      stateVariable = stateVar.getOrElse("state"),
      checkDeadlock = checkDeadlock.getOrElse(true)
    )
  }

  // TlaAction
  given Encoder[TlaAction] = Encoder.instance { a =>
    Json.obj(
      "name" -> a.name.asJson,
      "guard" -> a.guard.asJson,
      "effect" -> a.effect.asJson,
      "pcFrom" -> a.pcFrom.asJson,
      "pcTo" -> a.pcTo.asJson
    )
  }

  given Decoder[TlaAction] = Decoder.instance { c =>
    for {
      name <- c.downField("name").as[String]
      guard <- c.downField("guard").as[String]
      effect <- c.downField("effect").as[String]
      pcFrom <- c.downField("pcFrom").as[String]
      pcTo <- c.downField("pcTo").as[String]
    } yield TlaAction(name, guard, effect, pcFrom, pcTo)
  }

  // TlaSpec
  given Encoder[TlaSpec] = Encoder.instance { s =>
    Json.obj(
      "moduleName" -> s.moduleName.asJson,
      "extends" -> s.extends_.asJson,
      "variables" -> s.variables.asJson,
      "init" -> s.init.asJson,
      "actions" -> s.actions.asJson,
      "next" -> s.next.asJson,
      "spec" -> s.spec.asJson,
      "invariants" -> s.invariants.asJson
    )
  }

  // TlcTraceState - must be defined before TlcResult
  given tlcTraceStateEncoder: Encoder[TlcTraceState] = Encoder.instance { s =>
    Json.obj(
      "stateNumber" -> s.stateNumber.asJson,
      "action" -> s.action.asJson,
      "variables" -> s.variables.asJson
    )
  }

  given tlcTraceStateDecoder: Decoder[TlcTraceState] = Decoder.instance { c =>
    for {
      stateNum <- c.downField("stateNumber").as[Int]
      action <- c.downField("action").as[String]
      variables <- c.downField("variables").as[String]
    } yield TlcTraceState(stateNum, action, variables)
  }

  // TlcResult - uses TlcTraceState
  given Encoder[TlcResult] = Encoder.instance { r =>
    Json.obj(
      "success" -> r.success.asJson,
      "invariantViolation" -> r.invariantViolation.asJson,
      "deadlock" -> r.deadlock.asJson,
      "temporalPropertyViolation" -> r.temporalPropertyViolation.asJson,
      "syntaxError" -> r.syntaxError.asJson,
      "errorMessage" -> r.errorMessage.asJson,
      "statesGenerated" -> r.statesGenerated.asJson,
      "distinctStates" -> r.distinctStates.asJson,
      "errorTrace" -> r.errorTrace.asJson,
      "skipped" -> r.skipped.asJson,
      "skipReason" -> r.skipReason.asJson
    )
  }

  given Decoder[TlcResult] = Decoder.instance { c =>
    for {
      success <- c.downField("success").as[Boolean]
      invViolation <- c.downField("invariantViolation").as[Option[Boolean]]
      deadlock <- c.downField("deadlock").as[Option[Boolean]]
      tempViolation <- c.downField("temporalPropertyViolation").as[Option[Boolean]]
      syntaxError <- c.downField("syntaxError").as[Option[Boolean]]
      errorMsg <- c.downField("errorMessage").as[Option[String]]
      statesGen <- c.downField("statesGenerated").as[Option[Int]]
      distinctSt <- c.downField("distinctStates").as[Option[Int]]
      errorTrace <- c.downField("errorTrace").as[Option[List[TlcTraceState]]]
      skipped <- c.downField("skipped").as[Option[Boolean]]
      skipReason <- c.downField("skipReason").as[Option[String]]
    } yield TlcResult(
      success = success,
      invariantViolation = invViolation.getOrElse(false),
      deadlock = deadlock.getOrElse(false),
      temporalPropertyViolation = tempViolation.getOrElse(false),
      syntaxError = syntaxError.getOrElse(false),
      errorMessage = errorMsg,
      statesGenerated = statesGen,
      distinctStates = distinctSt,
      errorTrace = errorTrace.getOrElse(Nil),
      skipped = skipped.getOrElse(false),
      skipReason = skipReason
    )
  }

  // TlcOptions
  given Encoder[TlcOptions] = Encoder.instance { o =>
    Json.obj(
      "workers" -> o.workers.asJson,
      "checkDeadlock" -> o.checkDeadlock.asJson,
      "depth" -> o.depth.asJson,
      "timeout" -> o.timeout.asJson,
      "skipIfUnavailable" -> o.skipIfUnavailable.asJson
    )
  }

  given Decoder[TlcOptions] = Decoder.instance { c =>
    for {
      workers <- c.downField("workers").as[Option[Int]]
      checkDeadlock <- c.downField("checkDeadlock").as[Option[Boolean]]
      depth <- c.downField("depth").as[Option[Int]]
      timeout <- c.downField("timeout").as[Option[Int]]
      skipIfUnavailable <- c.downField("skipIfUnavailable").as[Option[Boolean]]
    } yield TlcOptions(
      workers = workers.getOrElse(1),
      checkDeadlock = checkDeadlock.getOrElse(true),
      depth = depth,
      timeout = timeout,
      skipIfUnavailable = skipIfUnavailable.getOrElse(true)
    )
  }

  // RaceViolation - must be defined before RaceAnalysisResult
  given raceViolationEncoder: Encoder[RaceViolation] = Encoder.instance { v =>
    Json.obj(
      "trace" -> v.trace.asJson,
      "finalState" -> v.finalState.asJson
    )
  }

  // TlaGenResult
  given Encoder[TlaGenResult] = Encoder.instance { r =>
    Json.obj(
      "file" -> r.file.asJson,
      "tlaSpec" -> r.tlaSpec.asJson,
      "moduleName" -> r.moduleName.asJson,
      "instances" -> r.instances.asJson,
      "invariant" -> r.invariant.asJson,
      "tlcResult" -> r.tlcResult.asJson
    )
  }

  // RaceAnalysisResult - uses RaceViolation
  given Encoder[RaceAnalysisResult] = Encoder.instance { r =>
    Json.obj(
      "file" -> r.file.asJson,
      "handlerName" -> r.handlerName.asJson,
      "instances" -> r.instances.asJson,
      "totalInterleavings" -> r.totalInterleavings.asJson,
      "violatingInterleavings" -> r.violatingInterleavings.asJson,
      "invariant" -> r.invariant.asJson,
      "exampleViolation" -> r.exampleViolation.asJson
    )
  }
}
