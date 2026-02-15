package dev.bosatsu.tool_command

import cats.Traverse
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{Json, JsonEncodingError, LocationMap, PlatformIO, rankn}
import dev.bosatsu.tool.{CliException, CommonOpts, Output}
import org.typelevel.paiges.Doc

object JsonCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    enum JsonInput {
      case FromString(asString: String)
      case FromPath(path: Path)

      def read: F[String] =
        this match {
          case JsonInput.FromString(asString) => moduleIOMonad.pure(asString)
          case JsonInput.FromPath(path)       => platformIO.readUtf8(path)
        }
    }

    enum JsonMode derives CanEqual {
      case Write
      case Apply(in: JsonInput)
      case Traverse(in: JsonInput)
    }

    def ioJson(io: F[String]): F[Json] =
      io.flatMap { jsonString =>
        Json.parserFile.parseAll(jsonString) match {
          case Right(j)  => moduleIOMonad.pure(j)
          case Left(err) =>
            val idx = err.failedAtOffset
            val errMsg0 = jsonString.substring(idx + 1)
            val errMsg =
              if (errMsg0.length > 20)
                errMsg0.take(20) + show"... (and ${errMsg0.length - 20} more)"
              else errMsg0

            moduleIOMonad.raiseError(
              CliException.Basic(
                show"could not parse a JSON record at ${idx + 1}: $errMsg"
              )
            )
        }
      }

    def toJsonOpt(modeOpt: Opts[JsonMode]): Opts[F[Output[Path]]] =
      (
        commonOpts.sourcePathOpts,
        commonOpts.includePathOpts,
        commonOpts.packageResolverOpts,
        commonOpts.publicDependencyOpts,
        commonOpts.privateDependencyOpts,
        modeOpt,
        commonOpts.mainIdentifierOpt,
        commonOpts.outputPathOpt.orNone,
        Colorize.optsConsoleDefault
      ).mapN {
        (
            srcs,
            includes,
            packageResolver,
            publicDependencies,
            privateDependencies,
            mode,
            mainPackage,
            outputOpt,
            errColor
        ) =>
          platformIO.withEC {
            EvalCommand
              .runEval(
                platformIO,
                srcs,
                includes,
                packageResolver,
                publicDependencies,
                privateDependencies,
                "json",
                mainPackage,
                errColor
              )
              .flatMap { case (ev, res) =>
                val valueToJson = ev.valueToJson

                def unsupported[A](
                    j: JsonEncodingError.UnsupportedType
                ): F[A] = {
                  def typeDoc(t: rankn.Type) =
                    rankn.Type.fullyResolvedDocument.document(t)

                  val path = j.path.init
                  val badType = j.path.last
                  val pathMsg = path match {
                    case Nil  => Doc.empty
                    case nonE =>
                      val sep =
                        Doc.lineOrSpace + Doc.text("contains") + Doc.lineOrSpace
                      val pd =
                        (Doc.intercalate(
                          sep,
                          nonE.map(typeDoc(_))
                        ) + sep + typeDoc(
                          badType
                        )).nested(4)
                      pd + Doc.hardLine + Doc.hardLine + Doc.text("but") +
                        Doc.hardLine +
                        Doc.hardLine
                  }
                  val msg =
                    pathMsg + Doc.text("the type") + Doc.space + typeDoc(
                      badType
                    ) + Doc.space + Doc.text("isn't supported")
                  val tpeStr = msg.render(80)

                  moduleIOMonad.raiseError(
                    CliException
                      .Basic(show"cannot convert type to Json: $tpeStr")
                  )
                }

                def process[G[_]: Traverse](
                    io: F[String],
                    extract: Json => F[G[Json]],
                    inject: G[Json] => Json
                ): F[Output[Path]] =
                  valueToJson.valueFnToJsonFn(res.tpe) match {
                    case Left(unsup)           => unsupported(unsup)
                    case Right((arity, fnGen)) =>
                      fnGen(res.value.value) match {
                        case Right(fn) =>
                          ioJson(io)
                            .flatMap(extract)
                            .flatMap {
                              _.traverse {
                                case ary @ Json.JArray(items)
                                    if items.length == arity =>
                                  fn(ary) match {
                                    case Left(dataError) =>
                                      moduleIOMonad.raiseError[Json](
                                        CliException.Basic(
                                          show"invalid input json: $dataError"
                                        )
                                      )
                                    case Right(json) =>
                                      moduleIOMonad.pure(json)
                                  }
                                case otherJson =>
                                  moduleIOMonad.raiseError[Json](
                                    CliException.Basic(
                                      show"required a json array of size $arity, found:\n\n${otherJson.render}"
                                    )
                                  )
                              }
                            }
                            .map { fjson =>
                              Output.JsonOutput(inject(fjson), outputOpt)
                            }
                        // $COVERAGE-OFF$ defensive fallback for ill-typed runtime values
                        case Left(valueError) =>
                          moduleIOMonad.raiseError(
                            CliException.Basic(
                              show"unexpected value error: $valueError"
                            )
                          )
                        // $COVERAGE-ON$
                      }
                  }

                mode match {
                  case JsonMode.Write =>
                    valueToJson.toJson(res.tpe) match {
                      case Left(unsup) => unsupported(unsup)
                      case Right(fn)   =>
                        fn(res.value.value) match {
                          // $COVERAGE-OFF$ defensive fallback for ill-typed runtime values
                          case Left(valueError) =>
                            moduleIOMonad.raiseError(
                              CliException.Basic(
                                show"unexpected value error: $valueError"
                              )
                            )
                          // $COVERAGE-ON$
                          case Right(j) =>
                            moduleIOMonad.pure(Output.JsonOutput(j, outputOpt))
                        }
                    }

                  case JsonMode.Apply(in) =>
                    process[cats.Id](
                      in.read,
                      json => moduleIOMonad.pure(json),
                      json => json
                    )
                  case JsonMode.Traverse(in) =>
                    process[Vector](
                      in.read,
                      {
                        case Json.JArray(items) => moduleIOMonad.pure(items)
                        case other              =>
                          moduleIOMonad.raiseError(
                            CliException.Basic(
                              show"require an array or arrays for traverse, found: ${other.getClass.getName}"
                            )
                          )
                      },
                      items => Json.JArray(items)
                    )
                }
              }
          }
      }

    import platformIO.pathArg

    val input: Opts[JsonInput] =
      Opts
        .option[Path]("json_input", help = "json input path")
        .map(JsonInput.FromPath(_))
        .orElse(
          Opts
            .option[String]("json_string", help = "json string argument")
            .map(JsonInput.FromString(_))
        )

    val applyInput = input.map(JsonMode.Apply(_))
    val traverseInput = input.map(JsonMode.Traverse(_))

    val subcommands = Opts
      .subcommand("write", "write a bosatsu expression into json")(
        toJsonOpt(Opts(JsonMode.Write))
      )
      .orElse(
        Opts.subcommand(
          "apply",
          "apply a bosatsu function to a json array argument list"
        )(toJsonOpt(applyInput))
      )
      .orElse(
        Opts.subcommand(
          "traverse",
          "apply a bosatsu function to each element of an array or each value in an object"
        )(toJsonOpt(traverseInput))
      )

    Opts.subcommand("json", "json writing and transformation tools")(
      subcommands
    )
  }
}
