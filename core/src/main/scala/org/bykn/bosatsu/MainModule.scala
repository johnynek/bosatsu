package org.bykn.bosatsu

import cats.data.{Chain, Validated, ValidatedNel, NonEmptyList}
import cats.{Eval, MonadError, Traverse}
import com.monovore.decline.{Argument, Command, Help, Opts}
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

import CollectionUtils.listToUnique
import Identifier.Bindable
import IorMethods.IorExtension
import LocationMap.Colorize

import cats.implicits._

/** This is an implementation of the CLI tool where Path is abstracted. The idea
  * is to allow it to be testable and usable in scalajs where we don't have
  * file-IO
  */
abstract class MainModule[IO[_]](implicit
    val moduleIOMonad: MonadError[IO, Throwable]
) {
  type Path

  implicit def pathArg: Argument[Path]

  def readPath(p: Path): IO[String]

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]]

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]]

  /** given an ordered list of prefered roots, if a packFile starts with one of
    * these roots, return a PackageName based on the rest
    */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName]

  /** Modules optionally have the capability to combine paths into a tree
    */
  def resolvePath: Option[(Path, PackageName) => IO[Option[Path]]]

  /** some modules have paths that form directory trees
    *
    * if the given path is a directory, return Some and all the first children.
    */
  def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]]

  def hasExtension(str: String): Path => Boolean

  // we can do side effects in here
  def delay[A](a: => A): IO[A]

  //////////////////////////////
  // Below here are concrete and should not use override
  //////////////////////////////

  final def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    delay(Par.newService())
      .flatMap { es =>
        fn(Par.ecFromService(es))
          .flatMap { a =>
            delay {
              Par.shutdownService(es)
              a
            }
          }
          .recoverWith { case e =>
            delay {
              Par.shutdownService(es)
            }.flatMap(_ => moduleIOMonad.raiseError[A](e))
          }
      }

  final def run(args: List[String]): Either[Help, IO[Output]] =
    MainCommand.command
      .parse(args.toList)
      .map(_.run.widen)

  sealed abstract class FileKind(val name: String)
  object FileKind {
    case object Source extends FileKind("source")
    case object Iface extends FileKind("interface")
    case object Pack extends FileKind("package")
  }

  sealed abstract class GraphOutput
  object GraphOutput {
    case object Dot extends GraphOutput
    case object Json extends GraphOutput

    val jsonOrDot: Opts[GraphOutput] =
      Opts
        .option[String]("graph_format", "format of graph, either json or dot")
        .mapValidated {
          case "json" => Validated.valid(Json)
          case "dot"  => Validated.valid(Dot)
          case other =>
            Validated.invalidNel(s"\"$other\" invalid, expected json or dot")
        }
        .withDefault(Json)
  }

  sealed abstract class Output
  object Output {
    case class TestOutput(
        tests: List[(PackageName, Option[Eval[Test]])],
        colorize: Colorize
    ) extends Output
    case class EvaluationResult(
        value: Eval[Value],
        tpe: rankn.Type,
        doc: Eval[Doc]
    ) extends Output
    case class JsonOutput(json: Json, output: Option[Path]) extends Output
    case class CompileOut(
        packList: List[Package.Typed[Any]],
        ifout: Option[Path],
        output: Option[Path]
    ) extends Output
    case class TranspileOut(outs: List[(NonEmptyList[String], Doc)], base: Path)
        extends Output

    case class ShowOutput(
        packages: List[Package.Typed[Any]],
        ifaces: List[Package.Interface],
        output: Option[Path]
    ) extends Output

    case class DepsOutput(
        depinfo: List[(Path, PackageName, FileKind, List[PackageName])],
        output: Option[Path],
        style: GraphOutput
    ) extends Output
  }

  sealed abstract class MainException extends Exception {
    def command: MainCommand
  }
  object MainException {
    case class NoInputs(command: MainCommand) extends MainException
    case class ParseErrors(
        command: MainCommand,
        errors: NonEmptyList[MainCommand.ParseError],
        color: Colorize
    ) extends MainException {

      def messages: List[String] =
        errors.toList.flatMap {
          case MainCommand.ParseError.ParseFailure(pf, path) =>
            // we should never be partial here
            val (r, c) = pf.locations.toLineCol(pf.position).get
            val ctx = pf.showContext(color)
            List(
              s"failed to parse $path:${r + 1}:${c + 1}",
              ctx.render(80)
            )
          case MainCommand.ParseError.FileError(path, err) =>
            err match {
              case e
                  if e.getClass.getName == "java.nio.file.NoSuchFileException" =>
                // This class isn't present in scalajs, use the String
                List(s"file not found: $path")
              case _ =>
                List(
                  s"failed to parse $path",
                  err.getMessage,
                  err.getClass.toString
                )
            }
        }
    }
    case class PackageErrors(
        command: MainCommand,
        sourceMap: PackageMap.SourceMap,
        errors: NonEmptyList[PackageError],
        color: Colorize
    ) extends MainException {
      def messages: List[String] =
        errors.toList.distinct
          .map(_.message(sourceMap, color))
    }
  }

  def mainExceptionToString(ex: Throwable): Option[String] =
    ex match {
      case me: MainException =>
        me match {
          case MainException.NoInputs(cmd) =>
            val name = cmd.name
            Some(s"no inputs given to $name")
          case pe @ MainException.ParseErrors(_, _, _) =>
            Some(pe.messages.mkString("\n"))
          case pe @ MainException.PackageErrors(_, _, _, _) =>
            Some(pe.messages.mkString("\n"))
        }
      case _ =>
        None
    }

  sealed abstract class MainCommand(val name: String) {
    type Result <: Output
    def run: IO[Result]
  }

  object MainCommand {
    def parseInputs[F[_]: Traverse](
        paths: F[Path],
        packRes: PackageResolver
    ): IO[ValidatedNel[ParseError, F[((Path, LocationMap), Package.Parsed)]]] =
      // we use IO(traverse) so we can accumulate all the errors in parallel easily
      // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
      // and use the composed applicative... too much work for the same result
      paths
        .traverse { path =>
          val defaultPack = packRes.packageNameFor(path)
          parseFile(Package.parser(defaultPack), path)
            .map(_.map { case (lm, parsed) =>
              ((path, lm), parsed)
            })
        }
        .map(_.sequence)

    def parseHeaders[F[_]: Traverse](
        paths: F[Path],
        packRes: PackageResolver
    ): IO[ValidatedNel[ParseError, F[(Path, Package.Header)]]] =
      // we use IO(traverse) so we can accumulate all the errors in parallel easily
      // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
      // and use the composed applicative... too much work for the same result
      paths
        .traverse { path =>
          val defaultPack = packRes.packageNameFor(path)
          readPath(path).map { str =>
            parseStart(Package.headerParser(defaultPack), path, str)
              .map { case (_, pp) => (path, pp) }
          }
        }
        .map(_.sequence)

    private def flatTrav[A, B, C](va: Validated[A, B])(
        fn: B => IO[Validated[A, C]]
    ): IO[Validated[A, C]] =
      va.traverse(fn).map(_.andThen(identity _))

    /** This parses all the given paths and returns them first, and if the
      * PackageResolver supports it, we look for any missing dependencies that
      * are not already included
      */
    def parseAllInputs(
        paths: List[Path],
        included: Set[PackageName],
        packRes: PackageResolver
    ): IO[
      ValidatedNel[ParseError, List[((Path, LocationMap), Package.Parsed)]]
    ] =
      parseInputs(paths, packRes)
        .flatMap {
          flatTrav(_) { parsed =>
            val done = included ++ parsed.toList.map(_._2.name)
            val allImports = parsed.toList.flatMap(_._2.imports.map(_.pack))
            val missing: List[PackageName] = allImports.filterNot(done)
            parseTransitivePacks(missing, packRes, done)
              .map(_.map { case (searched, _) => parsed ::: searched.toList })
          }
        }

    type ParseTransResult = ValidatedNel[
      ParseError,
      (Chain[((Path, LocationMap), Package.Parsed)], Set[PackageName])
    ]

    private def parseTransitive(
        search: PackageName,
        packRes: PackageResolver,
        done: Set[PackageName]
    ): IO[ParseTransResult] = {

      val maybeReadPack: IO[Option[(Path, String)]] =
        if (done(search)) {
          moduleIOMonad.pure(Option.empty[(Path, String)])
        } else {
          packRes
            .pathFor(search)
            .flatMap(_.traverse { path =>
              readPath(path).map((path, _))
            })
        }

      val optParsed: IO[
        ValidatedNel[ParseError, Option[((Path, LocationMap), Package.Parsed)]]
      ] =
        maybeReadPack.map { opt =>
          opt.traverse { case (path, str) =>
            val defaultPack = packRes.packageNameFor(path)
            parseString(Package.parser(defaultPack), path, str)
              .map { case (lm, parsed) =>
                ((path, lm), parsed)
              }
          }
        }

      def imports(p: Package.Parsed): List[PackageName] =
        p.imports.map(_.pack)

      val newDone = done + search

      optParsed.flatMap {
        flatTrav(_) {
          case None =>
            moduleIOMonad.pure(
              Validated.valid(
                (Chain.empty[((Path, LocationMap), Package.Parsed)], newDone)
              ): ParseTransResult
            )
          case Some(item @ (_, pack)) =>
            val imps = imports(pack).filterNot(done)
            parseTransitivePacks(imps, packRes, newDone)
              .map(_.map { case (newPacks, newDone) =>
                (item +: newPacks, newDone)
              })
        }
      }
    }

    private def parseTransitivePacks(
        search: List[PackageName],
        packRes: PackageResolver,
        done: Set[PackageName]
    ): IO[ParseTransResult] =
      search.foldM(Validated.valid((Chain.empty, done)): ParseTransResult) {
        (prev, impPack) =>
          flatTrav(prev) { case (acc, prevDone) =>
            parseTransitive(impPack, packRes, prevDone)
              .map(_.map { case (newPacks, newDone) =>
                (acc ++ newPacks, newDone)
              })
          }
      }

    sealed trait ParseError
    object ParseError {
      case class ParseFailure(error: Parser.Error.ParseFailure, path: Path)
          extends ParseError
      case class FileError(readPath: Path, error: Throwable) extends ParseError
    }

    def parseString[A](
        p: P0[A],
        path: Path,
        str: String
    ): ValidatedNel[ParseError, (LocationMap, A)] =
      Parser.parse(p, str).leftMap { nel =>
        nel.map { case pf @ Parser.Error.ParseFailure(_, _, _) =>
          ParseError.ParseFailure(pf, path)
        }
      }

    def parseStart[A](
        p0: P0[A],
        path: Path,
        str: String
    ): ValidatedNel[ParseError, (LocationMap, A)] = {
      val lm = LocationMap(str)
      p0.parse(str) match {
        case Right((_, a)) =>
          Validated.valid((lm, a))
        case Left(err) =>
          val idx = err.failedAtOffset
          Validated.invalidNel(
            ParseError.ParseFailure(
              Parser.Error.ParseFailure(idx, lm, err.expected),
              path
            )
          )
      }
    }

    def parseFile[A](
        p: P0[A],
        path: Path
    ): IO[ValidatedNel[ParseError, (LocationMap, A)]] =
      parseFileOrError(p, path)
        .map {
          case Right(v) => v
          case Left(err) =>
            Validated.invalidNel(ParseError.FileError(path, err))
        }

    /** If we cannot read the file, return the throwable, else parse
      */
    def parseFileOrError[A](
        p: P0[A],
        path: Path
    ): IO[Either[Throwable, ValidatedNel[ParseError, (LocationMap, A)]]] =
      readPath(path).attempt
        .map(_.map(parseString(p, path, _)))

    /** like typecheck, but a no-op for empty lists
      */
    def typeCheck0(
        cmd: MainCommand,
        inputs: List[Path],
        ifs: List[Package.Interface],
        errColor: Colorize,
        packRes: PackageResolver
    )(implicit
        ec: Par.EC
    ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
      NonEmptyList.fromList(inputs) match {
        case None =>
          // we should still return the predef
          // if it is not in ifs
          val useInternalPredef =
            !ifs.exists { (p: Package.Interface) =>
              p.name == PackageName.PredefName
            }

          if (useInternalPredef) {
            moduleIOMonad.pure(
              (PackageMap.fromIterable(PackageMap.predefCompiled :: Nil), Nil)
            )
          } else {
            moduleIOMonad.pure((PackageMap.empty, Nil))
          }
        case Some(nel) => typeCheck(cmd, nel, ifs, errColor, packRes)
      }

    def typeCheck(
        cmd: MainCommand,
        inputs: NonEmptyList[Path],
        ifs: List[Package.Interface],
        errColor: Colorize,
        packRes: PackageResolver
    )(implicit
        ec: Par.EC
    ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
      parseAllInputs(inputs.toList, ifs.map(_.name).toSet, packRes)
        .flatMap { ins =>
          moduleIOMonad.fromTry {
            // Now we have completed all IO, here we do all the checks we need for correctness
            toTry(cmd, ins, errColor)
              .flatMap { packs =>
                // TODO, we could use applicative, to report both duplicate packages and the other
                // errors
                NonEmptyList.fromList(packs) match {
                  case Some(packs) =>
                    val packsString = packs.map { case ((path, lm), parsed) =>
                      ((path.toString, lm), parsed)
                    }
                    PackageMap
                      .typeCheckParsed[String](packsString, ifs, "predef")
                      .strictToValidated match {
                      case Validated.Valid(p) =>
                        val pathToName: List[(Path, PackageName)] =
                          packs.map { case ((path, _), p) =>
                            (path, p.name)
                          }.toList
                        Success((p, pathToName))
                      case Validated.Invalid(errs) =>
                        val sourceMap = PackageMap.buildSourceMap(packs)
                        Failure(
                          MainException.PackageErrors(
                            cmd,
                            sourceMap,
                            errs,
                            errColor
                          )
                        )
                    }
                  case None =>
                    Success((PackageMap.empty, Nil))
                }
              }
          }
        }

    def buildPackMap(
        cmd: MainCommand,
        srcs: List[Path],
        deps: List[Path],
        errColor: Colorize,
        packRes: PackageResolver
    )(implicit
        ec: Par.EC
    ): IO[(PackageMap.Typed[Any], List[(Path, PackageName)])] =
      for {
        packs <- readPackages(deps)
        ifaces = packs.map(Package.interfaceOf(_))
        packsList <- typeCheck0(cmd, srcs, ifaces, errColor, packRes)
        (thesePacks, lst) = packsList
        packMap = packs.foldLeft(PackageMap.toAnyTyped(thesePacks))(_ + _)
      } yield (packMap, lst)

    /** This allows us to use either a path or packagename to select the main
      * file
      */
    sealed abstract class MainIdentifier {
      def path: Option[Path]
      def addIfAbsent(paths: List[Path]): List[Path] =
        path match {
          case Some(p) if !paths.contains(p) => p :: paths
          case _                             => paths
        }
      def getMain(
          ps: List[(Path, PackageName)]
      ): IO[(PackageName, Option[Bindable])]
    }
    object MainIdentifier {
      case class FromPackage(mainPackage: PackageName, value: Option[Bindable])
          extends MainIdentifier {
        def path: Option[Path] = None
        def getMain(
            ps: List[(Path, PackageName)]
        ): IO[(PackageName, Option[Bindable])] =
          moduleIOMonad.pure((mainPackage, value))
      }
      case class FromFile(mainFile: Path) extends MainIdentifier {
        def path: Option[Path] = Some(mainFile)
        def getMain(
            ps: List[(Path, PackageName)]
        ): IO[(PackageName, Option[Bindable])] =
          ps.collectFirst { case (path, pn) if path == mainFile => pn } match {
            case Some(p) => moduleIOMonad.pure((p, None))
            case None =>
              moduleIOMonad.raiseError(
                new Exception(
                  s"could not find file $mainFile in parsed sources"
                )
              )
          }
      }

      def opts(
          pnOpts: Opts[(PackageName, Option[Bindable])],
          fileOpts: Opts[Path]
      ): Opts[MainIdentifier] =
        pnOpts
          .map { case (p, i) => FromPackage(p, i) }
          .orElse(fileOpts.map(FromFile(_)))

      def list(
          packs: Opts[List[(PackageName, Option[Bindable])]],
          files: Opts[List[Path]]
      ): Opts[List[MainIdentifier]] =
        (packs, files).mapN { (ps, fs) =>
          ps.map { case (p, i) => FromPackage(p, i) } ::: fs.map(FromFile(_))
        }

      def addAnyAbsent(
          ms: List[MainIdentifier],
          paths: List[Path]
      ): List[Path] = {
        val present = paths.toSet
        val toAdd = ms.iterator.flatMap(_.path).filterNot(present).toList
        toAdd ::: paths
      }
    }

    /** This is a class that names packages based on path and finds packages
      * based on imports
      */
    sealed abstract class PackageResolver {
      def pathFor(name: PackageName): IO[Option[Path]]
      def packageNameFor(path: Path): Option[PackageName]
    }
    object PackageResolver {
      case object ExplicitOnly extends PackageResolver {
        def pathFor(name: PackageName): IO[Option[Path]] =
          moduleIOMonad.pure(Option.empty[Path])
        def packageNameFor(path: Path): Option[PackageName] = None
      }

      case class LocalRoots(
          roots: NonEmptyList[Path],
          optResolvePath: Option[(Path, PackageName) => IO[Option[Path]]]
      ) extends PackageResolver {
        def pathFor(name: PackageName): IO[Option[Path]] =
          optResolvePath match {
            case None => moduleIOMonad.pure(Option.empty[Path])
            case Some(resolvePath) =>
              def step(p: List[Path]): IO[Either[List[Path], Option[Path]]] =
                p match {
                  case Nil =>
                    moduleIOMonad.pure(Right[List[Path], Option[Path]](None))
                  case phead :: ptail =>
                    resolvePath(phead, name).map {
                      case None => Left[List[Path], Option[Path]](ptail)
                      case some @ Some(_) =>
                        Right[List[Path], Option[Path]](some)
                    }
                }

              moduleIOMonad.tailRecM(roots.toList)(step)
          }

        def packageNameFor(path: Path): Option[PackageName] =
          pathPackage(roots.toList, path)
      }
    }

    sealed abstract class Transpiler(val name: String) {
      def renderAll(
          pm: PackageMap.Typed[Any],
          externals: List[String],
          evaluators: List[String]
      )(implicit ec: Par.EC): IO[List[(NonEmptyList[String], Doc)]]
    }
    object Transpiler {
      case object PythonTranspiler extends Transpiler("python") {
        def renderAll(
            pm: PackageMap.Typed[Any],
            externals: List[String],
            evaluators: List[String]
        )(implicit ec: Par.EC): IO[List[(NonEmptyList[String], Doc)]] = {
          import codegen.python.PythonGen

          val allExternals = pm.allExternals
          val cmp = MatchlessFromTypedExpr.compile(pm)
          moduleIOMonad.catchNonFatal {
            val parsedExt =
              externals.map(Parser.unsafeParse(PythonGen.externalParser, _))
            val extMap = listToUnique(parsedExt.flatten)(
              { case (p, b, _, _) => (p, b) },
              { case (_, _, m, f) => (m, f) },
              "expected each package/name to map to just one file"
            ).get

            val exts = extMap.keySet
            val intrinsic = PythonGen.intrinsicValues
            val missingExternals =
              allExternals.iterator.flatMap { case (p, names) =>
                val missing = names.filterNot { case n =>
                  exts((p, n)) || intrinsic.get(p).exists(_(n))
                }

                if (missing.isEmpty) Nil
                else (p, missing.sorted) :: Nil
              }.toList

            if (missingExternals.isEmpty) {
              val tests = pm.toMap.iterator.flatMap { case (n, pack) =>
                Package.testValue(pack).iterator.map { case (bn, _, _) =>
                  (n, bn)
                }
              }.toMap

              val parsedEvals =
                evaluators.map(Parser.unsafeParse(PythonGen.evaluatorParser, _))
              val typeEvalMap = listToUnique(parsedEvals.flatten)(
                t => t._1,
                t => t._2,
                "expected each type to have to just one evaluator"
              ).get

              val evalMap = pm.toMap.iterator.flatMap { case (n, p) =>
                val optEval = p.lets.findLast { case (_, _, te) =>
                  typeEvalMap.contains(te.getType)
                }
                optEval.map { case (b, _, te) =>
                  val (m, i) = typeEvalMap(te.getType)
                  (n, (b, m, i))
                }
              }.toMap

              val docs = PythonGen
                .renderAll(cmp, extMap, tests, evalMap)
                .iterator
                .map { case (_, (path, doc)) =>
                  (path.map(_.name), doc)
                }
                .toList

              // python also needs empty __init__.py files in every parent directory
              def prefixes[A](
                  paths: List[(NonEmptyList[String], A)]
              ): List[(NonEmptyList[String], Doc)] = {
                val inits =
                  paths.map { case (path, _) =>
                    val parent = path.init
                    val initPy = parent :+ "__init__.py"
                    NonEmptyList.fromListUnsafe(initPy)
                  }.toSet

                inits.toList.sorted.map(p => (p, Doc.empty))
              }

              prefixes(docs) ::: docs
            } else {
              // we need to render this nicer
              val missingDoc =
                missingExternals
                  .sortBy(_._1)
                  .map { case (p, names) =>
                    (Doc.text("package") + Doc.lineOrSpace + Doc.text(
                      p.asString
                    ) + Doc.lineOrSpace +
                      Doc.char('[') +
                      Doc.intercalate(
                        Doc.comma + Doc.lineOrSpace,
                        names.map(b => Doc.text(b.sourceCodeRepr))
                      ) + Doc.char(']')).nested(4)
                  }

              val message = Doc.text(
                "Missing external values:"
              ) + (Doc.line + Doc.intercalate(Doc.line, missingDoc)).nested(4)

              throw new IllegalArgumentException(message.renderTrim(80))
            }
          }
        }
      }

      val all: List[Transpiler] = List(PythonTranspiler)

      implicit def argumentForTranspiler: Argument[Transpiler] =
        new Argument[Transpiler] {
          val nameTo = all.iterator.map(t => (t.name, t)).toMap

          def defaultMetavar: String = "transpiler"
          def read(string: String): ValidatedNel[String, Transpiler] =
            nameTo.get(string) match {
              case Some(t) => Validated.valid(t)
              case None =>
                val keys = nameTo.keys.toList.sorted.mkString(",")
                Validated.invalidNel(
                  s"unknown transpiler: $string, expected one of: $keys"
                )
            }
        }

      val opt: Opts[Transpiler] =
        Opts.option[Transpiler]("lang", "language to transpile to")
    }

    sealed abstract class JsonInput {
      def read: IO[String]
    }

    object JsonInput {
      case class FromString(asString: String) extends JsonInput {
        def read = moduleIOMonad.pure(asString)
      }
      case class FromPath(path: Path) extends JsonInput {
        def read = readPath(path)
      }
    }

    sealed abstract class JsonMode
    object JsonMode {
      case object Write extends JsonMode
      case class Apply(in: JsonInput) extends JsonMode
      case class Traverse(in: JsonInput) extends JsonMode
    }

    type PathGen = org.bykn.bosatsu.PathGen[IO, Path]
    val PathGen = org.bykn.bosatsu.PathGen

    sealed abstract class Inputs
    object Inputs {
      // This allows interfaces
      class Compile(
          srcs: PathGen,
          ifaces: PathGen,
          packageResolver: PackageResolver
      ) extends Inputs {

        private def inNel(cmd: MainCommand): IO[NonEmptyList[Path]] =
          srcs.read.flatMap { ins =>
            NonEmptyList.fromList(ins) match {
              case Some(nel) => moduleIOMonad.pure(nel)
              case None =>
                moduleIOMonad.raiseError(MainException.NoInputs(cmd))
            }
          }

        def compile(cmd: MainCommand, errColor: Colorize)(implicit
            ec: Par.EC
        ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
          for {
            ifpaths <- ifaces.read
            ifs <- readInterfaces(ifpaths)
            ins <- inNel(cmd)
            packPath <- typeCheck(
              cmd,
              ins,
              ifs,
              errColor,
              packageResolver
            )
          } yield packPath
      }

      class Show(
          srcs: PathGen,
          ifaces: PathGen,
          includes: PathGen,
          packageResolver: PackageResolver
      ) extends Inputs {
        def loadAndCompile(cmd: MainCommand, errColor: Colorize)(implicit
            ec: Par.EC
        ): IO[(List[Package.Interface], List[Package.Typed[Any]])] =
          (
            srcs.read,
            ifaces.read.flatMap(readInterfaces),
            includes.read.flatMap(readPackages)
          )
            .flatMapN {
              case (Nil, ifaces, packs) =>
                moduleIOMonad.pure((ifaces, packs))
              case (h :: t, ifaces, packs) =>
                val packIfs = packs.map(Package.interfaceOf(_))
                for {
                  packPath <- typeCheck(
                    cmd,
                    NonEmptyList(h, t),
                    ifaces ::: packIfs,
                    errColor,
                    packageResolver
                  )
                  allPacks = (PackageMap.fromIterable(
                    packs
                  ) ++ packPath._1.toMap.map(_._2)).toMap.toList.map(_._2)
                } yield (ifaces, allPacks)
            }
      }

      class Deps(
          srcs: PathGen,
          ifaces: PathGen,
          includes: PathGen,
          val packageResolver: PackageResolver
      ) extends Inputs {

        def srcList: IO[List[Path]] = srcs.read

        def readIfaces: IO[List[(Path, Package.Interface)]] =
          for {
            ifPaths <- ifaces.read
            withIf <- readInterfaces(ifPaths).map(ifPaths.zip(_))
          } yield withIf

        def readPacks: IO[List[(Path, Package.Typed[Any])]] =
          for {
            pPaths <- includes.read
            withPs <- readPackages(pPaths).map(pPaths.zip(_))
          } yield withPs
      }

      class Runtime(
          srcs: PathGen,
          includes: PathGen,
          packageResolver: PackageResolver
      ) extends Inputs {

        def packMap(
            cmd: MainCommand,
            mis: List[MainIdentifier],
            errColor: Colorize
        )(implicit
            ec: Par.EC
        ): IO[(PackageMap.Typed[Any], List[(Path, PackageName)])] =
          for {
            ins <- srcs.read
            ds <- includes.read
            ins1 = MainIdentifier.addAnyAbsent(mis, ins)
            pn <-
              if (ds.isEmpty && ins1.isEmpty)
                moduleIOMonad.raiseError(MainException.NoInputs(cmd))
              else
                buildPackMap(
                  cmd,
                  srcs = ins1,
                  deps = ds,
                  errColor,
                  packageResolver
                )
          } yield pn
      }

      private def pathGen(
          arg: String,
          help: String,
          ext: String
      ): Opts[PathGen] = {
        val direct = Opts
          .options[Path](arg, help = help)
          .orEmpty
          .map(paths => paths.foldMap(PathGen.Direct[IO, Path](_): PathGen))

        unfoldDir match {
          case None => direct
          case Some(unfold) =>
            val select = hasExtension(ext)
            val child1 = Opts
              .options[Path](arg + "_dir", help = s"all $help in directory")
              .orEmpty
              .map { paths =>
                paths.foldMap(
                  PathGen
                    .ChildrenOfDir[IO, Path](_, select, false, unfold): PathGen
                )
              }
            val childMany = Opts
              .options[Path](
                arg + "_all_subdir",
                help = s"all $help recursively in all directories"
              )
              .orEmpty
              .map { paths =>
                paths.foldMap(
                  PathGen
                    .ChildrenOfDir[IO, Path](_, select, true, unfold): PathGen
                )
              }

            (direct, child1, childMany).mapN { (a, b, c) =>
              (a :: b :: c :: Nil).combineAll
            }
        }
      }
      private val srcs =
        pathGen("input", help = "input source files", ".bosatsu")
      private val ifaces =
        pathGen("interface", help = "interface files", ".bosatsig")
      private val includes = pathGen(
        "include",
        help = "compiled packages to include files",
        ".bosatsu_package"
      )
      private val packRoot =
        Opts.options[Path](
          "package_root",
          help = "for implicit package names, consider these paths as roots"
        )
      private val packSearch =
        resolvePath match {
          case None => Opts(None)
          case some @ Some(_) =>
            Opts
              .flag(
                "search",
                help =
                  "if set, we search the package_roots for imports not explicitly given"
              )
              .orFalse
              .map {
                case true  => some
                case false => None
              }
        }

      private val packRes: Opts[PackageResolver] =
        (packRoot
          .product(packSearch))
          .orNone
          .map {
            case None => PackageResolver.ExplicitOnly
            case Some((paths, search)) =>
              PackageResolver.LocalRoots(paths, search)
          }

      // type-checking and writing protos should be explicit. search option isn't supported
      private val noSearchRes: Opts[PackageResolver] =
        packRoot.orNone
          .map {
            case None        => PackageResolver.ExplicitOnly
            case Some(paths) => PackageResolver.LocalRoots(paths, None)
          }

      val compileOpts: Opts[Inputs.Compile] =
        (srcs, ifaces, noSearchRes).mapN(new Compile(_, _, _))

      val runtimeOpts: Opts[Inputs.Runtime] =
        (srcs, includes, packRes).mapN(new Runtime(_, _, _))

      val showOpts: Opts[Inputs.Show] =
        (srcs, ifaces, includes, packRes).mapN(new Show(_, _, _, _))

      val depsOpts: Opts[Inputs.Deps] =
        (srcs, ifaces, includes, packRes).mapN(new Deps(_, _, _, _))

    }

    case class TranspileCommand(
        inputs: Inputs.Runtime,
        errColor: Colorize,
        generator: Transpiler,
        outDir: Path,
        exts: List[Path],
        evals: List[Path]
    ) extends MainCommand("transpile") {

      // case class TranspileOut(outs: Map[PackageName, (List[String], Doc)], base: Path) extends Output
      type Result = Output.TranspileOut

      def run =
        withEC { implicit ec =>
          for {
            pn <- inputs.packMap(this, Nil, errColor)
            (packs, names) = pn
            extStrs <- exts.traverse(readPath)
            evalStrs <- evals.traverse(readPath)
            data <- generator.renderAll(packs, extStrs, evalStrs)
          } yield Output.TranspileOut(data, outDir)
        }
    }

    case class Evaluate(
        inputs: Inputs.Runtime,
        mainPackage: MainIdentifier,
        errColor: Colorize
    ) extends MainCommand("eval") {

      type Result = Output.EvaluationResult

      def runEval: IO[(Evaluation[Any], Output.EvaluationResult)] = withEC {
        implicit ec =>
          for {
            (packs, names) <- inputs.packMap(this, List(mainPackage), errColor)
            (mainPackageName, value) <- mainPackage.getMain(names)
            out <-
              if (packs.toMap.contains(mainPackageName)) {
                val ev = Evaluation(packs, Predef.jvmExternals)

                val res = value match {
                  case None        => ev.evaluateMain(mainPackageName)
                  case Some(ident) => ev.evaluateName(mainPackageName, ident)
                }

                res match {
                  case None =>
                    moduleIOMonad.raiseError(
                      new Exception("found no main expression")
                    )
                  case Some((eval, tpe)) =>
                    // here is the doc:
                    val memoE = eval.memoize
                    val fn = ev.valueToDoc.toDoc(tpe)
                    val edoc =
                      memoE.map { v =>
                        fn(v) match {
                          case Right(d)  => d
                          case Left(err) =>
                            // $COVERAGE-OFF$ unreachable due to being well typed
                            sys.error(s"got illtyped error: $err")
                          // $COVERAGE-ON$
                        }
                      }

                    moduleIOMonad.pure(
                      (ev, Output.EvaluationResult(eval, tpe, edoc))
                    )
                }
              } else {
                moduleIOMonad.raiseError(
                  new Exception(
                    s"package ${mainPackageName.asString} not found"
                  )
                )
              }
          } yield out
      }

      def run = runEval.map(_._2)
    }

    case class ToJson(
        inputs: Inputs.Runtime,
        mode: JsonMode,
        mainPackage: MainIdentifier,
        outputOpt: Option[Path],
        errColor: Colorize
    ) extends MainCommand("json") {

      type Result = Output.JsonOutput

      private def showError[A](prefix: String, str: String, idx: Int): IO[A] = {
        val errMsg0 = str.substring(idx + 1)
        val errMsg =
          if (errMsg0.length > 20)
            errMsg0.take(20) + s"... (and ${errMsg0.length - 20} more"
          else errMsg0

        moduleIOMonad.raiseError(
          new Exception(s"$prefix at ${idx + 1}: $errMsg")
        )
      }

      private def ioJson(io: IO[String]): IO[Json] =
        io.flatMap { jsonString =>
          Json.parserFile.parseAll(jsonString) match {
            case Right(j) => moduleIOMonad.pure(j)
            case Left(err) =>
              val idx = err.failedAtOffset
              showError("could not parse a JSON record", jsonString, idx)
          }
        }

      def run =
        Evaluate(inputs, mainPackage, errColor).runEval
          .flatMap { case (ev, res) =>
            val v2j = ev.valueToJson
            def unsupported[A](j: JsonEncodingError.UnsupportedType): IO[A] = {
              def typeDoc(t: rankn.Type) =
                rankn.Type.fullyResolvedDocument.document(t)
              val path = j.path.init
              val badType = j.path.last
              val pathMsg = path match {
                case Nil => Doc.empty
                case nonE =>
                  val sep =
                    Doc.lineOrSpace + Doc.text("contains") + Doc.lineOrSpace
                  val pd =
                    (Doc.intercalate(sep, nonE.map(typeDoc(_))) + sep + typeDoc(
                      badType
                    )).nested(4)
                  pd + Doc.hardLine + Doc.hardLine + Doc.text(
                    "but"
                  ) + Doc.hardLine + Doc.hardLine
              }
              val msg = pathMsg + Doc.text("the type") + Doc.space + typeDoc(
                badType
              ) + Doc.space + Doc.text("isn't supported")
              val tpeStr = msg.render(80)

              moduleIOMonad.raiseError(
                new Exception(s"cannot convert type to Json: $tpeStr")
              )
            }

            def process[F[_]: Traverse](
                io: IO[String],
                extract: Json => IO[F[Json]],
                inject: F[Json] => Json
            ): IO[Output.JsonOutput] =
              v2j.valueFnToJsonFn(res.tpe) match {
                case Left(unsup) => unsupported(unsup)
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
                                    new Exception(
                                      s"invalid input json: $dataError"
                                    )
                                  )
                                case Right(json) =>
                                  moduleIOMonad.pure(json)
                              }
                            case otherJson =>
                              moduleIOMonad.raiseError[Json](
                                new Exception(
                                  s"required a json array of size $arity, found:\n\n${otherJson.render}"
                                )
                              )
                          }
                        }
                        .map { fjson =>
                          Output.JsonOutput(inject(fjson), outputOpt)
                        }
                    case Left(valueError) =>
                      // shouldn't happen since value should be well typed
                      moduleIOMonad.raiseError(
                        new Exception(s"unexpected value error: $valueError")
                      )
                  }
              }

            mode match {
              case JsonMode.Write =>
                v2j.toJson(res.tpe) match {
                  case Left(unsup) => unsupported(unsup)
                  case Right(fn) =>
                    fn(res.value.value) match {
                      case Left(valueError) =>
                        moduleIOMonad.raiseError(
                          new Exception(s"unexpected value error: $valueError")
                        )
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
                    case other =>
                      moduleIOMonad.raiseError(
                        new Exception(
                          s"require an array or arrays for traverse, found: ${other.getClass}"
                        )
                      )
                  },
                  items => Json.JArray(items)
                )
            }
          }
    }

    case class Check(
        inputs: Inputs.Compile,
        output: Option[Path],
        ifout: Option[Path],
        errColor: Colorize
    ) extends MainCommand("check") {

      type Result = Output.CompileOut

      def run =
        withEC { implicit ec =>
          for {
            packPath <- inputs.compile(this, errColor)
            packs = packPath._1
            packList =
              packs.toMap.iterator
                .map { case (_, p) => p }
                // TODO currently we recompile predef in every run, so every interface includes
                // predef, we filter that out
                .filter(_.name != PackageName.PredefName)
                .toList
                .sortBy(_.name)
          } yield Output.CompileOut(packList, ifout, output)
        }
    }

    case class RunTests(
        inputs: Inputs.Runtime,
        testPacks: List[MainIdentifier],
        errColor: Colorize
    ) extends MainCommand("test") {

      type Result = Output.TestOutput

      def run = withEC { implicit ec =>
        for {
          (packs, nameMap) <- inputs.packMap(this, testPacks, errColor)
          testPackIdents <- testPacks.traverse(_.getMain(nameMap))
        } yield {
          val testPackNames: List[PackageName] = testPackIdents.map(_._1)
          val testIt: Iterator[PackageName] =
            if (testPacks.isEmpty) {
              // if there are no given files or packages to test, assume
              // we test all the files
              nameMap.iterator.map(_._2)
            } else {
              // otherwise we have a specific list packages/files to test
              testPackNames.iterator
            }

          val testPackages: List[PackageName] =
            testIt.toList.sorted.distinct
          val ev = Evaluation(packs, Predef.jvmExternals)
          val res0 = testPackages.map(p => (p, ev.evalTest(p)))
          val res =
            if (testPacks.isEmpty) res0.filter { case (_, testRes) =>
              testRes.isDefined
            }
            else res0

          Output.TestOutput(res, errColor)
        }
      }
    }

    case class Show(
        inputs: Inputs.Show,
        output: Option[Path],
        errColor: Colorize
    ) extends MainCommand("show") {

      type Result = Output.ShowOutput

      def run = withEC { implicit ec =>
        for {
          (ifaces, packs0) <- inputs.loadAndCompile(this, errColor)
          packs = packs0.filterNot(_.name == PackageName.PredefName)
        } yield Output.ShowOutput(packs, ifaces, output)
      }
    }

    case class Deps(
        inputs: Inputs.Deps,
        output: Option[Path],
        errColor: Colorize,
        style: GraphOutput
    ) extends MainCommand("deps") {

      type Result = Output.DepsOutput

      def srcDeps(
          paths: List[Path]
      ): IO[List[(Path, PackageName, FileKind, List[PackageName])]] =
        for {
          maybeParsed <- parseHeaders(paths, inputs.packageResolver)
          parsed <- moduleIOMonad.fromTry(toTry(this, maybeParsed, errColor))
        } yield parsed.map { case (path, (pn, imps, _)) =>
          (path, pn, FileKind.Source, norm(imps.map(_.pack)))
        }

      def ifaceDeps(iface: Package.Interface): List[PackageName] = {
        val pn = iface.name
        norm(
          iface.exports.iterator
            .flatMap { n =>
              n.tag match {
                case Referant.Value(t) => Iterator.single(t)
                case _                 => Iterator.empty
              }
            }
            .flatMap(rankn.Type.constantsOf)
            .collect { case rankn.Type.Const.Defined(p, _) if p != pn => p }
            .toList
        )
      }

      private def norm(lst: List[PackageName]): List[PackageName] =
        lst
          .filterNot(_ == PackageName.PredefName)
          .distinct
          .sorted

      def packageDeps(pack: Package.Typed[Any]): List[PackageName] =
        norm(pack.imports.map(_.pack.name))

      def run =
        for {
          srcPaths <- inputs.srcList
          sdeps <- srcDeps(srcPaths)
          ifaces <- inputs.readIfaces
          packs <- inputs.readPacks
          ideps = ifaces.map { case (p, iface) =>
            (p, iface.name, FileKind.Iface, ifaceDeps(iface))
          }
          pdeps = packs.map { case (p, pack) =>
            (p, pack.name, FileKind.Pack, packageDeps(pack))
          }
        } yield Output.DepsOutput(sdeps ::: ideps ::: pdeps, output, style)
    }

    def toTry[A](
        cmd: MainCommand,
        v: ValidatedNel[ParseError, A],
        color: Colorize
    ): Try[A] =
      v match {
        case Validated.Valid(a) => Success(a)
        case Validated.Invalid(errs) =>
          Failure(MainException.ParseErrors(cmd, errs, color))
      }

    val opts: Opts[MainCommand] = {

      def argFromParser[A](
          p: P0[A],
          defmeta: String,
          typeName: String,
          suggestion: String
      ): Argument[A] =
        new Argument[A] {
          def defaultMetavar: String = defmeta
          def read(string: String): ValidatedNel[String, A] =
            p.parseAll(string) match {
              case Right(a) => Validated.valid(a)
              case _ =>
                val sugSpace = if (suggestion.nonEmpty) s" $suggestion" else ""
                Validated.invalidNel(
                  s"could not parse $string as a $typeName." + sugSpace
                )
            }
        }

      implicit val argPack: Argument[PackageName] =
        argFromParser(
          PackageName.parser,
          "packageName",
          "package name",
          "Must be capitalized strings separated by /"
        )

      implicit val argValue: Argument[(PackageName, Option[Bindable])] =
        argFromParser(
          (PackageName.parser ~ (P.string(
            "::"
          ) *> Identifier.bindableParser).?),
          "valueIdent",
          "package or package::name",
          "Must be a package name with an optional :: value, e.g. Foo/Bar or Foo/Bar::baz."
        )

      implicit val argColor: Argument[Colorize] =
        new Argument[Colorize] {
          def defaultMetavar: String = "color"
          def read(str: String): ValidatedNel[String, Colorize] =
            str.toLowerCase match {
              case "none" => Validated.valid(Colorize.None)
              case "ansi" => Validated.valid(Colorize.Console)
              case "html" => Validated.valid(Colorize.HmtlFont)
              case other =>
                Validated.invalidNel(
                  s"unknown colorize: $other, expected: none, ansi or html"
                )
            }
        }

      val colorOpt = Opts
        .option[Colorize]("color", help = "colorize mode: none, ansi or html")
        .orElse(Opts(Colorize.Console))

      val mainP =
        MainIdentifier.opts(
          Opts.option[(PackageName, Option[Bindable])](
            "main",
            help =
              "main value to evaluate (package name or full identifier to a value)"
          ),
          Opts.option[Path](
            "main_file",
            help = "file containing the main package to evaluate"
          )
        )

      val testP =
        MainIdentifier.list(
          Opts
            .options[PackageName](
              "test_package",
              help = "package for which to run tests"
            )
            .map(_.map((_, None)))
            .orEmpty,
          Opts
            .options[Path](
              "test_file",
              help = "file containing the package for which to run tests"
            )
            .orEmpty
        )

      val outputPath = Opts.option[Path]("output", help = "output path")
      val interfaceOutputPath =
        Opts.option[Path]("interface_out", help = "interface output path")

      val jsonCommand = {
        def toJsonOpt(modeOpt: Opts[JsonMode]) =
          (Inputs.runtimeOpts, modeOpt, mainP, outputPath.orNone, colorOpt)
            .mapN(ToJson(_, _, _, _, _))

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

        val subs = Opts
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

        Opts.subcommand("json", "json writing and transformation tools")(subs)
      }

      val transpileOpt = (
        Inputs.runtimeOpts,
        colorOpt,
        Transpiler.opt,
        Opts.option[Path](
          "outdir",
          help = "directory to write all output into"
        ),
        Opts
          .options[Path](
            "externals",
            help =
              "external descriptors the transpiler uses to rewrite external defs"
          )
          .orEmpty,
        Opts
          .options[Path](
            "evaluators",
            help = "evaluators which run values of certain types"
          )
          .orEmpty
      )
        .mapN(TranspileCommand(_, _, _, _, _, _))

      val evalOpt = (Inputs.runtimeOpts, mainP, colorOpt)
        .mapN(Evaluate(_, _, _))

      val typeCheckOpt = (
        Inputs.compileOpts,
        outputPath.orNone,
        interfaceOutputPath.orNone,
        colorOpt
      )
        .mapN(Check(_, _, _, _))

      val testOpt = (Inputs.runtimeOpts, testP, colorOpt)
        .mapN(RunTests(_, _, _))

      Opts
        .subcommand("eval", "evaluate an expression and print the output")(
          evalOpt
        )
        .orElse(
          Opts.subcommand("check", "type check a set of packages")(
            typeCheckOpt
          )
        )
        .orElse(
          Opts.subcommand("test", "test a set of bosatsu modules")(testOpt)
        )
        .orElse(jsonCommand)
        .orElse(
          Opts.subcommand(
            "transpile",
            "transpile bosatsu into another language"
          )(transpileOpt)
        )
        .orElse(
          Opts.subcommand("show", "show compiled packages")(
            (Inputs.showOpts, outputPath.orNone, colorOpt)
              .mapN(Show(_, _, _))
          )
        )
        .orElse(
          Opts.subcommand("deps", "emit a graph description of dependencies")(
            (
              Inputs.depsOpts,
              outputPath.orNone,
              colorOpt,
              GraphOutput.jsonOrDot
            )
              .mapN(Deps(_, _, _, _))
          )
        )
    }

    def command: Command[MainCommand] = {
      val versionInfo =
        (s"version: ${BuildInfo.version}" ::
          s"scala-version: ${BuildInfo.scalaVersion}" ::
          (BuildInfo.gitHeadCommit.toList.map(sha => s"git-sha: ${sha}")))
          .mkString("\n")

      Command(
        "bosatsu",
        s"a total and functional programming language\n\n$versionInfo"
      )(opts)
    }
  }
}
