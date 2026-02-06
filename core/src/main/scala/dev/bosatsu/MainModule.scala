package dev.bosatsu

import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.implicits.catsKernelOrderingForOrder
import cats.Traverse
import com.monovore.decline.{Argument, Command, Help, Opts}
import cats.parse.{Parser => P}
import org.typelevel.paiges.Doc
import _root_.bosatsu.{TypedAst => proto}
import dev.bosatsu.Parser.argFromParser
import dev.bosatsu.tool_command
import dev.bosatsu.tool.{
  CliException,
  CompilerApi,
  ExitCode,
  FileKind,
  GraphOutput,
  Output,
  PathGen => ToolPathGen,
  PackageResolver,
  PathParseError
}
import dev.bosatsu.cruntime
import dev.bosatsu.library.{
  DecodedLibrary,
  LibConfig,
  LibraryEvaluation,
  Name,
  Version
}
import dev.bosatsu.library.LibConfig.LibMethods
import dev.bosatsu.hashing.Algo
import org.typelevel.paiges.Document
import java.util.regex.Pattern

import codegen.Transpiler

import Identifier.Bindable
import LocationMap.Colorize

import cats.syntax.all._

/** This is an implementation of the CLI tool where Path is abstracted. The idea
  * is to allow it to be testable and usable in scalajs where we don't have
  * file-IO
  */
class MainModule[IO[_], Path](val platformIO: PlatformIO[IO, Path]) {
  type F[A] = IO[A]

  import platformIO._

  //////////////////////////////
  // Below here are concrete and should not use override
  //////////////////////////////

  final def run(args: List[String]): Either[Help, IO[Output[Path]]] =
    MainCommand.command
      .parse(args.toList)
      .map(_.run.widen)

  final def runAndReport(args: List[String]): Either[Help, IO[ExitCode]] =
    run(args).map(report)

  sealed abstract class MainException extends Exception with CliException {
    def messageString: String
  }
  object MainException {
    case class NoInputs(command: MainCommand) extends MainException {
      def messageString: String = {
        val name = command.name
        s"no inputs given to $name"
      }

      def exitCode: ExitCode = ExitCode.Error
      def errDoc = Doc.text(messageString)
      def stdOutDoc: Doc = Doc.empty
    }

    case class ParseErrors(
        errors: NonEmptyList[PathParseError[Path]],
        color: Colorize
    ) extends MainException {

      def messages: List[String] =
        errors.toList.flatMap {
          case PathParseError.ParseFailure(pf, path) =>
            // we should never be partial here
            val (r, c) = pf.locations.toLineCol(pf.position).get
            val ctx = pf.showContext(color)
            List(
              s"failed to parse $path:${r + 1}:${c + 1}",
              ctx.render(80)
            )
          case PathParseError.FileError(path, err) =>
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

      def messageString: String = messages.mkString("\n")
      def errDoc = Doc.intercalate(Doc.hardLine, messages.map(Doc.text(_)))
      def stdOutDoc: Doc = Doc.empty
      def exitCode: ExitCode = ExitCode.Error
    }
  }

  def mainExceptionToString(ex: Throwable): Option[String] =
    ex match {
      case me: MainException => Some(me.messageString)
      case _                 => None
    }

  sealed abstract class MainCommand(val name: String) {
    type Result <: Output[Path]
    def run: IO[Result]
  }

  object MainCommand {

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
          ps.collectFirst {
            case (path, pn) if pathOrdering.equiv(path, mainFile) => pn
          } match {
            case Some(p) => moduleIOMonad.pure((p, None))
            case None    =>
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

    val transOpt: Opts[Transpiler.Optioned[F, Path]] =
      cats
        .Alternative[Opts]
        .combineAllK(
          codegen.python.PythonTranspiler.opts(platformIO) ::
            codegen.clang.ClangTranspiler.opts(platformIO) ::
            Nil
        )

    sealed abstract class JsonInput {
      def read: IO[String]
    }

    object JsonInput {
      case class FromString(asString: String) extends JsonInput {
        def read = moduleIOMonad.pure(asString)
      }
      case class FromPath(path: Path) extends JsonInput {
        def read = readUtf8(path)
      }
    }

    sealed abstract class JsonMode derives CanEqual
    object JsonMode {
      case object Write extends JsonMode
      case class Apply(in: JsonInput) extends JsonMode
      case class Traverse(in: JsonInput) extends JsonMode
    }

    type PathGen = ToolPathGen[IO, Path]
    val PathGen = ToolPathGen

    sealed abstract class Inputs
    object Inputs {
      private type DepDecode = (Path, DecodedLibrary[Algo.Blake3])

      private def readDep(path: Path): IO[DecodedLibrary[Algo.Blake3]] =
        readLibrary(path).flatMap(DecodedLibrary.decode(_))

      private def validateDepSet(
          pub: List[DecodedLibrary[Algo.Blake3]],
          priv: List[DecodedLibrary[Algo.Blake3]]
      ): IO[Unit] = {
        val conf = LibConfig(
          name = Name("_tool"),
          repoUri = "",
          nextVersion = Version.zero,
          previous = None,
          exportedPackages = Nil,
          allPackages = LibConfig.PackageFilter.Regex(Pattern.compile(".*")) :: Nil,
          publicDeps = pub.map(_.toDep),
          privateDeps = priv.map(_.toDep),
          defaultMain = None
        )
        val validated = conf.validateDeps(pub ::: priv)
        moduleIOMonad.fromTry(LibConfig.Error.toTry(validated)).void
      }

      private def readDepLibraries(
          pubPaths: List[Path],
          privPaths: List[Path]
      ): IO[(List[DepDecode], List[DepDecode])] =
        (
          pubPaths.traverse(path => readDep(path).map(path -> _)),
          privPaths.traverse(path => readDep(path).map(path -> _))
        ).flatMapN { (pub, priv) =>
          validateDepSet(pub.map(_._2), priv.map(_._2))
            .as((pub, priv))
        }

      private def depInterfaces(
          depLibs: (List[DepDecode], List[DepDecode])
      ): List[Package.Interface] =
        (depLibs._1 ::: depLibs._2).flatMap(_._2.interfaces)

      private def depPacks(
          depLibs: (List[DepDecode], List[DepDecode])
      ): List[Package.Typed[Any]] =
        (depLibs._1 ::: depLibs._2).flatMap { case (_, dec) =>
          dec.implementations.toMap.values.toList
        }

      private def duplicatePackNames(
          packs: List[Package.Typed[Any]]
      ): List[PackageName] =
        packs
          .groupBy(_.name)
          .collect { case (pn, _ :: _ :: _) => pn }
          .toList
          .sorted

      private def ensureDistinctPackages(
          packs: List[Package.Typed[Any]],
          note: String
      ): IO[Unit] = {
        val dups = duplicatePackNames(packs)
        if (dups.isEmpty) moduleIOMonad.unit
        else
          moduleIOMonad.raiseError(
            CliException.Basic(
              s"duplicate package names in $note: ${dups.map(_.asString).mkString(", ")}"
            )
          )
      }

      // This allows interfaces
      class Compile(
          srcs: PathGen,
          ifaces: PathGen,
          packageResolver: PackageResolver[IO, Path],
          pubDeps: List[Path],
          privDeps: List[Path]
      ) extends Inputs {

        private def inNel(cmd: MainCommand): IO[NonEmptyList[Path]] =
          srcs.read.flatMap { ins =>
            NonEmptyList.fromList(ins) match {
              case Some(nel) => moduleIOMonad.pure(nel)
              case None      =>
                moduleIOMonad.raiseError(MainException.NoInputs(cmd))
            }
          }

        def compile(cmd: MainCommand, errColor: Colorize)(implicit
            ec: Par.EC
        ): IO[PackageMap.Inferred] =
          for {
            deps <- readDepLibraries(pubDeps, privDeps)
            ifpaths <- ifaces.read
            ifs <- readInterfaces(ifpaths)
            ins <- inNel(cmd)
            packPath <- CompilerApi.typeCheck(
              platformIO,
              ins,
              ifs ::: depInterfaces(deps),
              errColor,
              packageResolver
            )
          } yield packPath._1
      }

      class Show(
          srcs: PathGen,
          ifaces: PathGen,
          includes: PathGen,
          packageResolver: PackageResolver[IO, Path],
          pubDeps: List[Path],
          privDeps: List[Path]
      ) extends Inputs {
        def loadAndCompile(errColor: Colorize)(implicit
            ec: Par.EC
        ): IO[(List[Package.Interface], List[Package.Typed[Any]])] =
          (
            srcs.read,
            ifaces.read.flatMap(readInterfaces),
            includes.read.flatMap(readPackages),
            readDepLibraries(pubDeps, privDeps)
          )
            .flatMapN {
              case (Nil, ifaces, packs, depLibs) =>
                val depIfs = depInterfaces(depLibs)
                val depPkgs = depPacks(depLibs)
                val allPacks = packs ::: depPkgs
                ensureDistinctPackages(allPacks, "show dependencies")
                  .as((ifaces ::: depIfs, allPacks))
              case (h :: t, ifaces, packs, depLibs) =>
                val depIfs = depInterfaces(depLibs)
                val depPkgs = depPacks(depLibs)
                val existingPacks = packs ::: depPkgs
                val packIfs = existingPacks.map(Package.interfaceOf(_))
                for {
                  packPath <- CompilerApi.typeCheck(
                    platformIO,
                    NonEmptyList(h, t),
                    ifaces ::: depIfs ::: packIfs,
                    errColor,
                    packageResolver
                  )
                  allPacks = (PackageMap.fromIterable(
                    existingPacks
                  ) ++ packPath._1.toMap.map(_._2)).toMap.toList.map(_._2)
                  _ <- ensureDistinctPackages(allPacks, "show dependencies")
                } yield (ifaces ::: depIfs, allPacks)
            }
      }

      class Deps(
          srcs: PathGen,
          ifaces: PathGen,
          includes: PathGen,
          val packageResolver: PackageResolver[IO, Path],
          pubDeps: List[Path],
          privDeps: List[Path]
      ) extends Inputs {

        def srcList: IO[List[Path]] = srcs.read

        def readIfaces: IO[List[(Path, Package.Interface)]] =
          for {
            ifPaths <- ifaces.read
            withIf <- readInterfaces(ifPaths).map(ifPaths.zip(_))
            depLibs <- readDepLibraries(pubDeps, privDeps)
            depIfs = (depLibs._1 ::: depLibs._2).flatMap { case (path, dep) =>
              dep.interfaces.map(path -> _)
            }
          } yield withIf ::: depIfs

        def readPacks: IO[List[(Path, Package.Typed[Any])]] =
          for {
            pPaths <- includes.read
            withPs <- readPackages(pPaths).map(pPaths.zip(_))
            depLibs <- readDepLibraries(pubDeps, privDeps)
            depPs = (depLibs._1 ::: depLibs._2).flatMap { case (path, dep) =>
              dep.implementations.toMap.values.map(path -> _)
            }
          } yield withPs ::: depPs
      }

      class Runtime(
          srcs: PathGen,
          includes: PathGen,
          packageResolver: PackageResolver[IO, Path],
          pubDeps: List[Path],
          privDeps: List[Path]
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
            includePaths <- includes.read
            includePacks <- readPackages(includePaths)
            depLibs <- readDepLibraries(pubDeps, privDeps)
            depIfs = depInterfaces(depLibs)
            depPackList = depPacks(depLibs)
            ins1 = MainIdentifier.addAnyAbsent(mis, ins)
            srcWithNames <-
              if (includePacks.isEmpty && depPackList.isEmpty && ins1.isEmpty)
                moduleIOMonad.raiseError(MainException.NoInputs(cmd))
              else {
                NonEmptyList.fromList(ins1) match {
                  case None =>
                    moduleIOMonad.pure((PackageMap.empty, List.empty[(Path, PackageName)]))
                  case Some(srcNel) =>
                    val baseIfaces =
                      includePacks.map(Package.interfaceOf(_)) ::: depIfs
                    CompilerApi
                      .typeCheck(
                        platformIO,
                        srcNel,
                        baseIfaces,
                        errColor,
                        packageResolver
                      )
                      .map { case (pm, names) =>
                        (PackageMap.toAnyTyped(pm), names.toList)
                      }
                }
              }
            (srcPacks, names) = srcWithNames
            allPacks = includePacks ::: depPackList ::: srcPacks.toMap.values.toList
            _ <- ensureDistinctPackages(allPacks, "runtime inputs and dependencies")
          } yield (PackageMap.fromIterable(allPacks), names)
      }

      private val srcs =
        PathGen.opts("input", help = "input source files", ".bosatsu")(
          platformIO
        )
      private val ifaces =
        PathGen.opts("interface", help = "interface files", ".bosatsig")(
          platformIO
        )
      private val includes = PathGen.opts(
        "include",
        help = "compiled packages to include files",
        ".bosatsu_package"
      )(platformIO)
      private val pubDeps =
        Opts
          .options[Path](
            "pub_dep",
            help =
              "public dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
          )
          .orEmpty
      private val privDeps =
        Opts
          .options[Path](
            "priv_dep",
            help =
              "private dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
          )
          .orEmpty

      private val packRes = PackageResolver.opts(platformIO)
      private val noSearchRes = PackageResolver.noSearchOpts(platformIO)

      val compileOpts: Opts[Inputs.Compile] =
        (srcs, ifaces, noSearchRes, pubDeps, privDeps).mapN(
          new Compile(_, _, _, _, _)
        )

      val runtimeOpts: Opts[Inputs.Runtime] =
        (srcs, includes, packRes, pubDeps, privDeps).mapN(
          new Runtime(_, _, _, _, _)
        )

      val showOpts: Opts[Inputs.Show] =
        (srcs, ifaces, includes, packRes, pubDeps, privDeps).mapN(
          new Show(_, _, _, _, _, _)
        )

      val depsOpts: Opts[Inputs.Deps] =
        (srcs, ifaces, includes, packRes, pubDeps, privDeps).mapN(
          new Deps(_, _, _, _, _, _)
        )

    }

    case class TranspileCommand(
        inputs: Inputs.Runtime,
        errColor: Colorize,
        generator: Transpiler.Optioned[F, Path]
    ) extends MainCommand("transpile") {

      type Result = Output.TranspileOut[Path]

      def run =
        withEC {
          for {
            pn <- inputs.packMap(this, Nil, errColor)
            (packs, names) = pn
            data <- generator.renderAll(packs)
          } yield Output.TranspileOut(data)
        }
    }

    case class Evaluate(
        inputs: Inputs.Runtime,
        mainPackage: MainIdentifier,
        errColor: Colorize
    ) extends MainCommand("eval") {

      type Result = Output.EvaluationResult

      def runEval: IO[(LibraryEvaluation[Unit], Output.EvaluationResult)] =
        withEC {
          for {
            (packs, names) <- inputs.packMap(this, List(mainPackage), errColor)
            (mainPackageName, value) <- mainPackage.getMain(names)
            ev = LibraryEvaluation.fromPackageMap(packs, Predef.jvmExternals)
            (eval, tpe) <- moduleIOMonad.fromEither {
              value match {
                case None        => ev.evaluateMainValue(mainPackageName)
                case Some(ident) => ev.evaluateNameValue(mainPackageName, ident)
              }
            }
            // here is the doc:
            memoE = eval.memoize
            fn = ev.valueToDoc.toDoc(tpe)
            edoc = memoE.map { v =>
              fn(v) match {
                case Right(d)  => d
                case Left(err) =>
                  // $COVERAGE-OFF$ unreachable due to being well typed
                  sys.error(s"got illtyped error: $err")
                // $COVERAGE-ON$
              }
            }
          } yield (ev, Output.EvaluationResult(eval, tpe, edoc))
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

      type Result = Output.JsonOutput[Path]

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
            case Right(j)  => moduleIOMonad.pure(j)
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
                case Nil  => Doc.empty
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

            def process[G[_]: Traverse](
                io: IO[String],
                extract: Json => IO[G[Json]],
                inject: G[Json] => Json
            ): IO[Output.JsonOutput[Path]] =
              v2j.valueFnToJsonFn(res.tpe) match {
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
                  case Right(fn)   =>
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
                    case other              =>
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

      type Result = Output.CompileOut[Path]

      def run =
        withEC {
          for {
            packs <- inputs.compile(this, errColor)
            packList =
              packs.toMap.iterator
                .map { case (_, p) => p }
                // TODO currently we recompile predef in every run, so every interface includes (https://github.com/johnynek/bosatsu/issues/414)
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

      def run = withEC {
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
          val ev = LibraryEvaluation.fromPackageMap(packs, Predef.jvmExternals)
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

      type Result = Output.ShowOutput[Path]

      def run = withEC {
        for {
          (ifaces, packs0) <- inputs.loadAndCompile(errColor)
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

      type Result = Output.DepsOutput[Path]

      def srcDeps(
          paths: List[Path]
      ): IO[List[(Path, PackageName, FileKind, List[PackageName])]] =
        for {
          maybeParsed <- inputs.packageResolver.parseHeaders(paths)(platformIO)
          parsed <- liftParseErrors(maybeParsed, errColor)
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

    case class FromOutput[Out <: Output[Path]](
        commandName: String,
        run: IO[Out]
    ) extends MainCommand(commandName) {
      type Result = Out
    }

    private def liftParseErrors[A](
        v: ValidatedNel[PathParseError[Path], A],
        color: Colorize
    ): F[A] =
      v match {
        case Validated.Valid(a)      => moduleIOMonad.pure(a)
        case Validated.Invalid(errs) =>
          moduleIOMonad.raiseError(MainException.ParseErrors(errs, color))
      }

    implicit val argValue: Argument[(PackageName, Option[Bindable])] =
      argFromParser(
        (PackageName.parser ~ (P.string("::") *> Identifier.bindableParser).?),
        "valueIdent",
        "package or package::name",
        "Must be a package name with an optional :: value, e.g. Foo/Bar or Foo/Bar::baz."
      )

    val mainIdentifierOpt: Opts[MainIdentifier] =
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

    val testIdentifiersOpt: Opts[List[MainIdentifier]] =
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

    val outputPathOpt: Opts[Path] =
      Opts.option[Path]("output", help = "output path")
    val interfaceOutputPathOpt: Opts[Path] =
      Opts.option[Path]("interface_out", help = "interface output path")

    private val versionCommand: Opts[MainCommand] = {
      val gitOpt =
        BuildInfo.gitHeadCommit match {
          case Some(gitVer) =>
            // We can see at build time if the git version is set, if it isn't set
            // don't create the option (in practice this will almost never happen)
            Opts
              .flag(
                "git",
                help =
                  s"use the git-sha ($gitVer) the compiler was built at.",
                "g"
              )
              .orFalse
              .map(if (_) Some(gitVer) else None)
          case None =>
            Opts(None)
        }

      Opts.subcommand("version", "print to stdout the version of the tool")(
        (
          gitOpt,
          Opts
            .option[Path](
              "output",
              "file to write to, if not set, use stdout.",
              "o"
            )
            .orNone
        )
          .mapN { (useGit, outPath) =>
            val vStr = useGit match {
              case Some(v) => v
              case None    => BuildInfo.version
            }

            val out =
              moduleIOMonad.pure(Output.Basic(Doc.text(vStr), outPath))
            FromOutput("version", out)
          }
      )
    }

    def opts: Opts[MainCommand] =
      Opts
        .subcommand("lib", "tools for working with bosatsu libraries")(
          library.Command
            .opts(platformIO)
            .map(FromOutput("lib", _))
        )
        .orElse {
          Opts.subcommand(
            "tool",
            "lower-level file-based commands for build tool integration"
          )(
            tool_command.ToolCommand
              .opts(MainModule.this)
          )
        }
        .orElse(versionCommand)
        .orElse {
          Opts.subcommand(
            "c-runtime",
            "tools for installing the bosatsu c runtime"
          )(
            cruntime.Command
              .opts(platformIO)
              .map(FromOutput("c-runtime", _))
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

  final def reportOutput(out: Output[Path]): IO[ExitCode] =
    out match {
      case Output.TestOutput(resMap, color) =>
        val hasMissing = resMap.exists(_._2.isEmpty)
        // it would be nice to run in parallel, but
        // MatchlessToValue is not currently threadsafe
        val evalTest = resMap.map {
          case (p, Some(evalTest)) =>
            (p, Some(evalTest.value))
          case (p, None) => (p, None)
        }

        val testReport = Test.outputFor(evalTest, color)
        val success = !hasMissing && (testReport.fails == 0)
        val code = if (success) ExitCode.Success else ExitCode.Error
        println(testReport.doc.render(80)).as(code)
      case Output.EvaluationResult(_, tpe, resDoc) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc =
          resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        println(doc.render(100)).as(ExitCode.Success)
      case Output.JsonOutput(json, pathOpt) =>
        writeOut(json.toDoc, pathOpt)
          .as(ExitCode.Success)

      case Output.TranspileOut(outs) =>
        outs.toList
          .map { case (p, d) =>
            (p, platformIO.writeDoc(p, d))
          }
          .sortBy(_._1)(using platformIO.pathOrdering)
          .traverse_ { case (_, w) => w }
          .as(ExitCode.Success)

      case Output.CompileOut(packList, ifout, output) =>
        val ifres = ifout match {
          case None            => moduleIOMonad.unit
          case Some(ifacePath) =>
            val ifs = packList.map(Package.interfaceOf(_))
            writeInterfaces(ifs, ifacePath)
        }
        val out = output.fold(moduleIOMonad.unit)(writePackages(packList, _))

        (ifres *> out).as(ExitCode.Success)

      case Output.ShowOutput(packs, ifaces, output) =>
        val pdocs = packs.map { pack =>
          Document[Package.Typed[Any]].document(pack)
        }
        val idocs = ifaces.map { iface =>
          Document[Package.Interface].document(iface)
        }

        val doc = Doc.intercalate(Doc.hardLine, idocs ::: pdocs)
        writeOut(doc, output).as(ExitCode.Success)

      case Output.DepsOutput(depinfo, output, style) =>
        style match {
          case GraphOutput.Json =>
            def toJson(
                dep: (Path, PackageName, FileKind, List[PackageName])
            ): Json =
              Json.JObject(
                ("path", Json.JString(dep._1.toString())) ::
                  ("package", Json.JString(dep._2.asString)) ::
                  ("kind", Json.JString(dep._3.name)) ::
                  (
                    "dependsOn",
                    Json.JArray(
                      dep._4.map(pn => Json.JString(pn.asString)).toVector
                    )
                  ) ::
                  Nil
              )

            val tupleOrdering = Ordering.Tuple3(using
              platformIO.pathOrdering,
              Ordering[PackageName],
              Ordering.String
            )
            val asJson = Json.JArray(
              depinfo
                .sortBy { case (path, pn, fk, _) => (path, pn, fk.name) }(using
                  tupleOrdering
                )
                .map(toJson)
                .toVector
            )

            writeOut(asJson.toDoc, output).as(ExitCode.Success)
          case GraphOutput.Dot =>
            def shapeOf(fk: FileKind): String =
              fk match {
                case FileKind.Iface  => "diamond"
                case FileKind.Pack   => "box"
                case FileKind.Source => "circle"
              }

            type Ident = String
            def makeNode(
                idx: Int,
                dep: (Path, PackageName, FileKind, List[PackageName])
            ): (Ident, String) = {
              // C [shape=box, fillcolor=lightgrey, label="Node C"];
              val ident = s"N$idx"
              val decl =
                s"$ident [shape=${shapeOf(dep._3)}, label=\"${dep._2.asString}\"];"
              (ident, decl)
            }
            def makeMissing(idx: Int, pack: PackageName): (Ident, String) = {
              // C [shape=box, fillcolor=lightgrey, label="Node C"];
              val ident = s"N$idx"
              val decl = s"$ident [shape=octogon, label=\"${pack.asString}\"];"
              (ident, decl)
            }

            val knownPacks = depinfo.map(_._2).toSet
            val allPacks =
              depinfo.flatMap(dep => dep._2 :: dep._4).distinct.sorted
            val unknownPacks = allPacks.filterNot(knownPacks)
            type NodeMap = Map[PackageName, NonEmptyList[
              (Int, Option[FileKind], String, String)
            ]]
            val depinfoSize = depinfo.size
            val nodes: NodeMap =
              (depinfo.zipWithIndex.map { case (dep, idx) =>
                val (ident, nstr) = makeNode(idx, dep)
                (dep._2, (idx, Some(dep._3), ident, nstr))
              } ::: unknownPacks.mapWithIndex { (pn, idx0) =>
                val idx = depinfoSize + idx0
                val (ident, nstr) = makeMissing(idx, pn)
                (pn, (idx, None, ident, nstr))
              })
                .groupByNel(_._1)
                .map { case (k, v) =>
                  (k, v.map(_._2))
                }

            // now NodeMap has everything
            def makeEdge(
                src: PackageName,
                k: FileKind,
                dst: PackageName,
                nm: NodeMap
            ): String = {
              implicit val orderKind: cats.Order[Option[FileKind]] =
                new cats.Order[Option[FileKind]] {
                  def compare(a: Option[FileKind], b: Option[FileKind]) =
                    (a, b) match {
                      case (None, None)                                   => 0
                      case (Some(_), None)                                => -1
                      case (None, Some(_))                                => 1
                      case (Some(FileKind.Iface), Some(FileKind.Iface))   => 0
                      case (Some(FileKind.Iface), Some(_))                => -1
                      case (Some(FileKind.Pack), Some(FileKind.Iface))    => 1
                      case (Some(FileKind.Pack), Some(FileKind.Pack))     => 0
                      case (Some(FileKind.Pack), Some(FileKind.Source))   => -1
                      case (Some(FileKind.Source), Some(FileKind.Source)) => 0
                      case (Some(FileKind.Source), Some(_))               => 1
                    }
                }

              val srcNode = nm(src).find { case (_, sk, _, _) =>
                sk == Some(k)
              }.get
              val dstNode = nm(dst).sortBy(rec => (rec._2, rec._1)).head
              s"${srcNode._3} -> ${dstNode._3};"
            }

            val header = Doc.text("digraph G {")
            val allNodes: List[Doc] = nodes.iterator
              .flatMap { case (_, ns) =>
                ns.map(rec => (rec._1, Doc.text(rec._4))).toList
              }
              .toList
              .sortBy(_._1)
              .map(_._2)
            val nodesDoc = Doc.intercalate(Doc.hardLine, allNodes)
            val edges: List[Doc] =
              depinfo.flatMap { case (_, pn, k, deps) =>
                deps.map { dep =>
                  Doc.text(makeEdge(pn, k, dep, nodes))
                }
              }
            val edgesDoc = Doc.intercalate(Doc.hardLine, edges)

            val fullDoc =
              header + (Doc.hardLine + nodesDoc + Doc.hardLine + edgesDoc)
                .nested(2) +
                Doc.hardLine + Doc.char('}')

            writeOut(fullDoc, output).as(ExitCode.Success)
        }

      case Output.Basic(doc, out) =>
        writeOut(doc, out).as(ExitCode.Success)

      case Output.Library(lib, path) =>
        writeLibrary(lib, path).as(ExitCode.Success)
      case Output.Many(items) =>
        items.foldM[IO, ExitCode](ExitCode.Success) {
          case (ExitCode.Success, item) => reportOutput(item)
          case (err, _)                 => moduleIOMonad.pure(err)
        }
    }

  private def stackTraceToString(t: Throwable): String = {
    val stringWriter = new java.io.StringWriter()
    val printWriter = new java.io.PrintWriter(stringWriter)
    t.printStackTrace(printWriter)
    stringWriter.toString
  }

  def reportException(ex: Throwable): IO[ExitCode] =
    ex match {
      case ce: CliException => ce.report(platformIO)
      case _                =>
        platformIO.errorln("unknown error:\n") *>
          platformIO
            .errorln(stackTraceToString(ex))
            .as(ExitCode.Error)
    }

  def report(io: IO[Output[Path]]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out)
      case Left(err)  => reportException(err)
    }
}
