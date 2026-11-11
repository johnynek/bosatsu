package dev.bosatsu.tool_command

import cats.{Hash, Show}
import cats.collections.HashMap
import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{Identifier, Kind, Package, PackageMap, PackageName, Par, PlatformIO, Region, TypeName}
import dev.bosatsu.codegen.CompilationSource
import dev.bosatsu.codegen.clang.{ClangGen, ClangTranspiler}
import dev.bosatsu.rankn.Type
import dev.bosatsu.tool.{CliException, LocalRuntime, LocalRuntimeManifest, Output}
import java.util.Locale
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedSet

object EmitCCommand {
  private enum Selector[P] {
    case Tests(bundle: P)
    case Main(bundle: P, packageName: PackageName)

    def selectedBundle: P = this match {
      case Tests(bundle)   => bundle
      case Main(bundle, _) => bundle
    }
  }

  private def fileName[F[_], P](platform: PlatformIO[F, P], path: P): String =
    platform.pathToString(platform.parent(path).flatMap(platform.relativize(_, path)).getOrElse(path))

  private def checkPendingNames[F[_], P](platform: PlatformIO[F, P], paths: List[P]): F[Unit] = {
    import platform.moduleIOMonad
    given Hash[platform.FileKey] = platform.fileKeyHash
    type Location = (platform.FileKey, P, P)
    // Keys do not exist yet. Only probe names the filesystem might fold
    // together; ordinary C and header leaves need no scratch I/O. Non-ASCII
    // names can also alias through filesystem-specific Unicode normalization.
    val names = paths.map(p => fileName(platform, p) -> p)
    val candidates =
      if (names.exists(_._1.exists(_ > 127))) paths
      else names.groupMap(_._1.toUpperCase(Locale.ROOT))(_._2).valuesIterator.filter(_.size > 1).flatten.toList

    def locate(p: P): F[Location] = moduleIOMonad.tailRecM(p) { current =>
      platform.parent(current) match {
        case None => moduleIOMonad.raiseError(CliException.Basic(s"no output directory for ${platform.pathToString(p)}"))
        case Some(parent) =>
          platform.existingFileKey(parent).map {
            case Some(key) => Right((key, parent, platform.relativize(parent, p).get))
            case None => Left(parent)
          }
      }
    }

    if (candidates.lengthCompare(2) < 0) moduleIOMonad.unit
    else candidates.traverse(locate).flatMap { located =>
      val grouped = located.foldLeft(HashMap.empty[platform.FileKey, List[(P, P)]]) {
        case (groups, (key, parent, suffix)) =>
          groups.updated(key, (parent -> suffix) :: groups.getOrElse(key, Nil))
      }
      grouped.valuesIterator.filter(_.size > 1).toList.traverse_ { group =>
        // Mirror absent suffixes beneath their nearest existing directory.
        // Identical leaves in genuinely case-sensitive directories stay legal.
        platform.withTempDirectoryIn(group.head._1) { temp =>
          group.traverse_ { case (_, suffix) =>
            val marker = platform.resolve(temp, suffix)
            platform.existingFileKey(marker).flatMap {
              case Some(_) => moduleIOMonad.raiseError[Unit](CliException.Basic("multiple C outputs refer to the same file"))
              case None => platform.writeDoc(marker, Doc.empty)
            }
          }
        }
      }
    }
  }

  private def checkOutputFiles[F[_], P](
      platform: PlatformIO[F, P], inputs: SortedSet[P], outputs: List[P]
  ): F[Unit] = {
    import platform.{moduleIOMonad, pathOrdering, showPath}
    given Hash[platform.FileKey] = platform.fileKeyHash
    type Key = Either[P, platform.FileKey]
    outputs.traverse { p =>
      platform.pendingOutputPath(p).flatMap { target =>
        platform.existingFileKey(target).map(key => (key.toRight(target): Key) -> p)
      }
    }.flatMap { files =>
      // In a clean action there are no prior outputs. Only a local rebuild
      // needs to stat the inputs, once each even with many requested headers.
      val existing = files.collect { case (Right(key), path) => key -> path }
      val byKey = HashMap.fromSeq(existing)
      val pending = files.collect { case (Left(path), _) => path }
      if (byKey.size != existing.size || SortedSet.from(pending).size != pending.size)
        moduleIOMonad.raiseError(CliException.Basic("multiple C outputs refer to the same file"))
      else {
        checkPendingNames(platform, pending) *> moduleIOMonad.whenA(byKey.nonEmpty) {
          inputs.traverse_ { p =>
            platform.existingFileKey(p).flatMap(_.flatMap(byKey.get).traverse_ { out =>
              moduleIOMonad.raiseError[Unit](CliException.Basic(show"output $out refers to runtime input $p"))
            })
          }
        }
      }
    }
  }

  // The native wrappers call their unboxed field with List[String] and
  // interpret its result using the external Prog representation. A loose
  // package controls the definitions behind nominal names, so recognition
  // alone cannot authorize either native call.
  private def supportsNativeProgWrapper(
      packages: PackageMap.Compiled,
      wrapperType: Type.TyConst,
      successType: Type
  ): Boolean = {
    val owner = Type.ProgTestType.tpe.toDefined.packageName
    val progName = TypeName("Prog")
    val progType = Type.const(owner, progName)
    val argsType = Type.TyApply(Type.ListType, Type.StrType)

    def validCallback(tpe: Type): Boolean = Type.splitQuantifiers(tpe)._3 match {
      case Type.Fun(NonEmptyList(arg, Nil), result) if arg.sameAs(argsType) =>
        Type.splitQuantifiers(result)._3 match {
          case Type.TyApply(Type.TyApply(foundProg, _), foundSuccess)
              if foundProg.sameAs(progType) && foundSuccess.sameAs(successType) => true
          case _ => false
        }
      case _ => false
    }

    packages.toMap.get(owner).exists { pack =>
      pack.types.getType(owner, progName).exists { prog =>
        prog.isOpaque && prog.annotatedTypeParams.map(_._2) === List(Kind.Type.co, Kind.Type.co)
      } && pack.types.getType(wrapperType).exists { wrapper =>
        wrapper.constructors match {
          case ctor :: Nil if ctor.isSingleArg => validCallback(ctor.args.head.tpe)
          case _ => false
        }
      }
    }
  }

  private def supportsNativeProgTest(packages: PackageMap.Compiled): Boolean =
    supportsNativeProgWrapper(packages, Type.ProgTestType, Type.TestType)

  private def supportsNativeMain(packages: PackageMap.Compiled): Boolean = {
    val owner = Type.ProgTestType.tpe.toDefined.packageName
    supportsNativeProgWrapper(packages, Type.TyConst(Type.Const.Defined(owner, TypeName("Main"))), Type.IntType)
  }

  private def externalBindings[P](
      scope: LocalRuntime[P],
      gen: ClangGen[Unit]
  ): gen.ExternalResolver =
    gen.ExternalResolver.stdExternals.filter((_, pack, name) => scope.hasBinding(pack, name))

  private def finishOutput[P: Show](
      scope: LocalRuntime[P],
      headers: List[(String, P)],
      out: P,
      bindings: ClangGen[Unit]#ExternalResolver,
      rendered: Either[ClangGen.Error, Doc]
  ): Either[Throwable, Output[P]] = {
    val program = rendered.leftMap {
      case err @ ClangGen.Error.UnknownValue(_, pack, name, _)
          if scope.packages.toMap.get(pack).exists(_.externalDefs.contains(name)) =>
        CliException.Basic(show"missing native binding for $pack::${name.sourceCodeRepr}\n${err.display.render(80)}")
      case err => CliException.Basic(err.display.render(80))
    }
    for {
      code <- program
      headerDocs <- {
        lazy val stubs = bindings.generateExternalsStub
        headers.traverse { case (name, path) =>
          stubs.get(name).map(path -> _).toRight(
            CliException.Basic(show"no declared external bindings generate header $path"))
        }
      }
    } yield Output.TranspileOut((out -> code) :: headerDocs)
  }

  private def renderTests[P: Show](
      scope: LocalRuntime[P],
      selected: P,
      members: NonEmptyList[PackageName],
      headers: List[(String, P)],
      out: P
  )(using Par.EC): Either[Throwable, Output[P]] = {
    // Membership comes from the implementation file itself. The declaration
    // order inside each decoded package determines its private test entry.
    val discovered = members.toList.flatMap { pn =>
      Package.testEntry(scope.packages.toMap(pn)).sequence.map(pn -> _)
    }
    for {
      tests <- ClangTranspiler.discoveredTests(discovered)
      _ <- Either.cond(tests.nonEmpty, (), CliException.Basic(
        show"no tests found in $selected (searched ${members.toList.map(_.asString).mkString(", ")})"))
      hasProgTest = tests.exists {
        case (_, Package.TestEntry.ProgTest(_, _, _)) => true
        case _ => false
      }
      _ <- Either.cond(!hasProgTest || supportsNativeProgTest(scope.packages), (), CliException.Basic(
        "incompatible Bosatsu/Prog::ProgTest for the native test runner: expected a single " +
          "List[String] -> Prog[err, Test] field and the external Bosatsu/Prog::Prog type"))
      roots = tests.iterator.map { case (pack, entry) => (pack, entry.bindable: Identifier) }.toSet
      output <- {
        val ns = CompilationSource.packageMapSource[Region](useStandardInt64 = false).namespace(scope.packages).treeShake(roots)
        val gen = new ClangGen[Unit](ns)
        // A loose provider can use standard package and symbol names with its
        // own implementation. Only declared providers may resolve references.
        val bindings = externalBindings(scope, gen)
        finishOutput(scope, headers, out, bindings, gen.renderTests(tests, bindings))
      }
    } yield output
  }

  private def renderMain[P: Show](
      scope: LocalRuntime[P],
      selected: P,
      packageName: PackageName,
      headers: List[(String, P)],
      out: P
  )(using Par.EC): Either[Throwable, Output[P]] = {
    val members = scope.members(selected)
    val namespace = CompilationSource.packageMapSource[Region](useStandardInt64 = false).namespace(scope.packages)
    for {
      _ <- Either.cond(members.toList.contains(packageName), (), CliException.Basic(
        show"main package $packageName is not a member of selected bundle $selected"))
      (binding, tpe) <- namespace.mainValues(ClangTranspiler.validMain(_).isRight).get(packageName)
        .toRight(CliException.Basic(show"no Bosatsu/Prog::Main value found in $packageName from selected bundle $selected"))
      _ <- Either.cond(supportsNativeMain(scope.packages), (), CliException.Basic(
        "incompatible Bosatsu/Prog::Main for the native program runner: expected a single " +
          "List[String] -> Prog[err, Int] field and the external opaque Bosatsu/Prog::Prog type"))
      runner <- ClangTranspiler.validMain(tpe).leftMap(detail =>
        CliException.Basic(show"invalid main value in $packageName from selected bundle $selected: $detail"))
      output <- {
        val roots = Set((packageName, binding: Identifier))
        val ns = CompilationSource.packageMapSource[Region](useStandardInt64 = false).namespace(scope.packages).treeShake(roots)
        val gen = new ClangGen[Unit](ns)
        val bindings = externalBindings(scope, gen)
        finishOutput(scope, headers, out, bindings, gen.renderMain(packageName, binding, runner, bindings))
      }
    } yield output
  }

  private def parseSelector[P](
      testBundle: Option[P],
      mainBundle: Option[P],
      mainPackage: Option[PackageName]
  ): Either[Throwable, Selector[P]] =
    (testBundle, mainBundle, mainPackage) match {
      case (Some(_), Some(_), _) | (Some(_), _, Some(_)) =>
        Left(CliException.Basic("--test-packages cannot be combined with --main-packages or --main-package"))
      case (Some(bundle), None, None) => Right(Selector.Tests(bundle))
      case (None, Some(bundle), Some(packageName)) => Right(Selector.Main(bundle, packageName))
      case (None, Some(_), None) => Left(CliException.Basic("--main-packages requires --main-package"))
      case (None, None, Some(_)) => Left(CliException.Basic("--main-package requires --main-packages"))
      case (None, None, None) =>
        Left(CliException.Basic("provide either --test-packages or both --main-packages and --main-package"))
    }

  def opts[F[_], P](platform: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platform.{moduleIOMonad, pathArg, pathOrdering, showPath}
    Opts.subcommand("emit-c", "emit C from declared compiled packages") {
      (
        Opts.option[P]("runtime-manifest", "local runtime graph JSON"),
        Opts.option[P]("test-packages", "implementation bundle whose package tests to run").orNone,
        Opts.option[P]("main-packages", "implementation bundle containing the program main").orNone,
        Opts.option[PackageName]("main-package", "package whose last Main value to run").orNone,
        Opts.option[P]("out", "generated C translation unit"),
        Opts.options[P]("extern-header-out", "generated external declaration header (repeatable)").orEmpty
      ).mapN { (file, testBundle, mainBundle, mainPackage, out, headerPaths) =>
        platform.withEC {
          for {
            selector <- moduleIOMonad.fromEither(parseSelector(testBundle, mainBundle, mainPackage))
            manifest <- LocalRuntimeManifest.read(platform, file)
            _ <- moduleIOMonad.raiseUnless(manifest.packages.exists(pathOrdering.equiv(_, selector.selectedBundle)))(
              CliException.Basic(show"selected bundle ${selector.selectedBundle} is absent from runtime manifest $file"))
            inputs = SortedSet.from(manifest.packages.toList) + file
            outputs = out :: headerPaths
            _ <- moduleIOMonad.raiseWhen(SortedSet.from(outputs).size != outputs.size)(
              CliException.Basic("emit-c requires distinct C and header output paths"))
            _ <- outputs.find(inputs).traverse_(p =>
              moduleIOMonad.raiseError[Unit](CliException.Basic(show"output $p is also a runtime input")))
            headers = headerPaths.map(path => fileName(platform, path) -> path)
            scope <- LocalRuntime.load(platform, manifest)
            _ <- checkOutputFiles(platform, inputs, outputs)
            result <- platform.canPromiseF.compute {
              selector match {
                case Selector.Tests(bundle) => renderTests(scope, bundle, scope.members(bundle), headers, out)
                case Selector.Main(bundle, packageName) => renderMain(scope, bundle, packageName, headers, out)
              }
            }.flatMap(moduleIOMonad.fromEither)
          } yield result
        }
      }
    }
  }
}
