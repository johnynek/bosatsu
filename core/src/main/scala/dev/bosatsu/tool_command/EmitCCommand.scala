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
import scala.collection.immutable.{SortedMap, SortedSet}

object EmitCCommand {
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

  // The native Prog runners call the unboxed one-field wrapper directly and
  // pass List[String] to its callback. A loose package controls the
  // definitions behind these nominal names, so recognizing the wrapper type
  // alone cannot authorize those calls.
  private def supportsNativeProgWrapper(
      packages: PackageMap.Compiled,
      wrapperName: TypeName,
      resultType: Type
  ): Boolean = {
    val owner = PackageName.parts("Bosatsu", "Prog")
    val progName = TypeName("Prog")
    val progType = Type.const(owner, progName)
    val argsType = Type.TyApply(Type.ListType, Type.StrType)

    def validCallback(tpe: Type): Boolean = Type.splitQuantifiers(tpe)._3 match {
      case Type.Fun(NonEmptyList(arg, Nil), result) if arg.sameAs(argsType) =>
        Type.splitQuantifiers(result)._3 match {
          case Type.TyApply(Type.TyApply(`progType`, _), actual) => actual.sameAs(resultType)
          case _ => false
        }
      case _ => false
    }

    packages.toMap.get(owner).exists { pack =>
      pack.types.getType(owner, progName).exists { prog =>
        prog.isOpaque && prog.annotatedTypeParams.map(_._2) === List(Kind.Type.co, Kind.Type.co)
      } && pack.types.getType(owner, wrapperName).exists { wrapper =>
        wrapper.constructors match {
          case ctor :: Nil if ctor.isSingleArg => validCallback(ctor.args.head.tpe)
          case _ => false
        }
      }
    }
  }

  private def supportsNativeProgTest(packages: PackageMap.Compiled): Boolean =
    supportsNativeProgWrapper(packages, TypeName("ProgTest"), Type.TestType)

  private def supportsNativeProgMain(packages: PackageMap.Compiled): Boolean =
    supportsNativeProgWrapper(packages, TypeName("Main"), Type.IntType)

  private def declaredExternals[P](
      gen: ClangGen[Unit],
      scope: LocalRuntime[P]
  ) =
    gen.ExternalResolver.stdExternals.filter((_, pack, name) => scope.hasBinding(pack, name))

  private def renderOutput[P: Show](
      scope: LocalRuntime[P],
      roots: Set[(PackageName, Identifier)],
      headers: List[(String, P)],
      out: P
  )(
      render: ClangGen[Unit] => Either[ClangGen.Error, (Doc, SortedMap[String, Doc])]
  )(using Par.EC): Either[Throwable, Output[P]] = {
    val ns = CompilationSource.packageMapSrc[Region]
      .namespace(scope.packages).treeShake(roots)
    val gen = new ClangGen(ns)
    for {
      rendered <- render(gen).leftMap {
        case err @ ClangGen.Error.UnknownValue(_, pack, name, _)
            if scope.packages.toMap.get(pack).exists(_.externalDefs.contains(name)) =>
          CliException.Basic(show"missing native binding for $pack::${name.sourceCodeRepr}\n${err.display.render(80)}")
        case err => CliException.Basic(err.display.render(80))
      }
      (program, stubs) = rendered
      headerDocs <- headers.traverse { case (name, path) =>
        stubs.get(name).map(path -> _).toRight(
          CliException.Basic(show"no declared external bindings generate header $path"))
      }
    } yield Output.TranspileOut((out -> program) :: headerDocs)
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
      output <- renderOutput(scope, roots, headers, out) { gen =>
        // Resolve standard native names only when the runtime manifest declares
        // the implementation, after tree shaking and inlining have removed dead
        // references.
        val bindings = declaredExternals(gen, scope)
        gen.renderTests(tests, bindings).map(doc => doc -> bindings.generateExternalsStub)
      }
    } yield output
  }

  private def renderMain[P: Show](
      scope: LocalRuntime[P],
      members: NonEmptyList[PackageName],
      mainPackage: PackageName,
      headers: List[(String, P)],
      out: P
  )(using Par.EC): Either[Throwable, Output[P]] = {
    if (!members.toList.contains(mainPackage))
      Left(CliException.Basic(
        show"main package $mainPackage is not a member of the selected program bundle"
      ))
    else {
      val ns = CompilationSource.packageMapSrc[Region]
        .namespace(scope.packages)
      ns.mainValues(ClangTranspiler.validMain(_).isRight).get(mainPackage) match {
        case None =>
          Left(CliException.Basic(show"no valid Bosatsu/Prog::Main value found in $mainPackage"))
        case Some((bindable, tpe)) =>
          ClangTranspiler.validMain(tpe) match {
            case Left(invalid) =>
              Left(CliException.Basic(show"invalid main value in $mainPackage: $invalid"))
            case Right(_) if !supportsNativeProgMain(scope.packages) =>
              Left(CliException.Basic(
                "incompatible Bosatsu/Prog::Main for the native program runner: expected a single " +
                  "List[String] -> Prog[err, Int] field and the external Bosatsu/Prog::Prog type"
              ))
            case Right(runner) =>
              val roots = Set((mainPackage, bindable: Identifier))
              renderOutput(scope, roots, headers, out) { gen =>
                val bindings = declaredExternals(gen, scope)
                gen.renderMain(mainPackage, bindable, runner, bindings)
                  .map(doc => doc -> bindings.generateExternalsStub)
              }
          }
      }
    }
  }

  def opts[F[_], P](platform: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platform.{moduleIOMonad, pathArg, pathOrdering, showPath}
    val modeOpts = Opts
      .option[P]("test-packages", "implementation bundle whose package tests to run")
      .map(_.asLeft[(P, PackageName)])
      .orElse(
        Opts.option[P]("main-packages", "implementation bundle containing the program main package")
          .product(Opts.option[PackageName]("main-package", "package containing a Bosatsu/Prog::Main value"))
          .map(_.asRight[P])
      )
    Opts.subcommand("emit-c", "emit C from declared compiled packages") {
      (
        Opts.option[P]("runtime-manifest", "local runtime graph JSON"),
        modeOpts,
        Opts.option[P]("out", "generated C translation unit"),
        Opts.options[P]("extern-header-out", "generated external declaration header (repeatable)").orEmpty
      ).mapN { (file, mode, out, headerPaths) =>
        platform.withEC {
          val selected = mode.fold(identity, _._1)
          for {
            manifest <- LocalRuntimeManifest.read(platform, file)
            _ <- moduleIOMonad.raiseUnless(manifest.packages.exists(pathOrdering.equiv(_, selected)))(
              mode match {
                case Left(_) =>
                  CliException.Basic(show"test bundle $selected is absent from runtime manifest $file")
                case Right(_) =>
                  CliException.Basic(show"program bundle $selected is absent from runtime manifest $file")
              }
            )
            inputs = SortedSet.from(manifest.packages.toList) + file
            outputs = out :: headerPaths
            _ <- moduleIOMonad.raiseWhen(SortedSet.from(outputs).size != outputs.size)(
              CliException.Basic("emit-c requires distinct C and header output paths"))
            _ <- outputs.find(inputs).traverse_(p =>
              moduleIOMonad.raiseError[Unit](CliException.Basic(show"output $p is also a runtime input")))
            headers = headerPaths.map(path => fileName(platform, path) -> path)
            scope <- LocalRuntime.load(platform, manifest)
            _ <- checkOutputFiles(platform, inputs, outputs)
            rendered = mode match {
              case Left(selectedBundle) =>
                renderTests(scope, selectedBundle, scope.members(selectedBundle), headers, out)
              case Right((selectedBundle, main)) =>
                renderMain(scope, scope.members(selectedBundle), main, headers, out)
            }
            result <- platform.canPromiseF.compute(rendered).flatMap(moduleIOMonad.fromEither)
          } yield result
        }
      }
    }
  }
}
