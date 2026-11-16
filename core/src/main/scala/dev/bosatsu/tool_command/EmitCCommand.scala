package dev.bosatsu.tool_command

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{Identifier, Kind, Package, PackageMap, PackageName, Par, PlatformIO, Region, TypeName}
import dev.bosatsu.codegen.CompilationSource
import dev.bosatsu.codegen.clang.{ClangGen, ClangTranspiler}
import dev.bosatsu.rankn.Type
import dev.bosatsu.tool.{CliException, LocalRuntime, LocalRuntimeManifest, Output}
import org.typelevel.paiges.Doc
import scala.collection.immutable.{SortedMap, SortedSet}

object EmitCCommand {
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
            headers = headerPaths.map(path => OutputPathChecks.fileName(platform, path) -> path)
            scope <- LocalRuntime.load(platform, manifest)
            _ <- OutputPathChecks.checkOutputFiles(platform, inputs.toList, outputs)
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
