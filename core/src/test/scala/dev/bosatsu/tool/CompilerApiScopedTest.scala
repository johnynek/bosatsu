package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.{Applicative, Monad, Parallel, Show}
import cats.Id
import cats.data.{Chain, Ior, NonEmptyList, Validated}
import dev.bosatsu.CompileOptions
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.Type
import dev.bosatsu.{
  ExportedName, Identifier, LocationMap, MemoryMain, Package, PackageError, PackageMap,
  PackageName, PackageSignatures, Par, ParTest, Referant, TypeName
}
import dev.bosatsu.library.{LibraryIdentity, Name, Version, libraryIdentityOrdering}
import scala.collection.immutable.{SortedMap, SortedSet}
import java.util.concurrent.atomic.AtomicInteger

class CompilerApiScopedTest extends munit.FunSuite with ParTest {
  private type Path = Chain[String]
  private type MemoryResult[A] = Either[Throwable, A]
  private type MemoryF[A] = MemoryMain.StateF[MemoryResult][A]

  private given Ordering[Path] = Ordering.by(_.toList.mkString("/"))
  private given Show[Path] = Show.show(_.toList.mkString("/"))
  private given Ordering[LibraryIdentity] = libraryIdentityOrdering

  private def packageName(value: String): PackageName = PackageName.parse(value).get

  private def path(value: String): Path = Chain.fromSeq(value.split("/", -1).toIndexedSeq)

  private def parsed(value: String): Package.Parsed =
    Package.parser.parseAll(value).fold(error => fail(error.toString), identity)

  private def valueInterface(name: PackageName, valueType: Type): Package.Interface =
    Package(name, Nil, List(ExportedName.Binding(
      Identifier.Name("value"), Referant.Value(valueType)
    )), ())

  private def scopedCompile(
      sources: List[Package.Parsed],
      interfacesByMember: Map[PackageName, List[Package.Interface]],
      sourceDependencies: Map[PackageName, List[PackageName]]
  ): PackageMap.ScopedCompileResult = {
    import Par.F

    val members = sources.map(_.name)
    val importsByMember = members.map { member =>
      val external = interfacesByMember.getOrElse(member, Nil).map { iface =>
        val stub: Package.Interface = Package(iface.name, Nil, Nil, ())
        iface.name -> Left(stub)
      }.toMap
      member -> external
    }.toMap
    val inference = PackageMap.ScopedInference[F](
      importsByMember,
      sourceDependencies,
      members.map(member => member -> Algo.blake3Algo.hashBytes(member.asString.getBytes("UTF-8"))).toMap,
      (member, _) => Applicative[F].pure(interfacesByMember.getOrElse(member, Nil)),
      builtinPredef = true
    )
    val sourceUnits = NonEmptyList.fromListUnsafe(sources.map { source =>
      PackageMap.SourceUnit.fromParsed[F, String](source.name.asString, LocationMap(""), source)
    })
    val result = Par.await(PackageMap.typeCheckSourcesScoped[F, String](
      sourceUnits,
      inference,
      "predef",
      CompileOptions.Default,
      InferCache.noop[F, CompileCache.GenerateKeyInput, Package.Compiled],
      InferPhases.default
    ))
    result match {
      case Ior.Right(compiled) => compiled
      case Ior.Both(errors, _) => fail(errors.toString)
      case Ior.Left(errors) => fail(errors.toString)
    }
  }

  private def valueType(compiled: Package.Compiled, valueName: String): Type =
    Package.interfaceOf(compiled).exports.collectFirst {
      case ExportedName.Binding(name, Referant.Value(valueType))
          if name == Identifier.Name(valueName) => valueType
    }.getOrElse(fail(s"missing exported value $valueName in ${compiled.name.asString}"))

  private def sourceFile(source: Package.Parsed): PackageResolver.SourceFile[Id, Path] =
    PackageResolver.SourceFile(
      path(s"${source.name.asString}.bosatsu"),
      LocationMap(""),
      source.name,
      source.imports,
      source.exports,
      source.exposes,
      CompileCache.sourceExprHash(source),
      Validated.validNel[PathParseError[Path], Package.Parsed](source)
    )

  private def sourceFiles(
      sources: Package.Parsed*
  ): NonEmptyList[PackageResolver.SourceFile[Id, Path]] =
    NonEmptyList.fromListUnsafe(sources.toList.map(sourceFile))

  private def scopeMap(
      members: List[PackageName],
      dependencies: List[PackageBindings.ScopeDependency],
      bindings: List[PackageBindings.BindingRequest[Path]]
  ): PackageBindings.ScopeMap[Path] =
    PackageBindings.ScopeMap(
      SortedSet.from(members),
      dependencies,
      SortedMap.empty[LibraryIdentity, PackageBindings.ImportedScope[Path]],
      PackageBindings.indexFromPackages(Nil, identities = Nil),
      SortedSet.empty[Path],
      SortedMap.from(bindings.map(binding => binding.key -> binding))(
        using PackageBindings.bindingKeyOrdering
      )
    )

  private def importedInputs: LibraryInspection.ImportedInputs[Path] =
    LibraryInspection.ImportedInputs(
      None,
      PackageBindings.indexFromPackages(Nil, identities = Nil),
      SortedMap.empty[LibraryIdentity, proto.Interfaces]
    )

  private def sourceBinding(
      from: PackageName,
      required: PackageName,
      provider: PackageName
  ): PackageBindings.BindingRequest[Path] = {
    val referrer = PackageBindings.Scope.Member(from)
    val owner = PackageBindings.Scope.Member(provider)
    val key = PackageBindings.BindingKey(referrer, required)
    PackageBindings.BindingRequest(key, owner, None)
  }

  test("scoped preparation derives source edges from headers before selecting interfaces") {
    val app = packageName("App")
    val sourceProvider = packageName("Provider")
    val appScope = PackageBindings.Scope.Member(app)
    val providerScope = PackageBindings.Scope.Member(sourceProvider)
    val map = scopeMap(
      List(app, sourceProvider),
      List(PackageBindings.ScopeDependency(appScope, sourceProvider, providerScope)),
      List(sourceBinding(app, sourceProvider, sourceProvider))
    )

    val prepared = CompilerApi.prepareScopedCompilation(
      sourceFiles(
        parsed("package App\nfrom Provider import value"),
        parsed("package Provider\nvalue = 1")
      ),
      map,
      importedInputs,
      Nil,
      builtinPredef = true
    ).fold(error => fail(error.toString), identity)

    assertEquals(prepared.checked.scopeMap.dependencies, map.dependencies)
    assertEquals(prepared.selection.members(app).sourceDependencies, List(sourceProvider))
    assertEquals(prepared.selection.members(sourceProvider).sourceDependencies, Nil)
  }

  test("scoped preparation does not use declared edges as source evidence") {
    val app = packageName("App")
    val provider = packageName("Provider")
    val appScope = PackageBindings.Scope.Member(app)
    val providerScope = PackageBindings.Scope.Member(provider)
    val map = scopeMap(
      List(app, provider),
      List(PackageBindings.ScopeDependency(appScope, provider, providerScope)),
      List(sourceBinding(app, provider, provider))
    )

    val result = CompilerApi.prepareScopedCompilation(
      sourceFiles(
        parsed("package App\nfrom Other import value"),
        parsed("package Provider\nvalue = 1")
      ),
      map,
      importedInputs,
      Nil,
      builtinPredef = true
    )

    assertEquals(
      result.failed.get.getMessage,
      "scope-map dependency edges do not match source-derived dependencies"
    )
  }

  test("scoped preparation reports a missing binding for a real source import") {
    val app = packageName("App")
    val provider = packageName("Provider")
    val map = scopeMap(List(app, provider), Nil, Nil)

    val result = CompilerApi.prepareScopedCompilation(
      sourceFiles(
        parsed("package App\nfrom Provider import value"),
        parsed("package Provider\nvalue = 1")
      ),
      map,
      importedInputs,
      Nil,
      builtinPredef = true
    )

    assert(
      result.failed.get.getMessage.contains("missing interface binding for Provider"),
      result.failed.get.getMessage
    )
  }

  test("scoped package resolution honors each member's selected interface") {
    val first = parsed("package Client/First\nfrom Shared import item")
    val second = parsed("package Client/Second\nfrom Shared import item")
    val sourceUser = parsed("package Client/Source\nfrom Shared import item")
    val local = parsed("package Shared\nitem = 1")
    val sourceMap = PackageMap.fromIterable(List(first, second, sourceUser, local).map { source =>
      Package(source.name, source.imports, source.exports, (), source.exposes)
    })
    val firstInterface: Package.Interface = Package(PackageName.parts("Shared"), Nil, Nil, ())
    val secondInterface: Package.Interface = firstInterface.copy(
      exposes = List(List(packageName("Expose/Second")))
    )
    val selected: Map[PackageName, Map[
      PackageName,
      Either[Package.Interface, PackageName]
    ]] = Map(
      first.name -> Map(firstInterface.name -> Left(firstInterface)),
      second.name -> Map(secondInterface.name -> Left(secondInterface)),
      sourceUser.name -> Map(local.name -> Right(local.name))
    )

    val resolved = PackageMap.resolvePackagesScoped(sourceMap, selected)
      .toOption.getOrElse(fail("expected scoped imports to resolve"))

    assertEquals(
      Package.unfix(resolved.toMap(first.name).imports.head.pack).left.toOption,
      Option(firstInterface)
    )
    assertEquals(
      Package.unfix(resolved.toMap(second.name).imports.head.pack).left.toOption,
      Option(secondInterface)
    )
    assertEquals(
      Package.unfix(resolved.toMap(sourceUser.name).imports.head.pack).toOption.map(_.name),
      Option(local.name)
    )
    assert(
      PackageMap.resolvePackagesScoped(sourceMap, selected - first.name).isInvalid,
      "a missing checked owner must not fall back to the same-named local source"
    )
  }

  test("scoped inference keeps incompatible same-named providers separate from local sources") {
    val first = parsed(
      "package Client/First\nfrom Shared import value\nexport result\nresult = value"
    )
    val second = parsed(
      "package Client/Second\nfrom Shared import value\nexport result\nresult = value"
    )
    val local = parsed("package Shared\nvalue = False")
    val firstIface = valueInterface(packageName("Shared"), Type.IntType)
    val secondIface = valueInterface(packageName("Shared"), Type.StrType)
    val compiled = scopedCompile(
      List(first, second, local),
      Map(first.name -> List(firstIface), second.name -> List(secondIface)),
      Map.empty
    ).compiled.toMap

    assertEquals(valueType(compiled(first.name), "result"), Type.IntType)
    assertEquals(valueType(compiled(second.name), "result"), Type.StrType)
  }

  test("source providers are inferred once and exposed to selected foreign APIs") {
    import Par.F

    val app = parsed(
      "package Client/App\nfrom Provider import Token\nfrom Foreign import value\n" +
        "export result\nexposes Provider\ndef keep(token: Token) -> Token:\n  token\n" +
        "result = keep(value)"
    )
    val provider = parsed("package Provider\nexport Token\ntype Token = Int")
    val foreign = packageName("Foreign")
    val providerName = provider.name
    val tokenName = TypeName(Identifier.Constructor("Token"))
    val foreignIface = valueInterface(foreign, Type.const(providerName, tokenName)).copy(
      exposes = List(List(providerName))
    )
    val foreignEntry = PackageSignatures.encode(List(foreignIface)).toOption.get.interfaces.head
    val foreignIdentity = Name("foreign") -> Version.parser.parseAll("1.0.0").toOption.get
    val foreignRoot = new PackageSignatures.Root(foreign, "foreign.sig", foreignEntry)
    val selectedForeign = PackageSignatures.SelectedInterface(
      PackageBindings.PackageOwner(PackageBindings.Scope.Library(foreignIdentity), foreign),
      PackageSignatures.Input(path("foreign.sig"), None),
      foreignEntry,
      foreignRoot
    )
    val selection = PackageSignatures.ScopedResolution(Map(
      app.name -> PackageSignatures.MemberSelection(
        app.name, List(selectedForeign), List(providerName), Nil
      ),
      providerName -> PackageSignatures.MemberSelection(providerName, Nil, Nil, Nil)
    ))
    val count = new AtomicInteger(0)
    val defaultPhases = InferPhases.default
    val phases = new InferPhases {
      val id: String = defaultPhases.id
      def dependencyInterface(pack: Package.Typed[Any]): Package.Interface =
        defaultPhases.dependencyInterface(pack)
      def finishPackage(
          pack: Package.Inferred,
          depIfaces: SortedMap[PackageName, Package.Interface],
          options: CompileOptions
      ): Package.Inferred = {
        if (pack.name == providerName) {
          count.incrementAndGet()
          ()
        }
        defaultPhases.finishPackage(pack, depIfaces, options)
      }
    }
    val stub: Package.Interface = Package(foreign, Nil, Nil, ()).copy(
      exposes = List(List(providerName))
    )
    val inference = PackageMap.ScopedInference[F](
      Map(
        app.name -> Map(foreign -> Left(stub), providerName -> Right(providerName)),
        providerName -> Map.empty
      ),
      Map(app.name -> List(providerName), providerName -> Nil),
      Map(app.name -> Algo.blake3Algo.hashBytes(Array[Byte](1)),
        providerName -> Algo.blake3Algo.hashBytes(Array[Byte](2))),
      (member, dependencies) => {
        if (member == PackageName.PredefName && !selection.members.contains(member))
          Applicative[F].pure(Nil)
        else if (member == app.name) {
          assert(dependencies.exists(_.name == providerName))
          val token = dependencies.find(_.name == providerName).toList
            .flatMap(_.exports).collectFirst {
              case ExportedName.TypeName(_, Referant.TypeAliasT(alias))
                  if alias.name.ident == Identifier.Constructor("Token") => alias.toTypeTyConst
            }.getOrElse(fail("missing source-supplied Token type"))
          assertEquals(token, Type.const(providerName, tokenName))
          Applicative[F].pure(selection.bindMember(member, dependencies, builtinPredef = true)
            .fold(error => fail(error.toString), identity))
        } else Applicative[F].pure(
          selection.bindMember(member, dependencies, builtinPredef = true)
            .fold(error => fail(error.toString), identity)
        )
      },
      builtinPredef = true
    )
    val sourceUnits = NonEmptyList.of(app, provider).map(source =>
      PackageMap.SourceUnit.fromParsed[F, String](source.name.asString, LocationMap(""), source)
    )
    val result = Par.await(PackageMap.typeCheckSourcesScoped[F, String](
      sourceUnits,
      inference,
      "predef",
      CompileOptions.Default,
      InferCache.noop[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases
    ))
    val compiled = result match {
      case Ior.Right(value) => value
      case Ior.Both(errors, _) => fail(errors.toString)
      case Ior.Left(errors) => fail(errors.toString)
    }

    assertEquals(count.get(), 1)
    assert(compiled.publicDependencies(app.name).exists(_.name == providerName), "source dependency is retained")
    assert(compiled.publicDependencies(app.name).exists(_.name == foreign), "foreign API is retained")
  }

  test("scoped source exposure cycles are diagnosed before recursive inference") {
    import Par.F

    val first = parsed("package Cycle/First\nvalue = 1")
    val second = parsed("package Cycle/Second\nvalue = 2")
    val inference = PackageMap.ScopedInference[F](
      Map(first.name -> Map.empty, second.name -> Map.empty),
      Map(first.name -> List(second.name), second.name -> List(first.name)),
      Map.empty,
      (_, _) => fail("cyclic source contexts must be rejected before interface binding"),
      builtinPredef = true
    )
    val sources = NonEmptyList.of(first, second).map(source =>
      PackageMap.SourceUnit.fromParsed[F, String](source.name.asString, LocationMap(""), source)
    )
    val result = Par.await(PackageMap.typeCheckSourcesScoped[F, String](
      sources,
      inference,
      "predef",
      CompileOptions.Default,
      InferCache.noop[F, CompileCache.GenerateKeyInput, Package.Compiled],
      InferPhases.default
    ))

    val hasCycle = result match {
      case Ior.Left(errors) => errors.toList.exists {
        case _: PackageError.CircularDependency[?, ?, ?] => true
        case _ => false
      }
      case _ => false
    }
    assert(hasCycle, "cycle is reported before recursive inference")
  }

  test("scoped source DAG in MemoryMain is independent of input order") {
    val platform = MemoryMain.memoryPlatformIO[MemoryResult]
    given Monad[MemoryF] = platform.moduleIOMonad
    given Parallel[MemoryF] = platform.parallelF
    given CanPromise[MemoryF] = platform.canPromiseF

    val first = parsed(
      "package Client/First\nfrom Z/Provider import value\nexport result\nresult = value"
    )
    val second = parsed(
      "package Client/Second\nfrom Z/Provider import value\nexport result\nresult = value"
    )
    val provider = parsed("package Z/Provider\nexport value\nvalue = 42")
    assert(provider.name.asString > first.name.asString)
    assert(provider.name.asString > second.name.asString)

    def compile(sources: List[Package.Parsed]): Map[PackageName, Package.Interface] = {
      val consumers = Set(first.name, second.name)
      val imports = sources.iterator.map { source =>
        val selected =
          if (consumers.contains(source.name)) Map(provider.name -> Right(provider.name))
          else Map.empty[PackageName, Either[Package.Interface, PackageName]]
        source.name -> selected
      }.toMap
      val inference = PackageMap.ScopedInference[MemoryF](
        imports,
        Map(
          first.name -> List(provider.name),
          second.name -> List(provider.name),
          provider.name -> Nil
        ),
        Map.empty,
        (_, _) => Applicative[MemoryF].pure(Nil),
        builtinPredef = true
      )
      val sourceUnits = NonEmptyList.fromListUnsafe(sources.map { source =>
        PackageMap.SourceUnit.fromParsed[MemoryF, String](
          source.name.asString,
          LocationMap(""),
          source
        )
      })
      val action = PackageMap.typeCheckSourcesScoped[MemoryF, String](
        sourceUnits,
        inference,
        "predef",
        CompileOptions.Default,
        InferCache.noop[MemoryF, CompileCache.GenerateKeyInput, Package.Compiled],
        InferPhases.default
      )
      action.runA(MemoryMain.State.empty) match {
        case Right(Ior.Right(result)) =>
          result.compiled.toMap.iterator.map { case (name, compiled) =>
            name -> Package.interfaceOf(compiled)
          }.toMap
        case Right(Ior.Both(errors, _)) => fail(errors.toString)
        case Right(Ior.Left(errors)) => fail(errors.toString)
        case Left(error) => fail(error.toString)
      }
    }

    val original = compile(List(first, second, provider))
    val reordered = compile(List(provider, second, first))
    assertEquals(reordered, original)
  }
}
