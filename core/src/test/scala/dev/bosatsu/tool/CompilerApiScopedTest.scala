package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.Id
import cats.data.{Chain, NonEmptyList, Validated}
import dev.bosatsu.cache.CompileCache
import dev.bosatsu.{LocationMap, Package, PackageMap, PackageName}
import dev.bosatsu.library.{Name, Version}
import scala.collection.immutable.{SortedMap, SortedSet}

class CompilerApiScopedTest extends munit.FunSuite {
  private type Path = Chain[String]

  private given Ordering[Path] = Ordering.by(_.toList.mkString("/"))
  private given Show[Path] = Show.show(_.toList.mkString("/"))
  private given Ordering[PackageBindings.LibraryIdentity] = Ordering.Tuple2(using
    Name.orderingName,
    Version.versionOrdering
  )

  private def packageName(value: String): PackageName = PackageName.parse(value).get

  private def path(value: String): Path = Chain.fromSeq(value.split("/", -1).toIndexedSeq)

  private def parsed(value: String): Package.Parsed =
    Package.parser.parseAll(value).fold(error => fail(error.toString), identity)

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
      SortedMap.empty[PackageBindings.LibraryIdentity, PackageBindings.ImportedScope[Path]],
      PackageBindings.indexFromPackages(Nil),
      SortedSet.empty[Path],
      SortedMap.from(bindings.map(binding => binding.key -> binding))(
        using PackageBindings.bindingKeyOrdering
      )
    )

  private def importedInputs: LibraryInspection.ImportedInputs[Path] =
    LibraryInspection.ImportedInputs(
      None,
      PackageBindings.indexFromPackages(Nil),
      SortedMap.empty[PackageBindings.LibraryIdentity, proto.Interfaces]
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
}
