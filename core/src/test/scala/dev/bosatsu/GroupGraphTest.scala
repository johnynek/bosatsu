package dev.bosatsu

import dev.bosatsu.tool.GroupGraph
import dev.bosatsu.tool.GroupGraph.{Edge, Member, Node, Scope}
import dev.bosatsu.tool.PackageBindings
import dev.bosatsu.tool.PackageBindings.{PackageOwner, PublicPackage, PublicProvider}
import dev.bosatsu.library.{Name, Version}
import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.util.{Success, Try}

class GroupGraphTest extends munit.FunSuite {
  private def label(group: String): String = s"//src:$group"
  private def file(group: String): String = s"out/$group.node"
  private def member(name: String): Member =
    Member(s"src/$name.bosatsu", PackageName.parts(name))

  private def group(name: String, deps: String*): Node[String] =
    Node(label(name), List(member(name)), deps.toList.map(d => Edge(label(d), file(d))))

  private def nodes(n: Node[String]*): Map[String, Node[String]] =
    n.iterator.map(g => file(g.label.stripPrefix("//src:")) -> g).toMap

  private final case class Run(result: Try[Unit], reads: List[String]) {
    def success(): Unit = assertEquals(result, Success(()))
    def error: String = result.failed.get.getMessage
  }

  private def validate(
      root: String,
      available: Map[String, Node[String]],
      arguments: Set[String],
      scope: Scope
  ): Run = {
    val reads = mutable.ListBuffer.empty[String]
    val result = GroupGraph.validate[Try, String](file(root), arguments, scope, identity) { (key, _) =>
      reads += key
      Try(available(key))
    }
    Run(result, reads.toList)
  }

  test("each manifest in a diamond is opened once, including the shared leaf") {
    val graph = nodes(group("App", "Left", "Right"), group("Left", "Common"),
      group("Right", "Common"), group("Common"))
    val run = validate("App", graph, graph.keySet, Scope.Transitive)
    run.success()
    assertEquals(run.reads, List("App", "Left", "Common", "Right").map(file))
  }

  test("local checks open exactly the root and direct nodes of a diamond") {
    val graph = nodes(group("App", "Left", "Right"), group("Left", "Common"),
      group("Right", "Common"), group("Common"))
    val direct = Set(file("Left"), file("Right"))
    val local = validate("App", graph, direct, Scope.Local)
    local.success()
    assertEquals(local.reads, List("App", "Left", "Right").map(file))

    val full = validate("App", graph, direct, Scope.Transitive)
    assert(full.error.contains("//src:App -> //src:Left -> //src:Common"), full.error)
    assert(!full.reads.contains(file("Common")))
  }

  test("local checks reject surplus declarations without reading them") {
    val graph = nodes(group("App", "Middle"), group("Middle", "End"), group("End"))
    val run = validate("App", graph, graph.keySet, Scope.Local)
    assert(run.error.contains("unreachable node manifests: out/End.node"), run.error)
    assertEquals(run.reads, List(file("App"), file("Middle")))
  }

  test("a transitive source collision reports both complete paths and the member claims") {
    val shared = "external/repo/Shared.bosatsu"
    val a = group("A").copy(members = List(Member(shared, PackageName.parts("One"))))
    val b = group("B").copy(members = List(Member(shared, PackageName.parts("Two"))))
    val graph = nodes(group("App", "Left", "Right"), group("Left", "A"), group("Right", "B"), a, b)
    val local = validate("App", graph, Set(file("Left"), file("Right")), Scope.Local)
    local.success()
    val full = validate("App", graph, graph.keySet, Scope.Transitive)
    assert(full.error.contains(s"duplicate source '$shared'"), full.error)
    assert(full.error.contains(s"//src:App -> //src:Left -> //src:A ($shared, One)"), full.error)
    assert(full.error.contains(s"//src:App -> //src:Right -> //src:B ($shared, Two)"), full.error)
  }

  test("the direct dependencies and root all participate in the local package namespace") {
    val app = group("App", "Left", "Right")
    val duplicate = PackageName.parts("Internal", "Helpers")
    val left = group("Left").copy(members = List(Member("a/internal.bosatsu", duplicate)))
    val right = group("Right").copy(members = List(Member("b/internal.bosatsu", duplicate)))
    val graph = nodes(app, left, right)
    val run = validate("App", graph, graph.keySet, Scope.Local)
    assert(run.error.contains("duplicate package Internal/Helpers"), run.error)
    assert(run.error.contains("//src:App -> //src:Left (a/internal.bosatsu"), run.error)
    assert(run.error.contains("//src:App -> //src:Right (b/internal.bosatsu"), run.error)

    val conflictWithRoot = nodes(app, group("Left").copy(members =
      List(Member("other/app.bosatsu", PackageName.parts("App")))), group("Right"))
    val rootRun = validate("App", conflictWithRoot, conflictWithRoot.keySet, Scope.Local)
    assert(rootRun.error.contains("//src:App (src/App.bosatsu"), rootRun.error)
    assert(rootRun.error.contains("//src:App -> //src:Left (other/app.bosatsu"), rootRun.error)
  }

  test("duplicate membership within the same node is a conflict") {
    val app = group("App").copy(members = List(member("Inner"), member("Inner")))
    val graph = nodes(app)
    assert(validate("App", graph, Set.empty, Scope.Local).error.contains("duplicate source"))
    val different = app.copy(members = List(member("Inner"), Member("second.bosatsu", PackageName.parts("Inner"))))
    assert(validate("App", nodes(different), Set.empty, Scope.Transitive).error.contains("duplicate package Inner"))
  }

  test("implicit Predef cannot be a loose implementation") {
    val app = group("App").copy(members = List(Member("predef.bosatsu", PackageName.PredefName)))
    val run = validate("App", nodes(app), Set.empty, Scope.Local)
    assert(run.error.contains("//src:App claims implicit Bosatsu/Predef in predef.bosatsu"), run.error)
  }

  test("the declared file must supply the expected group, even on a second visit") {
    val bad = Edge(label("Wrong"), file("Dep"))
    val app = group("App", "Dep")
    List(bad :: app.deps, app.deps :+ bad).foreach { edges =>
      val graph = nodes(app.copy(deps = edges), group("Dep"))
      val run = validate("App", graph, graph.keySet, Scope.Transitive)
      assert(run.error.contains("//src:App -> //src:Wrong"), run.error)
      assert(run.error.contains("out/Dep.node, found //src:Dep"), run.error)
      assertEquals(run.reads.count(_ == file("Dep")), 1)
    }
  }

  test("a label reached from two different manifest files cannot coalesce") {
    val second = "other/dep.node"
    val graph = nodes(group("App", "Left", "Right"), group("Left", "Dep"),
      group("Right").copy(deps = List(Edge(label("Dep"), second))), group("Dep"))
      .updated(second, group("Dep"))
    val run = validate("App", graph, graph.keySet, Scope.Transitive)
    assert(run.error.contains("two manifests"), run.error)
    assert(run.error.contains("out/Dep.node via //src:App -> //src:Left -> //src:Dep"), run.error)
    assert(run.error.contains("other/dep.node via //src:App -> //src:Right -> //src:Dep"), run.error)
  }

  test("repeated references to the same group resolve once") {
    val graph = nodes(group("App", "Dep", "Dep"), group("Dep"))
    val run = validate("App", graph, Set(file("Dep")), Scope.Transitive)
    run.success()
    assertEquals(run.reads, List(file("App"), file("Dep")))
  }

  test("missing edges cannot read an existing but undeclared file") {
    val graph = nodes(group("App", "Private"), group("Private"))
    val run = validate("App", graph, Set.empty, Scope.Transitive)
    assert(run.error.contains("missing declared --node out/Private.node"), run.error)
    assertEquals(run.reads, List(file("App")))
  }

  test("cycle detection reports a witness without re-reading active nodes") {
    val graph = nodes(group("App", "A"), group("A", "B"), group("B", "A"))
    val run = validate("App", graph, graph.keySet, Scope.Transitive)
    assert(run.error.contains("//src:App -> //src:A -> //src:B -> //src:A"), run.error)
    assertEquals(run.reads, List("App", "A", "B").map(file))

    val self = nodes(group("App", "App"))
    assert(validate("App", self, Set.empty, Scope.Local).error.contains("cyclic group dependency"))
  }

  test("the aggregate local reads and a single full scan scale linearly on long chains") {
    for (size <- List(3000, 6000)) {
      def name(i: Int) = s"G$i"
      val graph = (0 until size).iterator.map { i =>
        val g = if (i == 0) group(name(i)) else group(name(i), name(i - 1))
        file(name(i)) -> g
      }.toMap
      val localReads = (0 until size).iterator.map { i =>
        val declared = if (i == 0) Set.empty[String] else Set(file(name(i - 1)))
        val run = validate(name(i), graph, declared, Scope.Local)
        run.success()
        run.reads.size
      }.sum
      assertEquals(localReads, 2 * size - 1)
      val full = validate(name(size - 1), graph, graph.keySet, Scope.Transitive)
      full.success()
      assertEquals(full.reads.size, size)
      assertEquals(full.reads.distinct.size, size)
    }
  }

  test("shared member closure resolution retains exposure routes and permits disjoint releases") {
    def identity(name: String, version: String): PackageBindings.LibraryIdentity =
      Name(name) -> Version.parser.parseAll(version).toOption.get

    def packageName(name: String): PackageName = PackageName.parse(name).get

    val left = identity("left", "1.0.0")
    val right = identity("right", "1.0.0")
    val sharedV1 = identity("shared", "1.0.0")
    val sharedV2 = identity("shared", "2.0.0")
    val sharedPackage = packageName("Shared/Api")
    val leftPackage = PublicPackage(
      left,
      packageName("Left"),
      SortedMap.from(List(
        sharedPackage -> PublicProvider.Library(sharedV1, List(left, sharedV1))
      ))(using summon[Ordering[PackageName]])
    )
    val rightPackage = PublicPackage(
      right,
      packageName("Right"),
      SortedMap.from(List(
        sharedPackage -> PublicProvider.Library(sharedV2, List(right, sharedV2))
      ))(using summon[Ordering[PackageName]])
    )

    val publicIndex = PackageBindings.indexFromPackages(
      List(
        leftPackage,
        rightPackage,
        PublicPackage(sharedV1, sharedPackage, SortedMap.empty(using summon[Ordering[PackageName]])),
        PublicPackage(sharedV2, sharedPackage, SortedMap.empty(using summon[Ordering[PackageName]]))
      ),
      List(left, right, sharedV1, sharedV2)
    )
    val app = packageName("App")
    val appScope = PackageBindings.Scope.Member(app)
    val leftScope = PackageBindings.Scope.Library(left)
    val rightScope = PackageBindings.Scope.Library(right)
    val leftName = packageName("Left")
    val rightName = packageName("Right")
    val first = packageName("First")
    val second = packageName("Second")
    val sharedScopeOne = PackageBindings.Scope.Library(sharedV1)
    val sharedScopeTwo = PackageBindings.Scope.Library(sharedV2)
    val emptyImports = SortedMap.empty[
      PackageBindings.LibraryIdentity,
      PackageBindings.ImportedScope[String]
    ](using Ordering.Tuple2(using Name.orderingName, Version.versionOrdering))
    val emptyRequests = SortedMap.empty[
      PackageBindings.BindingKey,
      PackageBindings.BindingRequest[String]
    ](using PackageBindings.bindingKeyOrdering)
    val scopeMap = PackageBindings.ScopeMap(
      scala.collection.immutable.SortedSet.from(List(app, first, second)),
      Nil,
      emptyImports,
      publicIndex,
      scala.collection.immutable.SortedSet.empty[String],
      emptyRequests
    )
    def checkedBinding(
        referrer: PackageBindings.Scope,
        packageName: PackageName,
        provider: PackageBindings.Scope,
        witness: List[PackageBindings.Scope]
    ): (PackageBindings.BindingKey, PackageBindings.CheckedBinding[String]) = {
      val key = PackageBindings.BindingKey(referrer, packageName)
      key -> PackageBindings.CheckedBinding(
        key, PackageOwner(provider, packageName), None, witness
      )
    }
    val checkedBindings = List(
      checkedBinding(appScope, leftName, leftScope, List(appScope, leftScope)),
      checkedBinding(appScope, rightName, rightScope, List(appScope, rightScope)),
      checkedBinding(PackageBindings.Scope.Member(first), leftName, leftScope,
        List(PackageBindings.Scope.Member(first), leftScope)),
      checkedBinding(PackageBindings.Scope.Member(second), rightName, rightScope,
        List(PackageBindings.Scope.Member(second), rightScope)),
      checkedBinding(leftScope, sharedPackage, sharedScopeOne, List(leftScope, sharedScopeOne)),
      checkedBinding(rightScope, sharedPackage, sharedScopeTwo, List(rightScope, sharedScopeTwo))
    )
    val checked = PackageBindings.CheckedScopeMap(
      scopeMap,
      publicIndex,
      SortedMap.from(checkedBindings)(using PackageBindings.bindingKeyOrdering)
    )

    val conflict = PackageBindings.resolveMemberClosures(
      List(app -> List(leftName, rightName)), checked, Map.empty, builtinPredef = true
    ).left.toOption.get
    assert(conflict.contains("two releases of library shared"), conflict)
    assert(conflict.contains("library left@1.0.0 -> library shared@1.0.0"), conflict)
    assert(conflict.contains("library right@1.0.0 -> library shared@2.0.0"), conflict)

    val disjoint = PackageBindings.resolveMemberClosures(
      List(
        first -> List(leftName),
        second -> List(rightName)
      ),
      checked,
      Map.empty,
      builtinPredef = true
    )
    assert(disjoint.isRight, disjoint.left.toOption.getOrElse(""))

    val wrapper = packageName("Wrapper")
    val facade = packageName("Facade")
    val apiOne = packageName("Names/One")
    val apiTwo = packageName("Names/Two")
    val facadeLibrary = identity("facade", "1.0.0")
    val facadeScope = PackageBindings.Scope.Library(facadeLibrary)
    val sourceIndex = PackageBindings.indexFromPackages(List(
      PublicPackage(
        facadeLibrary,
        facade,
        SortedMap.from(List(
          apiTwo -> PublicProvider.Library(sharedV2, List(facadeLibrary, sharedV2))
        ))(using summon[Ordering[PackageName]])
      ),
      PublicPackage(sharedV1, apiOne, SortedMap.empty(using summon[Ordering[PackageName]])),
      PublicPackage(sharedV2, apiTwo, SortedMap.empty(using summon[Ordering[PackageName]]))
    ))
    val wrapperScope = PackageBindings.Scope.Member(wrapper)
    val sourceBindings = List(
      checkedBinding(appScope, wrapper, wrapperScope, List(appScope, wrapperScope)),
      checkedBinding(appScope, facade, facadeScope, List(appScope, facadeScope)),
      checkedBinding(wrapperScope, apiOne, sharedScopeOne, List(wrapperScope, sharedScopeOne)),
      checkedBinding(facadeScope, apiTwo, sharedScopeTwo, List(facadeScope, sharedScopeTwo))
    )
    val sourceScopeMap = scopeMap.copy(
      members = scala.collection.immutable.SortedSet.from(List(app, wrapper)),
      publicIndex = sourceIndex
    )
    val sourceChecked = PackageBindings.CheckedScopeMap(
      sourceScopeMap,
      sourceIndex,
      SortedMap.from(sourceBindings)(using PackageBindings.bindingKeyOrdering)
    )
    val sourceConflict = PackageBindings.resolveMemberClosures(
      List(app -> List(wrapper, facade)),
      sourceChecked,
      Map(app -> Some(Nil), wrapper -> Some(List(apiOne))),
      builtinPredef = true
    ).left.toOption.get
    assert(sourceConflict.contains("conflicting visible releases for shared"), sourceConflict)
    assert(sourceConflict.contains("member App -> member Wrapper -> library shared@1.0.0"), sourceConflict)
    assert(sourceConflict.contains("member App -> library facade@1.0.0 -> library shared@2.0.0"), sourceConflict)
  }
}
