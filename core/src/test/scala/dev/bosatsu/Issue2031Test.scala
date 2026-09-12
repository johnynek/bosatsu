package dev.bosatsu

class Issue2031Test extends munit.FunSuite with ParTest {
  private val lazyPack =
    Predef.loadFileInCompile("test_workspace/Bosatsu/Lazy.bosatsu")

  private val reproPack = """
package Repro/Issue2031

from Bosatsu/Lazy import (
  Lazy,
  lazy,
)

enum VLazyList[a: +*]:
  Empty
  Cons(head: Lazy[a], tail: Lazy[VLazyList[a]])
  Mapped[b](source: VLazyList[b], fn: b -> a)

struct LazyList[a: +*](bound: Int, list: VLazyList[a])

empty_LazyList: forall a. LazyList[a] = LazyList(0, Empty)
lazy_empty: forall a. Lazy[LazyList[a]] = lazy(() -> empty_LazyList)
"""

  test(
    "issue 2031: annotated polymorphic constructor result typechecks in lambda"
  ) {
    TestUtils.testInferred(
      List(lazyPack, reproPack),
      "Repro/Issue2031",
      { (pm, _) =>
        val pack = pm.toMap.getOrElse(
          PackageName.parts("Repro", "Issue2031"),
          fail("missing inferred package Repro/Issue2031")
        )
        assert(pack.lets.exists(_._1 == Identifier.Name("lazy_empty")))
      }
    )
  }
}
