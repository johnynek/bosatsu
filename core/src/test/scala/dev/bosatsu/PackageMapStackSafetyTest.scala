package dev.bosatsu

import cats.data.NonEmptyList

class PackageMapStackSafetyTest extends munit.FunSuite {
  private val importedMain: NonEmptyList[ImportedName[Unit]] =
    NonEmptyList.one(ImportedName.OriginalName(Identifier.Name("main"), ()))

  private val zafuGraph: List[(String, List[String])] = List(
    ("Zafu/Abstract/Applicative", List("Zafu/Abstract/Eq")),
    ("Zafu/Abstract/ApplicativeTests", List("Zafu/Abstract/Applicative", "Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Testing/HedgeHog")),
    ("Zafu/Abstract/Eq", List()),
    ("Zafu/Abstract/Foldable", List("Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Abstract/Applicative", "Zafu/Control/IterState")),
    ("Zafu/Abstract/Hash", List("Zafu/Abstract/Eq")),
    ("Zafu/Abstract/Instances/Predef", List("Zafu/Collection/List", "Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Monad", "Zafu/Abstract/Traverse", "Zafu/Control/IterState")),
    ("Zafu/Abstract/Monad", List("Zafu/Abstract/Eq", "Zafu/Abstract/Applicative", "Zafu/Control/IterState")),
    ("Zafu/Abstract/MonadTests", List("Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Monad", "Zafu/Control/IterState", "Zafu/Testing/HedgeHog")),
    ("Zafu/Abstract/Monoid", List("Zafu/Abstract/Eq", "Zafu/Abstract/Semigroup")),
    ("Zafu/Abstract/Ord", List("Zafu/Abstract/Eq")),
    ("Zafu/Abstract/Semigroup", List("Zafu/Abstract/Eq")),
    ("Zafu/Abstract/Traverse", List("Zafu/Abstract/Eq", "Zafu/Abstract/Monoid", "Zafu/Abstract/Applicative", "Zafu/Abstract/Foldable")),
    ("Zafu/Benchmark/Game/BinaryTrees", List("Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Game/BinaryTreesTests", List("Zafu/Benchmark/Game/BinaryTrees")),
    ("Zafu/Benchmark/Game/FannkuchRedux", List("Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Game/FannkuchReduxTests", List("Zafu/Benchmark/Game/FannkuchRedux", "Zafu/Collection/List")),
    ("Zafu/Benchmark/Game/Harness", List("Zafu/Control/Result", "Zafu/Text/Pretty", "Zafu/Cli/Args", "Zafu/Cli/Args/Types")),
    ("Zafu/Benchmark/Game/HarnessTests", List("Zafu/Control/Result", "Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Game/Mandelbrot", List("Zafu/Benchmark/Game/Harness", "Zafu/Benchmark/Game/Mandelbrot/Internal")),
    ("Zafu/Benchmark/Game/Mandelbrot/Internal", List("Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Game/MandelbrotMainTests", List("Zafu/Benchmark/Game/Mandelbrot")),
    ("Zafu/Benchmark/Game/MandelbrotTests", List("Zafu/Control/Result", "Zafu/Benchmark/Game/Harness", "Zafu/Benchmark/Game/Mandelbrot/Internal")),
    ("Zafu/Benchmark/Game/NBody", List("Zafu/Control/Result", "Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Game/SpectralNorm", List("Zafu/Control/Result", "Zafu/Benchmark/Game/Harness")),
    ("Zafu/Benchmark/Vector", List("Zafu/Collection/Vector")),
    ("Zafu/Cli/Args", List("Zafu/Collection/HashMap", "Zafu/Collection/NonEmptyChain", "Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Parse/Types", "Zafu/Text/Pretty", "Zafu/Cli/Args/Types", "Zafu/Cli/Args/Internal/Core", "Zafu/Cli/Args/Internal/Lex", "Zafu/Cli/Args/Internal/Decode", "Zafu/Cli/Args/Internal/Help")),
    ("Zafu/Cli/Args/Internal/Core", List("Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Eq", "Zafu/Abstract/Semigroup", "Zafu/Collection/HashMap", "Zafu/Collection/HashSet", "Zafu/Collection/List", "Zafu/Collection/NonEmptyChain", "Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Parse", "Zafu/Text/Parse/Error", "Zafu/Text/Parse/Types", "Zafu/Text/Pretty", "Zafu/Cli/Args/Types")),
    ("Zafu/Cli/Args/Internal/Decode", List("Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Eq", "Zafu/Collection/HashSet", "Zafu/Collection/List", "Zafu/Collection/NonEmptyList", "Zafu/Collection/NonEmptyChain", "Zafu/Control/Result", "Zafu/Text/Pretty", "Zafu/Cli/Args/Types", "Zafu/Cli/Args/Internal/Core", "Zafu/Cli/Args/Internal/Lex")),
    ("Zafu/Cli/Args/Internal/Help", List("Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Hash", "Zafu/Abstract/Eq", "Zafu/Collection/HashMap", "Zafu/Collection/HashSet", "Zafu/Collection/List", "Zafu/Collection/NonEmptyChain", "Zafu/Collection/NonEmptyList", "Zafu/Text/Pretty", "Zafu/Cli/Args/Types", "Zafu/Cli/Args/Internal/Core", "Zafu/Cli/Args/Internal/Lex")),
    ("Zafu/Cli/Args/Internal/Lex", List("Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/HashSet", "Zafu/Collection/List", "Zafu/Control/Result", "Zafu/Collection/NonEmptyList", "Zafu/Cli/Args/Types", "Zafu/Cli/Args/Internal/Core")),
    ("Zafu/Cli/Args/Types", List("Zafu/Collection/HashMap", "Zafu/Collection/HashSet", "Zafu/Collection/NonEmptyChain", "Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Pretty")),
    ("Zafu/Cli/ArgsTests", List("Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Hash", "Zafu/Collection/HashMap", "Zafu/Collection/NonEmptyList", "Zafu/Text/Pretty", "Zafu/Cli/Args", "Zafu/Cli/Args/Types")),
    ("Zafu/Collection/Chain", List("Zafu/Testing/HedgeHog", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/List", "Zafu/Control/IterState")),
    ("Zafu/Collection/Deque", List("Zafu/Testing/HedgeHog", "Zafu/Abstract/Foldable", "Zafu/Abstract/Monoid", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Eq", "Zafu/Collection/List", "Zafu/Control/IterState")),
    ("Zafu/Collection/HashMap", List("Zafu/Testing/HedgeHog", "Zafu/Collection/List", "Zafu/Abstract/Eq", "Zafu/Abstract/Hash", "Zafu/Abstract/Instances/Predef")),
    ("Zafu/Collection/HashSet", List("Zafu/Testing/HedgeHog", "Zafu/Collection/List", "Zafu/Abstract/Eq", "Zafu/Abstract/Hash", "Zafu/Abstract/Instances/Predef")),
    ("Zafu/Collection/Heap", List("Zafu/Testing/HedgeHog", "Zafu/Collection/List", "Zafu/Abstract/Foldable", "Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Monoid", "Zafu/Abstract/Instances/Predef", "Zafu/Control/IterState")),
    ("Zafu/Collection/LazyList", List("Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Control/IterState")),
    ("Zafu/Collection/LazyListTests", List("Zafu/Abstract/Applicative", "Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Traverse", "Zafu/Collection/List", "Zafu/Collection/LazyList", "Zafu/Testing/HedgeHog")),
    ("Zafu/Collection/LazyTree", List("Zafu/Abstract/Eq", "Zafu/Abstract/Applicative", "Zafu/Abstract/Monad", "Zafu/Collection/LazyList", "Zafu/Control/IterState")),
    ("Zafu/Collection/LazyTreeTests", List("Zafu/Abstract/Eq", "Zafu/Abstract/Monad", "Zafu/Collection/LazyList", "Zafu/Collection/LazyTree", "Zafu/Control/IterState", "Zafu/Testing/HedgeHog")),
    ("Zafu/Collection/List", List("Zafu/Testing/HedgeHog", "Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse")),
    ("Zafu/Collection/NonEmptyChain", List("Zafu/Collection/Chain", "Zafu/Collection/List", "Zafu/Collection/NonEmptyList", "Zafu/Abstract/Eq", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Collection/HashSet", "Zafu/Abstract/Instances/Predef", "Zafu/Control/IterState")),
    ("Zafu/Collection/NonEmptyList", List("Zafu/Collection/List", "Zafu/Abstract/Eq", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Collection/HashSet", "Zafu/Abstract/Instances/Predef", "Zafu/Control/IterState")),
    ("Zafu/Collection/Vector", List("Zafu/Testing/HedgeHog", "Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Foldable", "Zafu/Abstract/Applicative", "Zafu/Abstract/Traverse", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monoid", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/List", "Zafu/Control/IterState")),
    ("Zafu/Control/IterState", List()),
    ("Zafu/Control/IterStateTests", List("Zafu/Control/IterState", "Zafu/Testing/HedgeHog")),
    ("Zafu/Control/PartialResult", List("Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Applicative", "Zafu/Abstract/Monad", "Zafu/Abstract/Foldable", "Zafu/Abstract/Traverse", "Zafu/Control/Result", "Zafu/Control/IterState")),
    ("Zafu/Control/Result", List("Zafu/Abstract/Eq", "Zafu/Abstract/Ord", "Zafu/Abstract/Hash", "Zafu/Abstract/Applicative", "Zafu/Abstract/Semigroup", "Zafu/Abstract/Monad", "Zafu/Abstract/Foldable", "Zafu/Abstract/Traverse", "Zafu/Control/IterState")),
    ("Zafu/Testing/HedgeHog", List("Zafu/Abstract/Applicative", "Zafu/Abstract/Monad", "Zafu/Abstract/Traverse", "Zafu/Collection/LazyList", "Zafu/Collection/LazyTree", "Zafu/Control/IterState", "Zafu/Testing/HedgeHog/Range", "Zafu/Testing/HedgeHog/Seed", "Zafu/Testing/HedgeHog/Size")),
    ("Zafu/Testing/HedgeHog/Range", List("Zafu/Testing/HedgeHog/Seed", "Zafu/Testing/HedgeHog/Size")),
    ("Zafu/Testing/HedgeHog/Seed", List()),
    ("Zafu/Testing/HedgeHog/Size", List()),
    ("Zafu/Text/Parse", List("Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Parse/Error", "Zafu/Text/Parse/Types")),
    ("Zafu/Text/Parse/Error", List("Zafu/Abstract/Eq", "Zafu/Abstract/Hash", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/HashSet")),
    ("Zafu/Text/Parse/Rfc5234", List("Zafu/Control/Result", "Zafu/Text/Parse")),
    ("Zafu/Text/Parse/Types", List("Zafu/Abstract/Eq", "Zafu/Abstract/Instances/Predef", "Zafu/Collection/HashSet", "Zafu/Collection/List", "Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Parse/Error")),
    ("Zafu/Text/Pretty", List("Zafu/Abstract/Hash", "Zafu/Abstract/Instances/Predef", "Zafu/Abstract/Monoid", "Zafu/Abstract/Semigroup", "Zafu/Collection/HashSet", "Zafu/Collection/LazyList")),
    ("Zafu/Tool/Cat", List("Zafu/Control/Result", "Zafu/Text/Pretty", "Zafu/Cli/Args", "Zafu/Cli/Args/Types")),
    ("Zafu/Tool/JsonFormat", List("Zafu/Collection/NonEmptyList", "Zafu/Control/Result", "Zafu/Text/Parse", "Zafu/Text/Parse/Error", "Zafu/Text/Parse/Types", "Zafu/Text/Pretty", "Zafu/Cli/Args", "Zafu/Cli/Args/Types"))
  )

  private def packageNameFor(name: String): PackageName =
    PackageName.parse(name).getOrElse(sys.error(s"invalid package name: $name"))

  private def zafuShapedPackages: List[Package[PackageName, Unit, Unit, Unit]] =
    zafuGraph.map { case (name, deps) =>
      val imports = deps.map(dep => Import(packageNameFor(dep), importedMain))
      Package(packageNameFor(name), imports, Nil, ())
    }

  private def runResolvePackagesOnSmallStack(
      stackBytes: Long
  ): Option[Throwable] = {
    val packageMap = PackageMap.fromIterable(zafuShapedPackages)

    @volatile var failure: Option[Throwable] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            PackageMap.resolvePackages(packageMap, Nil) match {
              case cats.data.Validated.Valid(_)   => ()
              case cats.data.Validated.Invalid(e) =>
                fail(s"unexpected resolve errors: ${e.toList.mkString(", ")}")
            }
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "package-map-small-stack",
      stackBytes
    )

    thread.start()
    thread.join()
    failure
  }

  private def warmResolvePackages(): Unit =
    PackageMap.resolvePackages(PackageMap.fromIterable(zafuShapedPackages), Nil) match {
      case cats.data.Validated.Valid(_)   => ()
      case cats.data.Validated.Invalid(e) =>
        fail(s"unexpected warm-up resolve errors: ${e.toList.mkString(", ")}")
    }

  Platform.onJvm(
    test("resolvePackages should not stack overflow on a zafu-shaped package DAG") {
      val stackBytes =
        sys.props.get("repro.stackBytes").fold(64L * 1024L)(_.toLong)
      warmResolvePackages()

      runResolvePackagesOnSmallStack(stackBytes) match {
        case Some(_: StackOverflowError) =>
          fail(
            s"PackageMap.resolvePackages overflowed on the Zafu package graph (packageCount=${zafuGraph.size}, stackBytes=$stackBytes)"
          )
        case Some(other) =>
          val trace = other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          ()
      }
    }
  )
}
