package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import dev.bosatsu.DirectEC.directEC
import dev.bosatsu.IorMethods.IorExtension

object DebugAliasPackage {
  private def parse(s: String): Package.Parsed =
    Parser.unsafeParse(Package.parser, s)

  private def resolveThenInfer(
      ps: Iterable[Package.Parsed]
  ): Validated[NonEmptyList[PackageError], PackageMap.Compiled] = {
    implicit val showInt: Show[Int] = Show.fromToString
    PackageMap
      .resolveThenInfer(ps.toList.zipWithIndex.map(_.swap), Nil, CompileOptions.Default)
      .strictToValidated
  }

  private def runCase(
      name: String,
      expectValid: Boolean,
      programs: List[String]
  ): Unit = {
    println(s"running: $name")
    val result = resolveThenInfer(programs.map(parse))
    println(result)
    result match {
      case Validated.Valid(_) if expectValid     => println(s"passed: $name")
      case Validated.Invalid(_) if !expectValid  => println(s"passed: $name")
      case other                                 =>
        sys.error(s"unexpected result for $name: $other")
    }
  }

  def main(args: Array[String]): Unit = {
    runCase(
      "exported alias import",
      expectValid = true,
      List(
        """
package Alias/Export
export Foo, mkFoo

struct Box(item)

Foo = Box[Int]

mkFoo: Foo = Box(1)
""",
        """
package Alias/Import
from Alias/Export import Foo, mkFoo
export main

main: Foo = mkFoo
"""
      )
    )

    runCase(
      "alias collision",
      expectValid = false,
      List(
        """
package Alias/Collision

Foo = Int
struct Foo

main = 1
"""
      )
    )

    runCase(
      "alias forward ref",
      expectValid = false,
      List(
        """
package Alias/Forward

Foo = Bar
Bar = Int

main = 1
"""
      )
    )

    runCase(
      "alias self ref",
      expectValid = false,
      List(
        """
package Alias/Self

Foo = Foo

main = 1
"""
      )
    )
  }
}
