package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite
import java.nio.file.{Path, Paths}

import scala.collection.JavaConverters._

class PathModuleTest extends FunSuite {

  implicit val arbPath: Arbitrary[Path] =
    Arbitrary {
      val str = Gen.identifier

      Gen.listOf(str).map { parts => Paths.get(parts.mkString("/")) }
    }

  test("no roots means no Package") {
    forAll { p: Path =>
      assert(PathModule.pathPackage(Nil, p) == None)
    }
  }

  test("empty path is not okay for a package") {
    forAll { roots: List[Path] =>
      assert(PathModule.pathPackage(roots, Paths.get("")) == None)
    }
  }

  test("if we add to a path that becomes Package") {
    forAll { (root: Path, otherRoots: List[Path], rest: Path) =>
      if (rest.toString != "") {
        val path = root.resolve(rest)
        val pack =
          PackageName.parse(rest.asScala.map(_.toString.toLowerCase.capitalize).mkString("/"))
        assert(PathModule.pathPackage(root :: otherRoots, path) == pack)
      }
    }
  }

  test("if none of the roots are prefixes we have none") {
    forAll { (r0: Path, roots0: List[Path], file: Path) =>
      val roots = (r0 :: roots0).filterNot(_.toString == "")
      val pack = PathModule.pathPackage(roots, file)

      val noPrefix = !roots.exists { r => file.asScala.toList.startsWith(r.asScala.toList) }

      if (noPrefix) assert(pack == None)
    }
  }
}
