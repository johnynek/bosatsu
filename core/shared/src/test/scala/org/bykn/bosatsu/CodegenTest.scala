package org.bykn.bosatsu

import org.scalatest.FunSuite
import cats.data.Validated
import cats.implicits._
import org.typelevel.paiges.Doc

class CodegenTest extends FunSuite {

  def generateStr(ss: List[String]): PackageMap.Inferred = {
    val validated =
      ss.traverse(Parser.parse(Package.parser, _))
        .andThen { parsed =>
          PackageMap.resolveThenInfer(parsed)._2
        }

    validated match {
      case Validated.Valid(inf) => inf
      case other => sys.error(other.toString)
    }
  }


  test("Simple codegen") {
    val pm = generateStr(List("""
package Foo
export [x]

x = 1

y = "foo"

z = \x -> x

def w(i):
  j = i
  j
""",
"""
package Bar
import Foo [x]

z = x
"""))
    val cg = new CodeGen { }

    def packToDoc(p: Package.Inferred): Doc =
      CodeGen.run(cg.genPackage(p, Externals.empty))._1

    val doc1 = packToDoc(pm.toMap.toList(0)._2)
    val doc2 = packToDoc(pm.toMap.toList(1)._2)
    assert(doc1.render(80).nonEmpty) // TODO
    assert(doc2.render(80).nonEmpty)
  }
}
