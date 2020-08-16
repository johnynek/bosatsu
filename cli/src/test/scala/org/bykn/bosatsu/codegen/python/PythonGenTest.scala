package org.bykn.bosatsu.codegen.python

import cats.Show
import cats.data.NonEmptyList
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.{Paths, Files}
import org.bykn.bosatsu.{PackageMap, MatchlessFromTypedExpr, Parser, Package, LocationMap, PackageName}
import org.scalatest.FunSuite
import org.python.util.PythonInterpreter


import org.bykn.bosatsu.DirectEC.directEC

class PythonGenTest extends FunSuite {

  implicit val showStr = Show[String](identity)

  def compileFile(path: String): PackageMap.Typed[Any] = {
    val str = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")

    val pack = Parser.unsafeParse(Package.parser(None), str)
    val packNEL = NonEmptyList((("", LocationMap(str)), pack), Nil)
    PackageMap.typeCheckParsed(packNEL, Nil, "").right.get
  }

  def isfromString(s: String): InputStream =
    new ByteArrayInputStream(s.getBytes("UTF-8"))

  test("we can compile Nat.bosatsu") {
    val natPathBosatu: String = "test_workspace/Nat.bosatsu"

    val bosatsuPM = compileFile(natPathBosatu)
    val matchless = MatchlessFromTypedExpr.compile(bosatsuPM)

    val intr = new PythonInterpreter()

    val packMap = PythonGen.renderAll(matchless, Map.empty)
    val natDoc = packMap(PackageName.parts("Bosatsu", "Nat"))._2

    intr.execfile(isfromString(natDoc.renderTrim(80)), "nat.py")
  }

}
