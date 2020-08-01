package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }

class DeclarationTest extends FunSuite {

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  val genDecl = Generators.genDeclaration(depth = 4)

  test("freeVarsSet is a subset of allVars") {
    forAll(genDecl) { decl =>
      val frees = decl.freeVars
      val av = decl.allNames
      val missing = frees -- av
      assert(missing.isEmpty, s"expression:\n\n${decl}\n\nallVars: $av\n\nfrees: $frees")
    }
  }

  test("after substitution, a variable is no longer free") {
    lazy val genNonFree: Gen[Declaration.NonBinding] =
     genDecl.flatMap {
       case decl: Declaration.NonBinding if decl.freeVars.isEmpty => Gen.const(decl)
       case _ => genNonFree
     }

    forAll(genDecl, genNonFree) { (d0, d1) =>
      d0.freeVars.toList match {
        case Nil => ()
        case b :: _ =>
          Declaration.substitute(b, d1, d0) match {
            case None =>
              // te1 has no free variables, this shouldn't fail
              assert(false)

            case Some(d0sub) =>
              val d0Str = d0.toDoc.render(80)
              val d1Str = d1.toDoc.render(80)
              val dSubStr = d0sub.toDoc.render(80)

              assert(!d0sub.freeVars.contains(b),
                s"subs:\n\n$d0Str\n\n===============\n\n$d1Str===============\n\n$dSubStr")
          }
      }
    }
  }
}
