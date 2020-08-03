package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }

import Identifier.Bindable

class DeclarationTest extends FunSuite {

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  val genDecl = Generators.genDeclaration(depth = 4)
  lazy val genNonFree: Gen[Declaration.NonBinding] =
   genDecl.flatMap {
     case decl: Declaration.NonBinding if decl.freeVars.isEmpty => Gen.const(decl)
     case _ => genNonFree
   }


  test("freeVarsSet is a subset of allVars") {
    forAll(genDecl) { decl =>
      val frees = decl.freeVars
      val av = decl.allNames
      val missing = frees -- av
      assert(missing.isEmpty, s"expression:\n\n${decl}\n\nallVars: $av\n\nfrees: $frees")
    }
  }

  test("after substitution, a variable is no longer free") {
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

  test("substituting a non-free variable is identity") {
    val genDNF: Gen[(Declaration, Bindable)] =
      genDecl.flatMap { decl =>
        val frees = decl.freeVars
        lazy val notFree: Gen[Bindable] =
          Generators.bindIdentGen.flatMap {
            case b if frees(b) => notFree
            case b => Gen.const(b)
          }

        notFree.map((decl, _))
     }

    forAll(genDNF, genNonFree) { case ((d0, b), d1) =>
      assert(Declaration.substitute(b, d1, d0) == Some(d0))
    }
  }

  test("isCheap is constant under Annotation or Parens") {
    forAll(genDecl) { d =>
      val an = Declaration.Annotation(d.toNonBinding, null)(null)
      assert(an.isCheap == d.isCheap)
      val p = Declaration.Parens(d)(null)
      assert(p.isCheap == d.isCheap)
    }

    assert(Declaration.Var(Identifier.Name(""))(null).isCheap)
    assert(Declaration.Literal(Lit(""))(null).isCheap)
    assert(Declaration.Literal(Lit.fromInt(0))(null).isCheap)
  }
}
