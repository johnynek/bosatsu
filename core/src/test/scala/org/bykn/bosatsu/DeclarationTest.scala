package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import Identifier.Bindable

import Parser.unsafeParse
import org.scalatest.funsuite.AnyFunSuite

class DeclarationTest extends AnyFunSuite {

  import Generators.shrinkDecl

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 200 else 20
    )
  // PropertyCheckConfiguration(minSuccessful = 50)

  implicit val emptyRegion: Region = Region(0, 0)

  val genDecl = Generators.genDeclaration(depth = 4)

  lazy val genNonFree: Gen[Declaration.NonBinding] =
    genDecl.flatMap {
      case decl: Declaration.NonBinding if decl.freeVars.isEmpty =>
        Gen.const(decl)
      case _ => genNonFree
    }

  test("freeVarsSet is a subset of allVars") {
    forAll(genDecl) { decl =>
      val frees = decl.freeVars
      val av = decl.allNames
      val missing = frees -- av
      assert(
        missing.isEmpty,
        s"expression:\n\n${decl}\n\nallVars: $av\n\nfrees: $frees"
      )
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

              assert(
                !d0sub.freeVars.contains(b),
                s"subs:\n\n$d0Str\n\n===============\n\n$d1Str===============\n\n$dSubStr"
              )
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
            case b             => Gen.const(b)
          }

        notFree.map((decl, _))
      }

    def law(b: Bindable, d1: Declaration.NonBinding, d0: Declaration) = {
      val frees = d0.freeVars

      // shrinking can cause us to call this law for free variables
      if (frees(b)) assert(true)
      else {
        val subsD0 = Declaration.substitute(b, d1, d0)
        if (subsD0 == Some(d0)) assert(true)
        else {
          subsD0 match {
            case None => assert(false, "substitute failed")
            case Some(sub) =>
              val left = sub.toDoc.render(80)
              val right = d0.toDoc.render(80)

              // there must be some diff
              val diffPos =
                left.iterator
                  .zip(right.iterator)
                  .zipWithIndex
                  .dropWhile { case ((a, b), _) => a == b }
                  .next()
                  ._2

              val line = ("=" * 80) + "\n\n"
              val leftAt = left.drop(diffPos).take(50)
              val rightAt = right.drop(diffPos).take(50)
              val diff = s"offset: $diffPos$line$leftAt\n\n$line$rightAt"
              val msg =
                s"left$line${left}\n\nright$line$right\n\ndiff$line$diff"
              assert(false, msg)
          }
        }
      }
    }

    forAll(genDNF, genNonFree) { case ((d0, b), d1) =>
      law(b, d1, d0)
    }

    val regressions: List[(Bindable, Declaration.NonBinding, Declaration)] =
      List(
        {
          import Declaration._
          import Identifier.{Name, Constructor, Backticked}
          import OptIndent._

          val b = Identifier.Backticked("")
          val d1 = Literal(Lit.fromInt(0))
          val d0 = DefFn(
            DefStatement(
              Name("mfLjwok"),
              None,
              NonEmptyList.of(Pattern.Var(Name("foo"))),
              None,
              (
                NotSameLine(Padding(10, Indented(10, Var(Backticked(""))))),
                Padding(
                  10,
                  Binding(
                    BindingStatement(
                      Pattern.Var(Backticked("")),
                      Var(Constructor("Rgt")),
                      Padding(
                        1,
                        DefFn(
                          DefStatement(
                            Backticked(""),
                            None,
                            NonEmptyList.of(Pattern.Var(Name("bar"))),
                            None,
                            (
                              NotSameLine(
                                Padding(
                                  2,
                                  Indented(4, Literal(Lit.fromInt(42)))
                                )
                              ),
                              Padding(
                                2,
                                DefFn(
                                  DefStatement(
                                    Name("gkxAckqpatu"),
                                    None,
                                    NonEmptyList.of(Pattern.Var(Name("quux"))),
                                    Some(
                                      TypeRef.TypeName(
                                        TypeName(Constructor("Y"))
                                      )
                                    ),
                                    (
                                      NotSameLine(
                                        Padding(
                                          6,
                                          Indented(8, Literal(Lit("oimsu")))
                                        )
                                      ),
                                      Padding(2, Var(Name("j")))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          (b, d1, d0)
        }
      )

    regressions.foreach { case (b, d1, d0) => law(b, d1, d0) }

  }

  test("substituting a free variable with itself is identity") {
    lazy val genFrees: Gen[(Declaration, Bindable)] =
      genDecl.flatMap { decl =>
        val frees = decl.freeVars.toList
        frees match {
          case Nil      => genFrees
          case nonEmpty => Gen.oneOf(nonEmpty).map((decl, _))
        }
      }

    def law(b: Bindable, d0: Declaration) = {
      val d1 = Declaration.Var(b)
      Declaration.substitute(b, d1, d0) match {
        case None =>
          // this mean the free variable is also shadowed
          // at some point
          ()
        case Some(res) => assert(res == d0)
      }
    }

    forAll(genFrees) { case (d, b) => law(b, d) }

    val regressions: List[(String, String)] =
      List(
        ("Foo { a }", "a")
      )

    regressions.foreach { case (decl, v) =>
      val d = unsafeParse(Declaration.parser(""), decl)
      val bind = unsafeParse(Identifier.bindableParser, v)
      law(bind, d)
    }
  }

  test("test example substitutions") {
    def law(bStr: String, to: String, in: String, res: Option[String]) = {
      val d1 = unsafeParse(Declaration.parser(""), to)
      val d0 = unsafeParse(Declaration.parser(""), in)
      val resD = res.map(unsafeParse(Declaration.parser(""), _))
      val b = unsafeParse(Identifier.bindableParser, bStr)

      assert(Declaration.substitute(b, d1.toNonBinding, d0) == resD)
    }

    law(
      "b",
      "12",
      """x = b
x""",
      Some("""x = 12
x""")
    )

    law("b", "12", """[x for b in y]""", Some("""[x for b in y]"""))
    law("b", "12", """[b for z in y]""", Some("""[12 for z in y]"""))
    law("b", "12", """[b for b in b]""", Some("""[b for b in 12]"""))
    law("b", "12", """[b for b in b if b]""", Some("""[b for b in 12 if b]"""))
    law("b", "12", """Foo { b }""", Some("Foo { b: 12 }"))
  }

  test("test freeVars with explicit examples") {
    def law(decls: String, frees: List[String], all: List[String]) = {
      val binds = frees.map(unsafeParse(Identifier.bindableParser, _))
      val alls = all.map(unsafeParse(Identifier.bindableParser, _))
      val decl = unsafeParse(Declaration.parser(""), decls)

      assert(decl.freeVars.toSet == binds.toSet, "freeVars don't match")
      assert(decl.allNames.toSet == alls.toSet, "allVars don't match")
    }

    law("a", List("a"), List("a"))
    law("[a for b in c]", List("a", "c"), List("a", "b", "c"))
    law("[a for b in c if d]", List("a", "c", "d"), List("a", "b", "c", "d"))
    law("[a for b in c if b]", List("a", "c"), List("a", "b", "c"))
    law("[b for b in c if d]", List("c", "d"), List("b", "c", "d"))
    law("[b for b in c if b]", List("c"), List("b", "c"))
    law(
      "{ k: a for b in c if d}",
      List("k", "a", "c", "d"),
      List("k", "a", "b", "c", "d")
    )
    law(
      "{ k: a for b in c if b}",
      List("k", "a", "c"),
      List("k", "a", "b", "c")
    )
    law("Foo { a }", List("a"), List("a"))
    law("Foo { a: b }", List("b"), List("b"))
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
