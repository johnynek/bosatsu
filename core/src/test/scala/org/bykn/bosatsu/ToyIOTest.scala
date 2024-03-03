package org.bykn.bosatsu

import cats.Eval
import cats.data.EitherT
import org.scalacheck.{Gen, Cogen, Prop}
import org.bykn.bosatsu.ToyIO.Pure
import org.bykn.bosatsu.ToyIO.Err
import org.bykn.bosatsu.ToyIO.FlatMap
import org.bykn.bosatsu.ToyIO.ApplyFix
import org.bykn.bosatsu.ToyIO.RecoverWith

class ToyIOTest extends munit.ScalaCheckSuite {

  def toEvalT[E, A](toyio: ToyIO[E, A]): EitherT[Eval, E, A] =
    toyio match {
      case Pure(get) => EitherT[Eval, E, A](Eval.now(Right(get)))
      case Err(get) => EitherT[Eval, E, A](Eval.now(Left(get)))
      case fm: FlatMap[e, a1, a2] =>
        val first = Eval.defer[Either[e, a1]](toEvalT(fm.init).value)
        EitherT(first.flatMap {
          case Right(a1) =>
            val fn1 = fm.fn.andThen { toyio => Eval.defer(toEvalT[e, a2](toyio).value) }
            fn1(a1)
          case Left(err) =>
            Eval.now(Left(err))
        })
      case rw: RecoverWith[e, e1, a] =>
        val first = Eval.defer[Either[e, a]](toEvalT(rw.init).value)
        EitherT(first.flatMap {
          case Right(a1) => Eval.now(Right(a1))
          case Left(err) =>
            val fn1 = rw.fn.andThen { toyio => Eval.defer(toEvalT[e1, a](toyio).value) }
            fn1(err)
        })
      case af: ApplyFix[e, a, b] =>
        lazy val fix: a => ToyIO[e, b] =
          { (a: a) => af.fixed(fix)(a) }

        EitherT(Eval.defer(toEvalT(fix(af.arg)).value))
    }

  trait Move[A] {
    def notLessThan(a: A): A
    def notMoreThan(a: A): A
  }
  object Move {
    implicit val moveBoolean: Move[Boolean] = 
      new Move[Boolean] {
        def notLessThan(a: Boolean): Boolean = true
        def notMoreThan(a: Boolean): Boolean = false
      }

    implicit val moveByte: Move[Byte] = 
      new Move[Byte] {
        def notLessThan(a: Byte): Byte =
          if (a == Byte.MaxValue) Byte.MaxValue
          else (a + 1).toByte
        def notMoreThan(a: Byte): Byte =
          if (a == Byte.MinValue) Byte.MinValue
          else (a - 1).toByte
      }

    def apply[A](implicit m: Move[A]): Move[A] = m
  }

  def genToy[E: Cogen, A: Cogen: Ordering: Move](genE: Gen[E], genA: Gen[A]): Gen[ToyIO[E, A]] = {
    lazy val recur = Gen.lzy(genToy(genE, genA))
    val cogenFn: Cogen[A => ToyIO[E, A]] =
      Cogen(_.hashCode.toLong)

    lazy val genFix: Gen[(A => ToyIO[E, A]) => (A => ToyIO[E, A])] =
      Gen.zip(genA, Gen.oneOf(true, false), genA, genE).map { case (cut, lt, result, err) =>
      
        val ord = implicitly[Ordering[A]]
        val cmpFn =
          if (lt) { (a: A) => ord.lt(a, cut) }
          else { (a: A) => ord.gt(a, cut) }
        val step = 
          if (lt) { (a: A) => Move[A].notLessThan(a) }
          else { (a: A) => Move[A].notMoreThan(a) }

        { recur =>
          (a: A) => {
            if (ord.equiv(a, cut)) ToyIO.pure(result)
            else if (cmpFn(a)) recur(step(a))
            else ToyIO.raiseError(err)
          }
        }
      }
      Gen.function1(Gen.function1(recur)(Cogen[A]))(cogenFn)

    Gen.frequency(
      2 -> genA.map(ToyIO.pure(_)),
      2 -> genE.map(ToyIO.raiseError(_)),
      1 -> Gen.zip(recur, Gen.function1[A, ToyIO[E, A]](recur)).map { case (io, fn) =>
        io.flatMap(fn)  
      },
      1 -> Gen.zip(recur, Gen.function1[E, ToyIO[E, A]](recur)).map { case (io, fn) =>
        io.recoverWith(fn)  
      },
      1 -> Gen.zip(genA, genFix).map { case (a, fn) => ToyIO.fix(fn)(a) }
    )
  }

  val bytes = Gen.choose(Byte.MinValue, Byte.MaxValue)
  val bools = Gen.oneOf(true, false)

  property("evaluation of ToyIO via Eval matches E=Byte, A=Byte") {
    Prop.forAll(genToy(bytes, bytes)) { toyio =>
      assertEquals(toyio.run, toEvalT(toyio).value.value)  
    }
  }

  property("evaluation of ToyIO via Eval matches E=Bool, A=Bool") {
    Prop.forAll(genToy(bools, bools)) { toyio =>
      assertEquals(toyio.run, toEvalT(toyio).value.value)  
    }
  }
}