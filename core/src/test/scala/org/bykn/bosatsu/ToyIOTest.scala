package org.bykn.bosatsu

import cats.Eval
import cats.data.EitherT
import org.scalacheck.{Gen, Cogen, Prop}
import org.bykn.bosatsu.ToyIO.Pure
import org.bykn.bosatsu.ToyIO.Err
import org.bykn.bosatsu.ToyIO.FlatMap
import org.bykn.bosatsu.ToyIO.ApplyFix
import org.bykn.bosatsu.ToyIO.ReadEnv
import org.bykn.bosatsu.ToyIO.RecoverWith
import org.bykn.bosatsu.ToyIO.RemapEnv

class ToyIOTest extends munit.ScalaCheckSuite {

    override def scalaCheckTestParameters =
      super.scalaCheckTestParameters
        .withMinSuccessfulTests(20000)
        .withMaxDiscardRatio(10)

  def toEvalT[R, E, A](env: R, toyio: ToyIO[R, E, A]): EitherT[Eval, E, A] =
    toyio match {
      case Pure(get) => EitherT[Eval, E, A](Eval.now(Right(get)))
      case Err(get) => EitherT[Eval, E, A](Eval.now(Left(get)))
      case fm: FlatMap[r, e, a1, a2] =>
        val first = Eval.defer[Either[e, a1]](toEvalT(env, fm.init).value)
        EitherT(first.flatMap {
          case Right(a1) =>
            val fn1 = fm.fn.andThen { toyio => Eval.defer(toEvalT[r, e, a2](env, toyio).value) }
            fn1(a1)
          case Left(err) =>
            Eval.now(Left(err))
        })
      case rw: RecoverWith[r, e, e1, a] =>
        val first = Eval.defer[Either[e, a]](toEvalT(env, rw.init).value)
        EitherT(first.flatMap {
          case Right(a1) => Eval.now(Right(a1))
          case Left(err) =>
            val fn1 = rw.fn.andThen { toyio => Eval.defer(toEvalT[r, e1, a](env, toyio).value) }
            fn1(err)
        })
      case af: ApplyFix[r, e, a, b] =>
        lazy val fix: a => ToyIO[r, e, b] =
          { (a: a) => af.fixed(fix)(a) }

        EitherT(Eval.defer(toEvalT(env, fix(af.arg)).value))
      case _: ReadEnv[r] => EitherT[Eval, E, A](Eval.now(Right(env)))
      case remap: RemapEnv[r1, r2, e, a] =>
        val r2 = remap.fn(env)
        val io = remap.io
        EitherT(Eval.defer(toEvalT(r2, io).value))
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

  def genToy[R: Cogen, E: Cogen, A: Cogen: Ordering: Move](genR: Gen[R], genE: Gen[E], genA: Gen[A]): Gen[ToyIO[R, E, A]] = {
    lazy val recur = Gen.lzy(genToy[R, E, A](genR, genE, genA))
    val cogenFn: Cogen[A => ToyIO[R, E, A]] =
      Cogen(_.hashCode.toLong)

    val envToA: Gen[R => A] = Gen.function1[R, A](genA)

    lazy val genFix: Gen[(A => ToyIO[R, E, A]) => (A => ToyIO[R, E, A])] =
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
      1 -> genA.map(ToyIO.pure(_)),
      1 -> genE.map(ToyIO.raiseError(_)),
      1 -> envToA.map { fn => ToyIO.readEnv[A].remapEnv(fn) },
      5 -> Gen.zip(recur, Gen.function1[A, ToyIO[R, E, A]](recur)).map { case (io, fn) =>
        io.flatMap(fn)  
      },
      1 -> Gen.zip(recur, Gen.function1[E, ToyIO[R, E, A]](recur)).map { case (io, fn) =>
        io.recoverWith(fn)  
      },
      1 -> Gen.zip(recur, Gen.function1[R, R](genR)).map { case (io, fn) =>
        io.remapEnv(fn)  
      },
      1 -> Gen.zip(genA, genFix).map { case (a, fn) => ToyIO.fix(fn)(a) }
    )
  }

  val bytes = Gen.choose(Byte.MinValue, Byte.MaxValue)
  val bools = Gen.oneOf(true, false)

  property("evaluation of ToyIO via Eval matches E=Byte, A=Byte") {
    Prop.forAll(bytes, genToy(bytes, bytes, bytes)) { (env, toyio) =>
      assertEquals(toyio.run(env), toEvalT(env, toyio).value.value)  
    }
  }

  property("evaluation of ToyIO via Eval matches E=Bool, A=Bool") {
    Prop.forAll(bools, genToy(bools, bools, bools)) { (env, toyio) =>
      assertEquals(toyio.run(env), toEvalT(env, toyio).value.value)  
    }
  }
  
  test("loop from defer works") {
    val fn = ToyIO.loopFromDefer[Eval, Int, Int] { recur =>

      { (i: Int) =>
        if (i >= 0) recur(i - 1).map(_ + i)
        else Eval.now(0)
      }  
    }

    assertEquals(fn(100000).value, (0 to 100000).sum)
  }
}