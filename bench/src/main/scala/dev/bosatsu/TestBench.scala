package dev.bosatsu

import cats.Show
import cats.data.Validated
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import cats.implicits._
import IorMethods.IorExtension

@State(Scope.Thread)
class TestBench {
  // don't use threads in the benchmark which will complicate matters

  private def prepPackages(
      packages: List[String],
      mainPackS: String
  ): (PackageMap.Inferred, PackageName) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          val d = p.showContext(LocationMap.Colorize.None)
          System.err.println(d.render(100))
        }
        sys.error("failed to parse") // errs.toString)
    }

    implicit val show: Show[(String, LocationMap)] = Show.show { case (s, _) =>
      s
    }
    Par.noParallelism(PackageMap
      .resolveThenInfer(
        PackageMap.withPredefA(("predef", LocationMap("")), parsedPaths),
        Nil
      ))
      .strictToValidated match {
      case Validated.Valid(packMap) =>
        (packMap, mainPack)
      case other => sys.error(s"expected clean compilation: $other")
    }
  }

  def gauss(n: Int) = prepPackages(
    List(s"""
package Gauss

gauss$n = range($n).foldLeft(0, add)
"""),
    "Gauss"
  )

  val compiled0: (PackageMap.Inferred, PackageName) =
    gauss(10)

  @Benchmark def bench0(): Unit = {
    val c = compiled0
    val ev = Evaluation(c._1, Predef.jvmExternals)
    // run the evaluation
    val _ = ev.evaluateMain(c._2).get._1.value
    ()
  }

  val compiled1: (PackageMap.Inferred, PackageName) =
    gauss(20)

  @Benchmark def bench1(): Unit = {
    val c = compiled1
    val ev = Evaluation(c._1, Predef.jvmExternals)
    // run the evaluation
    val _ = ev.evaluateMain(c._2).get._1.value
    ()
  }

  val compiled2 =
    prepPackages(
      List("""
package Euler4

def operator >(a, b):
  match a.cmp_Int(b):
    case GT: True
    case _ : False

operator - = sub

# given a maximum value, and a function to Option[Int], return
# the maximum value of the function for inputs greater than 0
# if the starting number is <= 0, we return None
def max_of(n, fn):
  int_loop(n, None, \i, res ->
    next_i = i.sub(1)
    res1 = match fn(i):
      case None: res
      case Some(m1) as sm1:
        match res:
          case None: sm1
          case Some(m) as sm: sm1 if m1 > m else sm
    (next_i, res1))

# return the first defined value from largest to smallest
# of the given function, if it is defined
def first_of(n, fn):
  int_loop(n, None, \i, _ ->
    match fn(i):
      case None: (i - 1, None)
      case nonNone: (0, nonNone))

def digit_list(n):
  rev_list = int_loop(n, [], \n, acc ->
    this_digit = n.mod_Int(10)
    next_acc = [this_digit, *acc]
    next_n = n.div(10)
    (next_n, next_acc))
  reverse(rev_list)

def is_palindrome(lst, eq_fn):
  (res, _) = lst.foldLeft((True, reverse(lst)), \res, item ->
    match res:
      case (False, _): res
      case (_, []):
        # can't really happen, the lists are the same length
        (False, [])
      case (True, [h, *t]): (eq_fn(item, h), t))
  res

def num_is_palindrome(n):
  digits = digit_list(n)
  is_palindrome(digits, eq_Int)

def product_palindrome(n1, n2):
  prod = n1.times(n2)
  Some(prod) if num_is_palindrome(prod) else None

max_pal_opt = max_of(99, \n1 -> first_of(99, product_palindrome(n1)))

max_pal = match max_pal_opt:
  case Some(m): m
  case None: 0
"""),
      "Euler4"
    )

  @Benchmark def bench2(): Unit = {
    val c = compiled2
    val ev = Evaluation(c._1, Predef.jvmExternals)
    // run the evaluation
    val _ = ev.evaluateMain(c._2).get._1.value
    ()
  }
}
