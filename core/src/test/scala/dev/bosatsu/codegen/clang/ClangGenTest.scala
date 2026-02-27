package dev.bosatsu.codegen.clang

import cats.data.NonEmptyList
import dev.bosatsu.Generators.genValidUtf
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.{
  CompileOptions,
  Generators,
  Identifier,
  LocationMap,
  Package,
  PackageMap,
  Par,
  Parser,
  PredefImpl,
  PackageName,
  TestUtils,
  Require,
  StringUtil
}
import org.scalacheck.{Prop, Gen}

class ClangGenTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(250000)
      .withMaxDiscardRatio(10)

  private def typeCheckPackage(src: String): PackageMap.Typed[Any] = {
    val pack = Parser.unsafeParse(Package.parser(None), src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(errs => fail(errs.toList.mkString("\n")), identity)
    }
  }

  def assertPredefFns(
      fns: String*
  )(matches: String)(implicit loc: munit.Location) =
    TestUtils.checkPackageMap("""
x = 1    
""") { pm =>
      val fnSet = fns.toSet
      val predef = pm.toMap(PackageName.PredefName)
      val filtered =
        predef.filterLets(ident => fnSet(ident.sourceCodeRepr))
      val pm1 = PackageMap.fromIterable(filtered :: Nil)

      val res = Par.withEC {
        ClangGen(pm1).renderMain(
          PackageName.PredefName,
          Identifier.Name(fns.last),
          Code.Ident("run_main")
        )
      }

      res match {
        case Right(d) =>
          val rendered = d.render(80)
          assertEquals(rendered, matches)
        case Left(e) => fail(e.toString)
      }
    }

  def md5HashToHex(content: String): String = {
    val md = java.security.MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }

  test("check build_List") {
    assertPredefFns("build_List")("""#include "bosatsu_runtime.h"
#include <stdlib.h>
#include "gc.h"

BValue __bsts_t_lambda0(BValue __bsts_b_a0, BValue __bsts_b_b0) {
    return alloc_enum2(1, __bsts_b_a0, __bsts_b_b0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_build__List(BValue __bsts_b_fn0) {
    return call_fn2(__bsts_b_fn0,
        alloc_boxed_pure_fn2(__bsts_t_lambda0),
        alloc_enum0(0));
}

int main(int argc, char** argv) {
    GC_init();
    init_statics();
    atexit(free_statics);
    BValue main_value = ___bsts_g_Bosatsu_l_Predef_l_build__List();
    return run_main(main_value, argc, argv);
}""")

  }
  test("check foldr_List") {
    assertPredefFns("foldr_List")("""#include "bosatsu_runtime.h"
#include <stdlib.h>
#include "gc.h"

BValue __bsts_t_lambda__loop0(BValue __bsts_b___a0,
    BValue __bsts_b___b0,
    BValue __bsts_b_list0) {
    if (get_variant(__bsts_b_list0) == 0) {
        return __bsts_b___a0;
    }
    else {
        BValue __bsts_a_0 = get_enum_index(__bsts_b_list0, 0);
        BValue __bsts_a_1 = get_enum_index(__bsts_b_list0, 1);
        BValue __bsts_b_h0 = __bsts_a_0;
        BValue __bsts_b_t0 = __bsts_a_1;
        return call_fn2(__bsts_b___b0,
            __bsts_b_h0,
            __bsts_t_lambda__loop0(__bsts_b___a0, __bsts_b___b0, __bsts_b_t0));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldr__List(BValue __bsts_b_list0,
    BValue __bsts_b_fn0,
    BValue __bsts_b_acc0) {
    return __bsts_t_lambda__loop0(__bsts_b_acc0, __bsts_b_fn0, __bsts_b_list0);
}

int main(int argc, char** argv) {
    GC_init();
    init_statics();
    atexit(free_statics);
    BValue main_value = ___bsts_g_Bosatsu_l_Predef_l_foldr__List();
    return run_main(main_value, argc, argv);
}""")
  }

  test("direct call for local non-closure recursive lambda") {
    TestUtils.checkPackageMap("""
enum MyList:
  Empty
  More(tail: MyList)

def foldr_local(list, fn, acc):
  def loop(acc1, fn1, list1):
    recur list1:
      case Empty: acc1
      case More(t): fn1(loop(acc1, fn1, t))
  loop(acc, fn, list)

main = foldr_local
""") { pm =>
      val renderedE = Par.withEC {
        ClangGen(pm).renderMain(
          TestUtils.testPackage,
          Identifier.Name("foldr_local"),
          Code.Ident("run_main")
        )
      }
      renderedE match {
        case Left(err) =>
          fail(err.toString)
        case Right(doc) =>
          val rendered = doc.render(80)
          assert(
            "return __bsts_t_lambda__loop\\d*\\(".r
              .findFirstIn(rendered)
              .nonEmpty
          )
          assert(!rendered.contains("return call_fn3(__bsts_b_loop"))
          assert(
            !rendered.contains("alloc_boxed_pure_fn3(__bsts_t_lambda__loop")
          )
      }
    }
  }

  test("top-level unit-arg function compiles to direct C function") {
    TestUtils.checkPackageMap("""
enum Nat:
  Z
  S(prev: Nat)

def source(n):
  recur n:
    case Z: 0
    case S(prev): source(prev)

def set_in_range_ok(_):
  match source(S(Z)):
    case 0: 0
    case _: 1

main = set_in_range_ok
""") { pm =>
      val renderedE = Par.withEC {
        ClangGen(pm).renderMain(
          TestUtils.testPackage,
          Identifier.Name("set_in_range_ok"),
          Code.Ident("run_main")
        )
      }
      renderedE match {
        case Left(err) =>
          fail(err.toString)
        case Right(doc) =>
          val rendered = doc.render(80)
          assert(
            "BValue ___bsts_g_.*set__in__range__ok\\(BValue ".r
              .findFirstIn(rendered)
              .nonEmpty
          )
          assert(
            "___bsts_s_.*set__in__range__ok".r
              .findFirstIn(rendered)
              .isEmpty
          )
          assert(
            "read_or_build\\(&___bsts_s_.*set__in__range__ok".r
              .findFirstIn(rendered)
              .isEmpty
          )
      }
    }
  }

  test(
    "top-level unit-arg function remains direct when nested matches share False branches"
  ) {
    TestUtils.checkPackageMap("""
enum Nat:
  Z
  S(prev: Nat)

enum Dig:
  D0
  D1
  D2
  D3
  D4
  D8

enum L:
  E
  N(h: Dig, t: L)

enum MaybeL:
  Some(arr: L)
  None

def mk(n):
  recur n:
    case Z: Some(N(D0, N(D1, N(D2, N(D3, N(D4, E))))))
    case S(prev): mk(prev)

def set_in_range_ok(_: ()):
  match mk(S(Z)):
    case Some(arr):
      match arr:
        case N(D0, N(D8, N(D2, N(D3, N(D4, E))))): 1
        case _: 0
    case None: 0

main = set_in_range_ok
""") { pm =>
      val renderedE = Par.withEC {
        ClangGen(pm).renderMain(
          TestUtils.testPackage,
          Identifier.Name("set_in_range_ok"),
          Code.Ident("run_main")
        )
      }
      renderedE match {
        case Left(err) =>
          fail(err.toString)
        case Right(doc) =>
          val rendered = doc.render(80)
          assert(
            "BValue ___bsts_g_.*set__in__range__ok\\(BValue ".r
              .findFirstIn(rendered)
              .nonEmpty
          )
          assert(
            "___bsts_s_.*set__in__range__ok".r
              .findFirstIn(rendered)
              .isEmpty
          )
          assert(
            "read_or_build\\(&___bsts_s_.*set__in__range__ok".r
              .findFirstIn(rendered)
              .isEmpty
          )
          assert(
            "call_fn1\\(___bsts_g_.*set__in__range__ok\\(\\)".r
              .findFirstIn(rendered)
              .isEmpty
          )
      }
    }
  }

  test(
    "global helper inlining with lambda argument avoids boxed lambda call at call site"
  ) {
    val src =
      """package Euler/P6
        |
        |operator + = add
        |operator - = sub
        |operator * = mul
        |
        |def sum(fn, n):
        |  int_loop(n, 0, (i, r) ->
        |    i = i - 1
        |    (i, r + fn(i)))
        |
        |diff = n -> sum((x ->
        |  x1 = x + 1
        |  x2 = x1 * x1
        |  x * x2), n)
        |""".stripMargin
    val pm = typeCheckPackage(src)
    val renderedE = Par.withEC {
      ClangGen(pm).renderMain(
        PackageName.parts("Euler", "P6"),
        Identifier.Name("diff"),
        Code.Ident("run_main")
      )
    }

    renderedE match {
      case Left(err) =>
        fail(err.toString)
      case Right(doc) =>
        val rendered = doc.render(80)
        assert(
          rendered.contains(
            "BValue ___bsts_g_Euler_l_P6_l_diff(BValue __bsts_b_n0)"
          )
        )
        assert(
          rendered.contains(
            "___bsts_g_Bosatsu_l_Predef_l_int__loop(__bsts_b_n0,"
          )
        )
        val boxedLambda = "alloc_boxed_pure_fn2\\(__bsts_t_lambda\\d+\\)".r
        assert(boxedLambda.findFirstIn(rendered).nonEmpty)
        assert(
          !rendered.contains("call_fn2(___bsts_g_Bosatsu_l_Predef_l_int__loop")
        )
    }
  }

  test("check foldl_List and reverse_concat") {
    assertPredefFns("foldl_List", "reverse_concat")(
      """#include "bosatsu_runtime.h"
#include <stdlib.h>
#include "gc.h"

BValue ___bsts_g_Bosatsu_l_Predef_l_foldl__List(BValue __bsts_b_lst0,
    BValue __bsts_b_item0,
    BValue __bsts_b_fn0) {
    BValue __bsts_a_2;
    BValue __bsts_a_3;
    BValue __bsts_a_5;
    BValue __bsts_a_7;
    __bsts_a_5 = __bsts_b_lst0;
    __bsts_a_7 = __bsts_b_item0;
    __bsts_a_2 = alloc_enum0(1);
    _Bool __bsts_l_cond0;
    __bsts_l_cond0 = get_variant_value(__bsts_a_2) == 1;
    while (__bsts_l_cond0) {
        if (get_variant(__bsts_a_5) == 0) {
            __bsts_a_2 = alloc_enum0(0);
            __bsts_a_3 = __bsts_a_7;
        }
        else {
            BValue __bsts_a_0 = get_enum_index(__bsts_a_5, 0);
            BValue __bsts_a_1 = get_enum_index(__bsts_a_5, 1);
            BValue __bsts_b_head0 = __bsts_a_0;
            BValue __bsts_b_tail0 = __bsts_a_1;
            BValue __bsts_a_4 = __bsts_b_tail0;
            BValue __bsts_a_6 = call_fn2(__bsts_b_fn0,
                __bsts_a_7,
                __bsts_b_head0);
            __bsts_a_5 = __bsts_a_4;
            __bsts_a_7 = __bsts_a_6;
        }
        __bsts_l_cond0 = get_variant_value(__bsts_a_2) == 1;
    }
    return __bsts_a_3;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(BValue __bsts_b_front0,
    BValue __bsts_b_back0) {
    BValue __bsts_a_10;
    BValue __bsts_a_11;
    BValue __bsts_a_13;
    BValue __bsts_a_15;
    __bsts_a_13 = __bsts_b_front0;
    __bsts_a_15 = __bsts_b_back0;
    __bsts_a_10 = alloc_enum0(1);
    _Bool __bsts_l_cond1;
    __bsts_l_cond1 = get_variant_value(__bsts_a_10) == 1;
    while (__bsts_l_cond1) {
        if (get_variant(__bsts_a_13) == 0) {
            __bsts_a_10 = alloc_enum0(0);
            __bsts_a_11 = __bsts_a_15;
        }
        else {
            BValue __bsts_a_8 = get_enum_index(__bsts_a_13, 0);
            BValue __bsts_a_9 = get_enum_index(__bsts_a_13, 1);
            BValue __bsts_b_head0 = __bsts_a_8;
            BValue __bsts_b_tail0 = __bsts_a_9;
            BValue __bsts_a_12 = __bsts_b_tail0;
            BValue __bsts_a_14 = alloc_enum2(1, __bsts_b_head0, __bsts_a_15);
            __bsts_a_13 = __bsts_a_12;
            __bsts_a_15 = __bsts_a_14;
        }
        __bsts_l_cond1 = get_variant_value(__bsts_a_10) == 1;
    }
    return __bsts_a_11;
}

int main(int argc, char** argv) {
    GC_init();
    init_statics();
    atexit(free_statics);
    BValue main_value = ___bsts_g_Bosatsu_l_Predef_l_reverse__concat();
    return run_main(main_value, argc, argv);
}"""
    )
  }

  test("float literals use packed bits") {
    TestUtils.checkPackageMap("""
main = 1.5
""") { pm =>
      val renderedE = Par.withEC {
        ClangGen(pm).renderMain(
          TestUtils.testPackage,
          Identifier.Name("main"),
          Code.Ident("run_main")
        )
      }
      renderedE match {
        case Left(err) =>
          fail(err.toString)
        case Right(doc) =>
          val rendered = doc.render(80)
          assert(rendered.contains("bsts_float64_from_bits"))
      }
    }
  }

  test("float literal pattern matching uses float equality helper") {
    TestUtils.checkPackageMap("""
def is_one(x):
  match x:
    case 1.0: 1
    case _: 0

main = is_one
""") { pm =>
      val renderedE = Par.withEC {
        ClangGen(pm).renderMain(
          TestUtils.testPackage,
          Identifier.Name("main"),
          Code.Ident("run_main")
        )
      }
      renderedE match {
        case Left(err) =>
          fail(err.toString)
        case Right(doc) =>
          val rendered = doc.render(80)
          assert(rendered.contains("bsts_float64_equals"))
      }
    }
  }

  def mockCodePointFn(bytes: Array[Byte], offset: Int): Int = {
    def s(i: Int) = bytes(offset + i).toInt & 0xff

    val c = s(0);
    var code_point: Int = 0
    val remaining = bytes.length - offset
    if (c <= 0x7f) {
      // 1-byte sequence (ASCII)
      code_point = c;
    } else if ((c & 0xe0) == 0xc0) {
      // 2-byte sequence
      if (remaining < 2 || (s(1) & 0xc0) != 0x80) {
        // Invalid continuation byte
        return -1;
      }
      code_point = ((c & 0x1f) << 6) | (s(1) & 0x3f);
    } else if ((c & 0xf0) == 0xe0) {
      // 3-byte sequence
      if (remaining < 3 || (s(1) & 0xc0) != 0x80 || (s(2) & 0xc0) != 0x80) {
        // Invalid continuation bytes
        return -1;
      }
      code_point = ((c & 0x0f) << 12) | ((s(1) & 0x3f) << 6) | (s(2) & 0x3f);
    } else if ((c & 0xf8) == 0xf0) {
      // 4-byte sequence
      if (
        remaining < 4 || (s(1) & 0xc0) != 0x80 || (s(2) & 0xc0) != 0x80 || (s(
          3
        ) & 0xc0) != 0x80
      ) {
        // Invalid continuation bytes
        return -1;
      }
      code_point =
        ((c & 0x07) << 18) | ((s(1) & 0x3f) << 12) | ((s(2) & 0x3f) << 6) | (s(
          3
        ) & 0x3f);
    } else {
      // Invalid UTF-8 leading byte
      return -1;
    }

    code_point
  }

  def bsts_string_code_point_bytes(bytes: Array[Byte]) = {
    // cast to an unsigned char for the math below
    def s(o: Int) = bytes(o).toInt & 0xff
    val c = s(0)
    val remaining = bytes.length;

    if (c <= 0x7f) {
      // 1-byte sequence (ASCII)
      1;
    } else if ((c & 0xe0) == 0xc0) {
      // 2-byte sequence
      if (remaining < 1 || (s(1) & 0xc0) != 0x80) {
        // Invalid continuation byte
        sys.error("invalid")
      } else {
        2;
      }
    } else if ((c & 0xf0) == 0xe0) {
      // 3-byte sequence
      if (remaining < 2 || (s(1) & 0xc0) != 0x80 || (s(2) & 0xc0) != 0x80) {
        // Invalid continuation bytes
        sys.error("invalid")
      } else {
        3;
      }
    } else if ((c & 0xf8) == 0xf0) {
      // 4-byte sequence
      if (
        remaining < 3 || (s(1) & 0xc0) != 0x80 || (s(2) & 0xc0) != 0x80 || (s(
          3
        ) & 0xc0) != 0x80
      ) {
        // Invalid continuation bytes
        sys.error("invalid")
      } else {
        4;
      }
    } else {
      // Invalid UTF-8 leading byte
      sys.error("invalid")
    }
  }

  def utf8ByteArray(cp: Int): Array[Byte] = {
    val sb = new java.lang.StringBuilder
    sb.appendCodePoint(cp).toString.getBytes("UTF-8")
  }
  def utf8Bytes(cp: Int): Int = utf8ByteArray(cp).length

  def allCodePoints(s: String): List[Int] = {
    val bytes = s.getBytes("UTF-8")
    def loop(o: Int): List[Int] =
      if (o >= bytes.length) Nil
      else {
        val cp = mockCodePointFn(bytes, o)
        val inc = utf8Bytes(cp)
        cp :: loop(o + inc)
      }

    loop(0)
  }

  def utf8Hex(s: String): String =
    s.getBytes("UTF-8").map(byte => f"$byte%02x").mkString

  property("our implementation of codepoint matches java") {
    Prop.forAll(genValidUtf) { (str: String) =>
      val cp1 = allCodePoints(str)
      val expected = StringUtil.codePoints(str)
      assertEquals(cp1, expected)
    }
  }

  property("our codepoint size mathces java") {
    Prop.forAll(Generators.genCodePoints) { cp =>
      val utf8Size = utf8Bytes(cp)
      val implSize = bsts_string_code_point_bytes(utf8ByteArray(cp))
      assertEquals(implSize, utf8Size)
    }
  }

  val wordSize = 32
  val maxWord: BigInt = BigInt(1) << wordSize
  val maskWord: BigInt = maxWord - 1
  // This is a test of the divmod algorithm where it is easy to property check
  def divMod(l: BigInt, r: BigInt) = {

    def divModPos(l: BigInt, r: BigInt): (BigInt, BigInt) = {
      // we know that l >= d * r and l < (d + 1) * r
      // low <= d <= high
      def search(low0: BigInt, high0: BigInt): (BigInt, BigInt) = {
        // we know that d fits in a 64 bit number
        Require(low0.bitLength <= wordSize * 2)
        Require(high0.bitLength <= wordSize * 2)
        // we know that div >= 1
        Require(low0 >= 1)
        Require(high0 >= low0)

        var low = low0
        var high = high0
        var cont = true
        var result: (BigInt, BigInt) = (BigInt(0), BigInt(0))
        while (cont) {
          val mid = (high + low) >> 1
          val mod = l - mid * r
          // println(s"with l = $l, r = $r search($low, $high) gives mid = $mid, c = $c")
          if (mod >= r) {
            // mid is too small
            low = mid
          } else if (mod < 0) {
            // mid is too big
            high = mid
            Require(
              high != low,
              s"low == high but mid ($low) is isn't correct: from search($low0, $high0)"
            )
          } else {
            cont = false
            // the div result fits in 2 words
            Require(mid.bitLength <= 2 * wordSize)
            result = (mid, mod)
          }
        }

        result
      }

      // when maxWord * n > m > n
      def divModTop(l: BigInt, r: BigInt): (BigInt, BigInt) = {
        Require(l < (r << wordSize))
        Require(r < l)
        Require(r > 1)
        val nwords = {
          val rlen = r.bitLength
          val wSize = rlen / wordSize
          if (rlen % wordSize == 0) wSize else (wSize + 1)
        }
        // we keep the top most word of n and divide
        val shiftCount0 = (nwords - 1) * wordSize
        val shiftCount = if (shiftCount0 > 0) shiftCount0 else 0
        val m1 = l >> shiftCount
        val n1 = r >> shiftCount
        // at this point, we have to have a relatively small number
        Require(
          m1.bitLength <= 2 * wordSize,
          s"m1 = $m1, bitlength = ${m1.bitLength}, n1 = $n1, bitlength = ${n1.bitLength}"
        )
        Require(
          n1.bitLength <= wordSize,
          s"m1 = $m1, bitlength = ${m1.bitLength}, n1 = $n1, bitlength = ${n1.bitLength}"
        )
        // this division can be done with unsigned longs
        val divGuess = m1 / n1
        val mod = l - divGuess * r
        if (mod < 0) {
          // divGuess is too big
          search(m1 / (n1 + 1), divGuess)
        } else if (mod > r) {
          // divGuess is too small
          search(divGuess, (m1 + 1) / n1)
        } else {
          // it's good
          (divGuess, mod)
        }
      }

      val l1 = l >> wordSize
      if (l1 >= r) {
        // we can use the recursive algo:
        val (d1, m1) = divModPos(l1, r)
        // we know that d1 >= 1, because l1 >= r
        // l = l1 * b + l0
        // l1 = d1 * r + m1
        // l = (d1 * r + m1) * b + l0
        // l = d1 * r * b + (m1 * b + l0)
        val l0 = l & maskWord
        // we know that m1 * b + l0 < l
        val nextL = (m1 << wordSize) | l0
        if (nextL >= l) {
          sys.error(
            s"loop error: l = $l, r = $r, l1 = $l1, l0 = $l0, nextL = $nextL"
          )
        }
        val (md1, mm1) = divModPos(nextL, r)
        // m1 * b + l0 == md1 * r + mm1
        // l = d1 * r * b + md1 * r + mm1 = (d1 * b + md1) * r + mm1
        ((d1 << wordSize) + md1, mm1)
      } else {
        val c = l.compare(r)
        if (c > 0) {
          // r < l < b * r
          divModTop(l, r)
        } else if (c < 0) {
          // r < l
          (BigInt(0), l)
        } else {
          // l == n
          (BigInt(1), BigInt(0))
        }
      }
    }

    if (r == BigInt(0)) {
      (BigInt(0), l)
    } else if (r.abs == BigInt(1)) {
      (l * r.sign, BigInt(0))
    } else {
      val (div, mod) = divModPos(l.abs, r.abs)
      // println(s"divModPos(${l.abs}, ${r.abs}) = ($div, $mod)")
      if (mod != BigInt(0)) {
        if (l > 0) {
          if (r > 0) {
            // l = d r + m
            (div, mod)
          } else {
            // l = dr + m
            // l = (-d)(-r) + m - r + r
            // l = -(d + 1) (-r) + (m + (-r))
            (-(div + 1), mod + r)
          }
        } else {
          if (r > 0) {
            // -l = (-d - 1) r + (r - m)
            (-(div + 1), r - mod)
          } else {
            //  l = d * r + m
            // -l = d (-r) + -m
            (div, -mod)
          }
        }
      } else {
        (div * l.sign * r.sign, BigInt(0))
      }
    }
  }

  val genBigInt: Gen[BigInt] = {
    val longBig = for {
      sign <- Gen.oneOf(-1, 1)
      exp <- Gen.choose(0, 62)
      l <- Gen.choose(0L, 1L << exp)
    } yield BigInt(l) * sign

    Gen.oneOf(longBig, Gen.zip(longBig, longBig).map { case (a, b) => a * b })
  }

  def law(l: BigInt, r: BigInt)(implicit loc: munit.Location) = {
    val (d, m) = divMod(l, r)

    val d1 = PredefImpl.divBigInteger(l.underlying, r.underlying)
    val m1 = PredefImpl.modBigInteger(l.underlying, r.underlying)

    assertEquals(d.underlying, d1, s"l = $l, r = $r")
    assertEquals(m.underlying, m1, s"l = $l, r = $r")
  }

  test("check some example divMods") {
    law(BigInt(9), BigInt(2))
    law(BigInt(9), BigInt(-2))
    law(BigInt(-9), BigInt(2))
    law(BigInt(-9), BigInt(-2))
    law(BigInt(9), BigInt(3))
    law(BigInt(9), BigInt(-3))
    law(BigInt(-9), BigInt(3))
    law(BigInt(-9), BigInt(-3))
    law(BigInt(2), BigInt(3))
    law(BigInt(2), BigInt(-3))
    law(BigInt(-2), BigInt(3))
    law(BigInt(-2), BigInt(-3))
    law(BigInt("-15934641381326140386510"), BigInt(599767409L))
    law(BigInt("2885517232792582372714"), BigInt(-7104274460L))
    law(BigInt("671836834585"), BigInt("7104274460"))
  }

  property("implementation of divMod matches predef") {
    Prop.forAll(genBigInt, genBigInt)((l, r) => law(l, r))
  }

  property("check upper bound property") {
    Prop.forAll(genBigInt, genBigInt) { (l0, r0) =>
      val l = l0.abs
      val r = r0.abs
      if (r > 1) {
        assert(((l + 1) / r) <= ((l >> 1) + 1) / (r >> 1))
      }
    }
  }
}
