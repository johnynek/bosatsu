package dev.bosatsu.codegen.clang

import dev.bosatsu.Generators.genValidUtf
import dev.bosatsu.{
  Generators,
  StringUtil,
  PredefImpl,
  PackageName,
  PackageMap,
  Par,
  TestUtils,
  Identifier,
  Require
}
import org.scalacheck.{Prop, Gen}

class ClangGenTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000000)
      .withMaxDiscardRatio(10)

  def assertPredefFns(
      fns: String*
  )(matches: String)(implicit loc: munit.Location) =
    TestUtils.checkPackageMap("""
x = 1    
""") { pm =>
      val fnSet = fns.toSet
      val predef = pm.toMap(PackageName.PredefName)
      val filtered = predef.filterLets(ident => fnSet(ident.asString))
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
        case Left(e)  => fail(e.toString)
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

BValue __bsts_t_closure__loop0(BValue* __bstsi_slot, BValue __bsts_b_list0) {
    if (get_variant(__bsts_b_list0) == 0) {
        return __bstsi_slot[0];
    }
    else {
        BValue __bsts_b_h0 = get_enum_index(__bsts_b_list0, 0);
        BValue __bsts_b_t0 = get_enum_index(__bsts_b_list0, 1);
        return call_fn2(__bstsi_slot[1],
            __bsts_b_h0,
            __bsts_t_closure__loop0(__bstsi_slot, __bsts_b_t0));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldr__List(BValue __bsts_b_list0,
    BValue __bsts_b_fn0,
    BValue __bsts_b_acc0) {
    BValue __bsts_l_captures1[2] = { __bsts_b_acc0, __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure1(2,
        __bsts_l_captures1,
        __bsts_t_closure__loop0);
    return call_fn1(__bsts_b_loop0, __bsts_b_list0);
}

int main(int argc, char** argv) {
    GC_init();
    init_statics();
    atexit(free_statics);
    BValue main_value = ___bsts_g_Bosatsu_l_Predef_l_foldr__List();
    return run_main(main_value, argc, argv);
}""")
  }

  test("check foldl_List and reverse_concat") {
    assertPredefFns("foldl_List", "reverse_concat")(
      """#include "bosatsu_runtime.h"
#include <stdlib.h>
#include "gc.h"

BValue ___bsts_g_Bosatsu_l_Predef_l_foldl__List(BValue __bsts_b_lst0,
    BValue __bsts_b_item0,
    BValue __bsts_b_fn0) {
    BValue __bsts_a_0;
    BValue __bsts_a_1;
    BValue __bsts_a_3;
    BValue __bsts_a_5;
    __bsts_a_3 = __bsts_b_lst0;
    __bsts_a_5 = __bsts_b_item0;
    __bsts_a_0 = alloc_enum0(1);
    _Bool __bsts_l_cond0;
    __bsts_l_cond0 = get_variant_value(__bsts_a_0) == 1;
    while (__bsts_l_cond0) {
        if (get_variant(__bsts_a_3) == 0) {
            __bsts_a_0 = alloc_enum0(0);
            __bsts_a_1 = __bsts_a_5;
        }
        else {
            BValue __bsts_b_head0 = get_enum_index(__bsts_a_3, 0);
            BValue __bsts_b_tail0 = get_enum_index(__bsts_a_3, 1);
            BValue __bsts_a_2 = __bsts_b_tail0;
            BValue __bsts_a_4 = call_fn2(__bsts_b_fn0,
                __bsts_a_5,
                __bsts_b_head0);
            __bsts_a_3 = __bsts_a_2;
            __bsts_a_5 = __bsts_a_4;
        }
        __bsts_l_cond0 = get_variant_value(__bsts_a_0) == 1;
    }
    return __bsts_a_1;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(BValue __bsts_b_front0,
    BValue __bsts_b_back0) {
    BValue __bsts_a_6;
    BValue __bsts_a_7;
    BValue __bsts_a_9;
    BValue __bsts_a_11;
    __bsts_a_9 = __bsts_b_front0;
    __bsts_a_11 = __bsts_b_back0;
    __bsts_a_6 = alloc_enum0(1);
    _Bool __bsts_l_cond1;
    __bsts_l_cond1 = get_variant_value(__bsts_a_6) == 1;
    while (__bsts_l_cond1) {
        if (get_variant(__bsts_a_9) == 0) {
            __bsts_a_6 = alloc_enum0(0);
            __bsts_a_7 = __bsts_a_11;
        }
        else {
            BValue __bsts_b_head0 = get_enum_index(__bsts_a_9, 0);
            BValue __bsts_b_tail0 = get_enum_index(__bsts_a_9, 1);
            BValue __bsts_a_8 = __bsts_b_tail0;
            BValue __bsts_a_10 = alloc_enum2(1, __bsts_b_head0, __bsts_a_11);
            __bsts_a_9 = __bsts_a_8;
            __bsts_a_11 = __bsts_a_10;
        }
        __bsts_l_cond1 = get_variant_value(__bsts_a_6) == 1;
    }
    return __bsts_a_7;
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
