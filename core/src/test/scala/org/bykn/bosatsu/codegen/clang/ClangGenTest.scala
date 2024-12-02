package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.{PackageName, TestUtils, Identifier}
import org.scalacheck.Prop
import org.bykn.bosatsu.{Generators, StringUtil}
import org.bykn.bosatsu.Generators.genValidUtf

class ClangGenTest extends munit.ScalaCheckSuite {
  def assertPredefFns(fns: String*)(matches: String)(implicit loc: munit.Location) =
    TestUtils.checkMatchless("""
x = 1    
""") { matchlessMap0 =>
  
      val fnSet = fns.toSet
      val matchlessMap = matchlessMap0
        .flatMap {
          case (k, vs) if k == PackageName.PredefName =>
            (k -> vs.filter(tup => fnSet(tup._1.asString))) :: Nil
          case _ => Nil
        }
        .toMap

      val res = ClangGen.renderMain(
        sortedEnv = Vector(
          NonEmptyList.one(PackageName.PredefName -> matchlessMap(PackageName.PredefName)),
        ),
        externals = ClangGen.ExternalResolver.FromJvmExternals,
        value = (PackageName.PredefName, Identifier.Name(fns.last))
      )

      res match {
        case Right(d) => assertEquals(d.render(80), matches)
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

BValue __bsts_t_lambda0(BValue __bsts_b_a0, BValue __bsts_b_b0) {
    return alloc_enum2(1, __bsts_b_a0, __bsts_b_b0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_build__List(BValue __bsts_b_fn0) {
    return call_fn2(__bsts_b_fn0,
        alloc_boxed_pure_fn2(__bsts_t_lambda0),
        alloc_enum0(0));
}""")
  }
  test("check foldr_List") {
    assertPredefFns("foldr_List")("""#include "bosatsu_runtime.h"

BValue __bsts_t_closure0(BValue* __bstsi_slot, BValue __bsts_b_list1) {
    if (get_variant(__bsts_b_list1) == (0)) {
        return __bstsi_slot[0];
    }
    else {
        BValue __bsts_b_h0 = get_enum_index(__bsts_b_list1, 0);
        BValue __bsts_b_t0 = get_enum_index(__bsts_b_list1, 1);
        return call_fn2(__bstsi_slot[1],
            __bsts_b_h0,
            __bsts_t_closure0(__bstsi_slot, __bsts_b_t0));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldr__List(BValue __bsts_b_list0,
    BValue __bsts_b_fn0,
    BValue __bsts_b_acc0) {
    BValue __bsts_l_captures1[2] = { __bsts_b_acc0, __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure1(2,
        __bsts_l_captures1,
        __bsts_t_closure0);
    return call_fn1(__bsts_b_loop0, __bsts_b_list0);
}""")
  }

  test("check foldLeft and reverse_concat") {
    assertPredefFns("foldLeft", "reverse_concat")("""#include "bosatsu_runtime.h"

BValue __bsts_t_closure0(BValue* __bstsi_slot,
    BValue __bsts_b_lst1,
    BValue __bsts_b_item1) {
    BValue __bsts_l_loop__temp3;
    BValue __bsts_l_loop__temp4;
    _Bool __bsts_l_cond1 = 1;
    BValue __bsts_l_res2;
    while (__bsts_l_cond1) {
        if (get_variant(__bsts_b_lst1) == (0)) {
            __bsts_l_cond1 = 0;
            __bsts_l_res2 = __bsts_b_item1;
        }
        else {
            BValue __bsts_b_head0 = get_enum_index(__bsts_b_lst1, 0);
            BValue __bsts_b_tail0 = get_enum_index(__bsts_b_lst1, 1);
            __bsts_l_loop__temp3 = __bsts_b_tail0;
            __bsts_l_loop__temp4 = call_fn2(__bstsi_slot[0],
                __bsts_b_item1,
                __bsts_b_head0);
            __bsts_b_lst1 = __bsts_l_loop__temp3;
            __bsts_b_item1 = __bsts_l_loop__temp4;
        }
    }
    return __bsts_l_res2;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldLeft(BValue __bsts_b_lst0,
    BValue __bsts_b_item0,
    BValue __bsts_b_fn0) {
    BValue __bsts_l_captures5[1] = { __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure2(1,
        __bsts_l_captures5,
        __bsts_t_closure0);
    return call_fn2(__bsts_b_loop0, __bsts_b_lst0, __bsts_b_item0);
}

BValue __bsts_t_lambda6(BValue __bsts_b_tail0, BValue __bsts_b_h0) {
    return alloc_enum2(1, __bsts_b_h0, __bsts_b_tail0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(BValue __bsts_b_front0,
    BValue __bsts_b_back0) {
    return ___bsts_g_Bosatsu_l_Predef_l_foldLeft(__bsts_b_front0,
        __bsts_b_back0,
        alloc_boxed_pure_fn2(__bsts_t_lambda6));
}""")
  }


  def mockCodePointFn(bytes: Array[Byte], offset: Int): Int = {
    def s(i: Int) = bytes(offset + i).toInt & 0xff

    val c = s(0);
    var code_point: Int = 0
    val remaining = bytes.length - offset
    if (c <= 0x7F) {
        // 1-byte sequence (ASCII)
        code_point = c;
    } else if ((c & 0xE0) == 0xC0) {
        // 2-byte sequence
        if (remaining < 2 || (s(1) & 0xC0) != 0x80) {
            // Invalid continuation byte
            return -1;
        }
        code_point = ((c & 0x1F) << 6) | (s(1) & 0x3F);
    } else if ((c & 0xF0) == 0xE0) {
        // 3-byte sequence
        if (remaining < 3 || (s(1) & 0xC0) != 0x80 || (s(2) & 0xC0) != 0x80) {
            // Invalid continuation bytes
            return -1;
        }
        code_point = ((c & 0x0F) << 12) | ((s(1) & 0x3F) << 6) | (s(2) & 0x3F);
    } else if ((c & 0xF8) == 0xF0) {
        // 4-byte sequence
        if (remaining < 4 || (s(1) & 0xC0) != 0x80 || (s(2) & 0xC0) != 0x80 || (s(3) & 0xC0) != 0x80) {
            // Invalid continuation bytes
            return -1;
        }
        code_point = ((c & 0x07) << 18) | ((s(1) & 0x3F) << 12) | ((s(2) & 0x3F) << 6) | (s(3) & 0x3F);
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

      if (c <= 0x7F) {
          // 1-byte sequence (ASCII)
          1;
      } else if ((c & 0xE0) == 0xC0) {
          // 2-byte sequence
          if (remaining < 1 || (s(1) & 0xC0) != 0x80) {
              // Invalid continuation byte
              sys.error("invalid")
          }
          else {
            2;
          }
      } else if ((c & 0xF0) == 0xE0) {
          // 3-byte sequence
          if (remaining < 2 || (s(1) & 0xC0) != 0x80 || (s(2) & 0xC0) != 0x80) {
              // Invalid continuation bytes
              sys.error("invalid")
          }
          else {
            3;
          }
      } else if ((c & 0xF8) == 0xF0) {
          // 4-byte sequence
          if (remaining < 3 || (s(1) & 0xC0) != 0x80 || (s(2) & 0xC0) != 0x80 || (s(3) & 0xC0) != 0x80) {
              // Invalid continuation bytes
              sys.error("invalid")
          }
          else {
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
    def loop(o: Int): List[Int] = {
      if (o >= bytes.length) Nil
      else {
        val cp = mockCodePointFn(bytes, o)
        val inc = utf8Bytes(cp)
        cp :: loop(o + inc)
      }
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

}