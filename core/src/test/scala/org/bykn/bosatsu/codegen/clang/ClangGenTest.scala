package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.{PackageName, TestUtils, Identifier}

class ClangGenTest extends munit.FunSuite {
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
        STATIC_PUREFN(__bsts_t_lambda0),
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
            __bsts_b_lst1 = __bsts_b_tail0;
            __bsts_b_item1 = call_fn2(__bstsi_slot[0],
                __bsts_b_item1,
                __bsts_b_head0);
        }
    }
    return __bsts_l_res2;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldLeft(BValue __bsts_b_lst0,
    BValue __bsts_b_item0,
    BValue __bsts_b_fn0) {
    BValue __bsts_l_captures3[1] = { __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure2(1,
        __bsts_l_captures3,
        __bsts_t_closure0);
    return call_fn2(__bsts_b_loop0, __bsts_b_lst0, __bsts_b_item0);
}

BValue __bsts_t_lambda4(BValue __bsts_b_tail0, BValue __bsts_b_h0) {
    return alloc_enum2(1, __bsts_b_h0, __bsts_b_tail0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(BValue __bsts_b_front0,
    BValue __bsts_b_back0) {
    return ___bsts_g_Bosatsu_l_Predef_l_foldLeft(__bsts_b_front0,
        __bsts_b_back0,
        STATIC_PUREFN(__bsts_t_lambda4));
}""")
  }
}