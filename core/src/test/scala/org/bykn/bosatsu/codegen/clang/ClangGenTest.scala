package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.{PackageName, PackageMap, TestUtils, Identifier, Predef}
import Identifier.Name
import org.bykn.bosatsu.MatchlessFromTypedExpr

import org.bykn.bosatsu.DirectEC.directEC

class ClangGenTest extends munit.FunSuite {
  val predef_c = Code.Include(true, "bosatsu_predef.h")

  def predef(s: String, arity: Int) =
    (PackageName.PredefName -> Name(s)) -> (predef_c,
      ClangGen.generatedName(PackageName.PredefName, Name(s)),
      arity)

  val jvmExternals = {
    val ext = Predef.jvmExternals.toMap.iterator.map { case ((_, n), ffi) => predef(n, ffi.arity) }
      .toMap[(PackageName, Identifier), (Code.Include, Code.Ident, Int)]
    
    { (pn: (PackageName, Identifier)) => ext.get(pn) }
  }

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
        externals = jvmExternals,
        value = (PackageName.PredefName, Identifier.Name(fns.last)),
        evaluator = (Code.Include(true, "eval.h"), Code.Ident("evaluator_run"))
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
  def testFilesCompilesToHash(path0: String, paths: String*)(hashHex: String)(implicit loc: munit.Location) = {
    val pm: PackageMap.Typed[Any] = TestUtils.compileFile(path0, paths*)
    /*
    val exCode = ClangGen.generateExternalsStub(pm)
    println(exCode.render(80))
    sys.error("stop")
    */
    val matchlessMap = MatchlessFromTypedExpr.compile(pm)
    val topoSort = pm.topoSort.toSuccess.get
    val sortedEnv = cats.Functor[Vector].compose[NonEmptyList].map(topoSort) { pn =>
      (pn, matchlessMap(pn))  
    }

    val res = ClangGen.renderMain(
      sortedEnv = sortedEnv,
      externals = jvmExternals,
      value = (PackageName.PredefName, Identifier.Name("ignored")),
      evaluator = (Code.Include(true, "eval.h"), Code.Ident("evaluator_run"))
    )

    res match {
      case Right(d) =>
        val everything = d.render(80)
        val hashed = md5HashToHex(everything)
        assertEquals(hashed, hashHex, s"compilation didn't match. Compiled code:\n\n${"//" * 40}\n\n$everything")
      case Left(e) => fail(e.toString)
    }
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

  test("test_workspace/Ackermann.bosatsu") {
    /* 
      To inspect the code, change the hash, and it will print the code out
     */
    testFilesCompilesToHash("test_workspace/Ackermann.bosatsu")(
      "46716ef3c97cf2a79bf17d4033d55854"
    )
  }
}