package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._

import ListLang.{KVPair, SpliceOrItem}

import Identifier.Constructor

import Declaration._

/**
 * Convert a Declaration (a syntactic expression) into
 * the core untyped lambda calculus, Expr
 */
final class DeclarationToExpr(
  nameToType: Constructor => rankn.Type.Const,
  nameToCons: Constructor => (PackageName, Constructor)) {

  final def apply(decl: Declaration): Expr[Declaration] =
    decl match {
      case Apply(fn, args, _) =>
        Expr.buildApp(apply(fn), args.toList.map(apply(_)), decl)
      case ao@ApplyOp(left, op, right) =>
        val opVar: Expr[Declaration] = Expr.Var(None, op, ao.opVar)
        Expr.buildApp(opVar, apply(left) :: apply(right) :: Nil, decl)
      case Binding(BindingStatement(pat, value, prest@Padding(_, rest))) =>
        val erest = apply(rest)

        def solvePat(pat: Pattern.Parsed, rhs: Expr[Declaration]): Expr[Declaration] =
          pat match {
            case Pattern.Var(arg) =>
              Expr.Let(arg, rhs, erest, RecursionKind.NonRecursive, decl)
            case Pattern.Annotation(pat, tpe) =>
              val realTpe = tpe.toType(nameToType)
              // move the annotation to the right
              val newRhs = Expr.Annotation(rhs, realTpe, decl)
              solvePat(pat, newRhs)
            case Pattern.Named(nm, p) =>
               // this is the same as creating a let nm = value first
               val inner = solvePat(p, rhs)
               Expr.Let(nm, rhs, inner, RecursionKind.NonRecursive, decl)
            case pat =>
              val newPattern = unTuplePattern(pat, nameToType, nameToCons)
              val expBranches = NonEmptyList.of((newPattern, erest))
              Expr.Match(rhs, expBranches, decl)
          }

        solvePat(pat, apply(value))
      case Comment(CommentStatement(_, Padding(_, decl))) =>
        apply(decl).map(_ => decl)
      case DefFn(defstmt@DefStatement(_, _, _, _)) =>
        val inExpr = defstmt.result match {
          case (_, Padding(_, in)) => apply(in)
        }
        val lambda = defstmt.toLambdaExpr({ res => apply(res._1.get) }, decl)(unTuplePattern(_, nameToType, nameToCons), _.toType(nameToType))
        // we assume all defs are recursive: we put them in scope before the method
        // is called. We rely on DefRecursionCheck to rule out bad recursions
        Expr.Let(defstmt.name, lambda, inExpr, recursive = RecursionKind.Recursive, decl)
      case IfElse(ifCases, elseCase) =>
        def apply0(ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])], elseC: Expr[Declaration]): Expr[Declaration] =
          ifs match {
            case NonEmptyList((cond, ifTrue), Nil) =>
              Expr.ifExpr(cond, ifTrue, elseC, decl)
            case NonEmptyList(ifTrue, h :: tail) =>
              val elseC1 = apply0(NonEmptyList(h, tail), elseC)
              apply0(NonEmptyList.of(ifTrue), elseC1)
          }
        apply0(ifCases.map { case (d0, d1) =>
          (apply(d0), apply(d1.get))
        }, apply(elseCase.get))
      case Lambda(args, body) =>
        Expr.buildPatternLambda(
          args.map(unTuplePattern(_, nameToType, nameToCons)),
          apply(body), decl)
      case Literal(lit) =>
        Expr.Literal(lit, decl)
      case Parens(p) =>
        apply(p).map(_ => decl)
      case Var(ident) =>
        Expr.Var(None, ident, decl)
      case Match(_, arg, branches) =>
        /*
         * The recursion kind is only there for DefRecursionCheck, once
         * that passes, the expr only cares if lets are recursive or not
         */
        val expBranches = branches.get.map { case (pat, oidecl) =>
          val decl = oidecl.get
          val newPattern = unTuplePattern(pat, nameToType, nameToCons)
          (newPattern, apply(decl))
        }
        Expr.Match(apply(arg), expBranches, decl)
      case tc@TupleCons(its) =>
        val tup0: Expr[Declaration] = Expr.Var(Some(Predef.packageName), Identifier.Constructor("Unit"), tc)
        val tup2: Expr[Declaration] = Expr.Var(Some(Predef.packageName), Identifier.Constructor("TupleCons"), tc)
        def tup(args: List[Declaration]): Expr[Declaration] =
          args match {
            case Nil => tup0
            case h :: tail =>
              val tailExp = tup(tail)
              val headExp = apply(h)
              Expr.buildApp(tup2, headExp :: tailExp :: Nil, tc)
          }

        tup(its)
      case l@ListDecl(list) =>
        list match {
          case ListLang.Cons(items) =>
            val revDecs: List[SpliceOrItem[Expr[Declaration]]] = items.reverseMap {
              case SpliceOrItem.Splice(s) =>
                SpliceOrItem.Splice(apply(s))
              case SpliceOrItem.Item(item) =>
                SpliceOrItem.Item(apply(item))
            }

            val pn = Option(Predef.packageName)
            def mkC(c: String): Expr[Declaration] =
              Expr.Var(pn, Identifier.Constructor(c), l)
            def mkN(c: String): Expr[Declaration] =
              Expr.Var(pn, Identifier.Name(c), l)

            val empty: Expr[Declaration] = mkC("EmptyList")
            def cons(head: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
              Expr.buildApp(mkC("NonEmptyList"), head :: tail :: Nil, l)

            def concat(headList: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
              Expr.buildApp(mkN("concat"), headList :: tail :: Nil, l)

            revDecs.foldLeft(empty) {
              case (tail, SpliceOrItem.Item(i)) =>
                cons(i, tail)
              case (tail, SpliceOrItem.Splice(s)) =>
                concat(s, tail)
            }
          case ListLang.Comprehension(res, binding, in, filter) =>
            /*
             * [x for y in z] ==
             * z.map_List(\y ->
             *   x)
             *
             * [x for y in z if w] =
             * z.flat_map_List(\y ->
             *   if w: [x]
             *   else: []
             * )
             *
             * [*x for y in z] =
             * z.flat_map_List(\y ->
             *   x
             * )
             *
             * [*x for y in z if w] =
             * z.flat_map_List(\y ->
             *   if w: x
             *   else: []
             * )
             */
            val pn = Option(Predef.packageName)
            val opName = (res, filter) match {
              case (SpliceOrItem.Item(_), None) =>
                "map_List"
              case (SpliceOrItem.Item(_) | SpliceOrItem.Splice(_), _) =>
                "flat_map_List"
            }
            val opExpr: Expr[Declaration] = Expr.Var(pn, Identifier.Name(opName), l)
            val resExpr: Expr[Declaration] = (res, filter) match {
              case (itOrSp, None) =>
                apply(itOrSp.value)
              case (itOrSp, Some(cond)) =>
                val empty: Expr[Declaration] =
                  Expr.Var(pn, Identifier.Constructor("EmptyList"), cond)
                // we need theReturn
                val r = itOrSp.value
                val ritem = apply(r)
                val sing = itOrSp match {
                  case SpliceOrItem.Item(_) =>
                    Expr.App(
                      Expr.App(Expr.Var(pn, Identifier.Constructor("NonEmptyList"), r), ritem, r),
                      empty,
                      r)
                  case SpliceOrItem.Splice(_) => ritem
                }

                Expr.ifExpr(apply(cond),
                  sing,
                  empty,
                  cond)
            }
            val newPattern = unTuplePattern(binding, nameToType, nameToCons)
            val fnExpr: Expr[Declaration] =
              Expr.buildPatternLambda(NonEmptyList.of(newPattern), resExpr, l)
            Expr.buildApp(opExpr, apply(in) :: fnExpr :: Nil, l)
        }
      case l@DictDecl(dict) =>
        val pn = Option(Predef.packageName)
        def mkN(n: String): Expr[Declaration] =
          Expr.Var(pn, Identifier.Name(n), l)
        val empty: Expr[Declaration] =
          Expr.App(mkN("empty_Dict"), mkN("string_Order"), l)

        def add(dict: Expr[Declaration], k: Expr[Declaration], v: Expr[Declaration]): Expr[Declaration] = {
          val fn = mkN("add_key")
          Expr.buildApp(fn, dict :: k :: v :: Nil, l)
        }
        dict match {
          case ListLang.Cons(items) =>
            val revDecs: List[KVPair[Expr[Declaration]]] = items.reverseMap {
              case KVPair(k, v) => KVPair(apply(k), apply(v))
            }
            revDecs.foldLeft(empty) {
              case (dict, KVPair(k, v)) => add(dict, k, v)
            }
          case ListLang.Comprehension(KVPair(k, v), binding, in, filter) =>
            /*
             * { x: y for p in z} ==
             * z.foldLeft(empty_Dict(stringOrder), \dict, v ->
             *   p = v
             *   dict.add_key(x, y)
             *   )
             *
             * { x: y for p in z if w } =
             * z.foldLeft(empty_Dict(stringOrder), \dict, v ->
             *   p = v
             *   if w: dict.add_key(x, y)
             *   else: dict
             *   )
             */
            val pn = Option(Predef.packageName)
            val opExpr: Expr[Declaration] = Expr.Var(pn, Identifier.Name("foldLeft"), l)
            val dictSymbol = Identifier.Name("$d") // TODO we should have better ways to gensym
            val elemSymbol = Identifier.Name("$e")
            val init: Expr[Declaration] = Expr.Var(None, dictSymbol, l)
            val added = add(init, apply(k), apply(v))

            val resExpr: Expr[Declaration] = filter match {
              case None => added
              case Some(cond) =>
                Expr.ifExpr(apply(cond),
                  added,
                  init,
                  cond)
            }
            val newPattern = unTuplePattern(binding, nameToType, nameToCons)
            val body: Expr[Declaration] =
              Expr.Match(Expr.Var(None, elemSymbol, l),
                NonEmptyList.of((newPattern, resExpr)), l)
            val foldFn = Expr.Lambda(dictSymbol,
              Expr.Lambda(elemSymbol,
                body,
                l),
              l)
            Expr.buildApp(opExpr, apply(in) :: empty :: foldFn :: Nil, l)
          }
    }
}
