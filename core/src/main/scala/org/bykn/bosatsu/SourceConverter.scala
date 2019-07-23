package org.bykn.bosatsu

import cats.implicits._
import cats.data.{ NonEmptyList, State }
import cats.Functor

import org.bykn.bosatsu.rankn.{Type, ParsedTypeEnv}

import ListLang.{KVPair, SpliceOrItem}

import Identifier.{Bindable, Constructor}

import Declaration._

/**
 * Convert a source types (a syntactic expression) into
 * the internal representations
 */
final class SourceConverter(
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
              val realTpe = toType(tpe)
              // move the annotation to the right
              val newRhs = Expr.Annotation(rhs, realTpe, decl)
              solvePat(pat, newRhs)
            case Pattern.Named(nm, p) =>
               // this is the same as creating a let nm = value first
               val inner = solvePat(p, rhs)
               Expr.Let(nm, rhs, inner, RecursionKind.NonRecursive, decl)
            case pat =>
              val newPattern = unTuplePattern(pat)
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
        val lambda = defstmt.toLambdaExpr({ res => apply(res._1.get) }, decl)(
          unTuplePattern(_), toType(_))
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
          args.map(unTuplePattern),
          apply(body),
          decl)
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
          val newPattern = unTuplePattern(pat)
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
            val newPattern = unTuplePattern(binding)
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
            val newPattern = unTuplePattern(binding)
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

  final def toType(t: TypeRef): Type = {
    import rankn.Type._
    import TypeRef._

    t match {
      case TypeVar(v) => TyVar(Type.Var.Bound(v))
      case TypeName(n) => TyConst(nameToType(n.ident))
      case TypeArrow(a, b) => Fun(toType(a), toType(b))
      case TypeApply(a, bs) =>
        def toType1(fn: Type, args: NonEmptyList[TypeRef]): Type =
          args match {
            case NonEmptyList(a0, Nil) => TyApply(fn, toType(a0))
            case NonEmptyList(a0, a1 :: as) => toType1(TyApply(fn, toType(a0)), NonEmptyList(a1, as))
          }
        toType1(toType(a), bs)
      case TypeLambda(pars0, TypeLambda(pars1, e)) =>
        // we normalize to lifting all the foralls to the outside
        toType(TypeLambda(pars0 ::: pars1, e))
      case TypeLambda(pars, e) =>
        Type.forAll(pars.map { case TypeVar(v) => Type.Var.Bound(v) }.toList, toType(e))
      case TypeTuple(ts) =>
        Type.Tuple(ts.map(toType(_)))
    }
  }

  def toDefinition(pname: PackageName, tds: TypeDefinitionStatement): rankn.DefinedType[Unit] = {
    import Statement._

    def typeVar(i: Long): Type.TyVar =
      Type.TyVar(Type.Var.Bound(s"anon$i"))

    type StT = ((Set[Type.TyVar], List[Type.TyVar]), Long)
    type VarState[A] = State[StT, A]

    def add(t: Type.TyVar): VarState[Type.TyVar] =
      State.modify[StT] { case ((ss, sl), i) => ((ss + t, t :: sl), i) }.as(t)

    lazy val nextVar: VarState[Type.TyVar] =
      for {
        vsid <- State.get[StT]
        ((existing, _), id) = vsid
        _ <- State.modify[StT] { case (s, id) => (s, id + 1L) }
        candidate = typeVar(id)
        tv <- if (existing(candidate)) nextVar else add(candidate)
      } yield tv

    def buildParam(p: (Bindable, Option[Type])): VarState[(Bindable, Type)] =
      p match {
        case (parname, Some(tpe)) =>
          State.pure((parname, tpe))
        case (parname, None) =>
          nextVar.map { v => (parname, v) }
      }

    def existingVars[A](ps: List[(A, Option[Type])]): List[Type.TyVar] = {
      val pt = ps.flatMap(_._2)
      Type.freeTyVars(pt).map(Type.TyVar(_))
    }

    def buildParams(args: List[(Bindable, Option[Type])]): VarState[List[(Bindable, Type)]] =
      args.traverse(buildParam _)

    // This is a functor on List[(Bindable, Option[A])]
    val deep = Functor[List].compose(Functor[(Bindable, ?)]).compose(Functor[Option])

    def updateInferedWithDecl(
      typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
      typeParams0: List[Type.Var.Bound]): List[Type.Var.Bound] =
        typeArgs match {
          case None => typeParams0
          case Some(decl) =>
            val declTV: List[Type.Var.Bound] = decl.toList.map(_.toBoundVar)
            val declSet = declTV.toSet
            // TODO we should have a lint that fails if declTV is not
            // a superset of what you would derive from the args
            // the purpose here is to control the *order* of
            // and to allow introducing phantom parameters, not
            // it is confusing if some are explicit, but some are not
            declTV.distinct ::: typeParams0.filterNot(declSet)
        }

    tds match {
      case Struct(nm, typeArgs, args, _) =>
        val argsType = deep.map(args)(toType(_))
        val initVars = existingVars(argsType)
        val initState = ((initVars.toSet, initVars.reverse), 0L)
        val (((_, typeVars), _), params) = buildParams(argsType).run(initState).value
        // we reverse to make sure we see in traversal order
        val typeParams0 = typeVars.reverseMap { tv =>
          tv.toVar match {
            case b@Type.Var.Bound(_) => b
            case unexpected => sys.error(s"unexpectedly parsed a non bound var: $unexpected")
          }
        }

        val typeParams = updateInferedWithDecl(typeArgs, typeParams0)

        val tname = TypeName(nm)
        val consValueType =
          rankn.DefinedType
            .constructorValueType(
              pname,
              tname,
              typeParams,
              params.map(_._2))
        rankn.DefinedType(pname,
          tname,
          typeParams.map((_, ())),
          (nm, params, consValueType) :: Nil)
      case Enum(nm, typeArgs, items, _) =>
        val conArgs = items.get.map { case (nm, args) =>
          val argsType = deep.map(args)(toType)
          (nm, argsType)
        }
        val constructorsS = conArgs.traverse { case (nm, argsType) =>
          buildParams(argsType).map { params =>
            (nm, params)
          }
        }
        val initVars = existingVars(conArgs.toList.flatMap(_._2))
        val initState = ((initVars.toSet, initVars.reverse), 0L)
        val (((_, typeVars), _), constructors) = constructorsS.run(initState).value
        // we reverse to make sure we see in traversal order
        val typeParams0 = typeVars.reverseMap { tv =>
          tv.toVar match {
            case b@Type.Var.Bound(_) => b
            case unexpected => sys.error(s"unexpectedly parsed a non bound var: $unexpected")
          }
        }
        val typeParams = updateInferedWithDecl(typeArgs, typeParams0)
        val tname = TypeName(nm)
        val finalCons = constructors.toList.map { case (c, params) =>
          val consValueType =
            rankn.DefinedType.constructorValueType(
              pname,
              tname,
              typeParams,
              params.map(_._2))
          (c, params, consValueType)
        }
        rankn.DefinedType(pname, TypeName(nm), typeParams.map((_, ())), finalCons)
      case ExternalStruct(nm, targs, _) =>
        rankn.DefinedType(pname, TypeName(nm), targs.map { case TypeRef.TypeVar(v) => (Type.Var.Bound(v), ()) }, Nil)
    }
  }

  /**
   * Tuples are converted into standard types using an HList strategy
   */
  def unTuplePattern(pat: Pattern.Parsed): Pattern[(PackageName, Constructor), rankn.Type] =
    pat.mapStruct[(PackageName, Constructor)] {
      case (None, args) =>
        // this is a tuple pattern
        def loop(args: List[Pattern[(PackageName, Constructor), TypeRef]]): Pattern[(PackageName, Constructor), TypeRef] =
          args match {
            case Nil =>
              // ()
              Pattern.PositionalStruct(
                (Predef.packageName, Constructor("Unit")),
                Nil)
            case h :: tail =>
              val tailP = loop(tail)
              Pattern.PositionalStruct(
                (Predef.packageName, Constructor("TupleCons")),
                h :: tailP :: Nil)
          }

        loop(args)
      case (Some(nm), args) =>
        // this is a struct pattern
        Pattern.PositionalStruct(nameToCons(nm), args)
    }
    .mapType(toType)

  def toTypeEnv(pn0: PackageName, stmt: Statement): ParsedTypeEnv[Unit] =
    Statement.definitionsOf(stmt)
      .foldLeft(ParsedTypeEnv.empty[Unit]) { (te, d) =>
        te.addDefinedType(toDefinition(pn0, d))
      }

  /**
   * Return the lets in order they appear
   */
  def toLets(stmt: Statement): (List[(Bindable, RecursionKind, Expr[Declaration])], List[(Bindable, Type)]) = {
    import Statement._

    // Each time we need a name, we can call anonNames.next()
    // it is mutable, but in a limited scope
    // this is lazy, because not all statements need anonymous names
    lazy val anonNames: Iterator[Bindable] = {
      // this is safe as a set because we only use it to filter
      val allNames =
        Statement.valuesOf(stmt)
          .iterator
          .flatMap { v => v.names.iterator ++ v.allNames.iterator }
          .toSet

      rankn.Type
        .allBinders
        .iterator
        .map(_.name)
        .map(Identifier.Name(_))
        .filterNot(allNames)
    }

    def bindings(
      b: Pattern[(PackageName, Constructor), Type],
      decl: Expr[Declaration]): NonEmptyList[(Identifier.Bindable, Expr[Declaration])] =
      b match {
        case Pattern.Var(nm) =>
          NonEmptyList((nm, decl), Nil)
        case Pattern.Annotation(p, tpe) =>
          // we can just move the annotation to the expr:
          bindings(p, Expr.Annotation(decl, tpe, decl.tag))
        case complex =>
          val (prefix, rightHandSide) = decl match {
            case v@Expr.Var(_, _, _) =>
              // no need to make a new var to point to a var
              (Nil, v)
            case _ =>
              val ident = anonNames.next()
              val v = Expr.Var(None, ident, decl.tag)
              ((ident, decl) :: Nil, v)
          }

          val tail = complex.names.map { nm =>
            val pat = complex.filterVars(_ == nm)
            (nm, Expr.Match(rightHandSide,
              NonEmptyList((pat, Expr.Var(None, nm, decl.tag)), Nil), decl.tag))
          }

          def concat[A](ls: List[A], tail: NonEmptyList[A]): NonEmptyList[A] =
            ls match {
              case Nil => tail
              case h :: t => NonEmptyList(h, t ::: tail.toList)
            }

          NonEmptyList.fromList(tail) match {
            case Some(netail) =>
              concat(prefix, netail)
            case None =>
              // there are no names to bind here, but we still need to typecheck the match
              val dummy = anonNames.next()
              val pat = complex.unbind
              val shapeMatch = (dummy,
                Expr.Match(rightHandSide,
                  NonEmptyList.of((pat, Expr.Literal(Lit.fromInt(0), decl.tag))), decl.tag))
              concat(prefix, NonEmptyList(shapeMatch, Nil))
            }
      }

      val init: (List[(Bindable, RecursionKind, Expr[Declaration])], List[(Bindable, Type)]) = (Nil, Nil)

      val (bs, es) = Statement.valuesOf(stmt)
        .foldLeft(init) { case ((binds, exts), vs) =>
          vs match {
            case Bind(BindingStatement(bound, decl, _)) =>
              val pat = unTuplePattern(bound)
              val binds1 = bindings(pat, apply(decl))
              // since we reverse below, we nee to reverse here
              val nonRec = binds1.toList.reverseMap { case (n, d) => (n, RecursionKind.NonRecursive, d) }
              (nonRec ::: binds, exts)
            case Def(defstmt@DefStatement(_, _, _, _)) =>
              // using body for the outer here is a bummer, but not really a good outer otherwise
              val lam = defstmt.toLambdaExpr(
                { res => apply(res._1.get) },
                defstmt.result._1.get)(unTuplePattern(_), toType(_))
              ((defstmt.name, RecursionKind.Recursive, lam) :: binds, exts)
            case ExternalDef(name, params, result, _) =>
              val tpe: rankn.Type = {
                def buildType(ts: List[rankn.Type]): rankn.Type =
                  ts match {
                    case Nil => toType(result)
                    case h :: tail => rankn.Type.Fun(h, buildType(tail))
                  }
                buildType(params.map { p => toType(p._2) })
              }
              val freeVars = rankn.Type.freeTyVars(tpe :: Nil)
              // these vars were parsed so they are never skolem vars
              val freeBound = freeVars.map {
                case b@rankn.Type.Var.Bound(_) => b
                case s@rankn.Type.Var.Skolem(_, _) => sys.error(s"invariant violation: parsed a skolem var: $s")
              }
              val maybeForAll = rankn.Type.forAll(freeBound, tpe)
              (binds, (name, maybeForAll) :: exts)
          }
        }

    // we need to reverse them to put in file order now
    (bs.reverse, es.reverse)
  }

  def toProgram(pn0: PackageName, stmt: Statement): Program[ParsedTypeEnv[Unit], Expr[Declaration], Statement] = {

    val pte0 = toTypeEnv(pn0, stmt)
    val (binds, exts) = toLets(stmt)

    val pte1 = exts.foldLeft(pte0) { case (pte, (name, tpe)) =>
      pte.addExternalValue(pn0, name, tpe)
    }

    Program(pte1, binds, exts.map(_._1), stmt)
  }
}
