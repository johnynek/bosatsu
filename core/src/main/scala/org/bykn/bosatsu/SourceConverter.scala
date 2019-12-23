package org.bykn.bosatsu

import cats.{Applicative, Functor}
import cats.data.{ Ior, NonEmptyChain, NonEmptyList, State }
import cats.implicits._
import org.bykn.bosatsu.rankn.{ParsedTypeEnv, Type, TypeEnv}
import scala.collection.immutable.SortedSet
import scala.collection.mutable.{Map => MMap}
import org.typelevel.paiges.{Doc, Document}

// this is used to make slightly nicer syntax on Error creation
import scala.language.implicitConversions

import ListLang.{KVPair, SpliceOrItem}

import Identifier.{Bindable, Constructor}

import Declaration._

import SourceConverter.{success, Result}

/**
 * Convert a source types (a syntactic expression) into
 * the internal representations
 */
final class SourceConverter(
  thisPackage: PackageName,
  imports: List[Import[PackageName, NonEmptyList[Referant[Variance]]]],
  localDefs: Stream[TypeDefinitionStatement]) {
  /*
   * We should probably error for non-predef name collisions.
   * Maybe we should even error even or predef collisions that
   * are not renamed
   */
  private val localTypeNames = localDefs.map(_.name).toSet
  private val localConstructors = localDefs.flatMap(_.constructors).toSet

  private val typeCache: MMap[Constructor, Type.Const] = MMap.empty
  private val consCache: MMap[Constructor, (PackageName, Constructor)] = MMap.empty

  private val importedTypes: Map[Identifier, (PackageName, TypeName)] =
    Referant.importedTypes(imports)

  private val resolveImportedCons: Map[Identifier, (PackageName, Constructor)] =
    Referant.importedConsNames(imports)

  val importedTypeEnv = Referant.importedTypeEnv(imports)(identity)

  private def nameToType(c: Constructor): rankn.Type.Const =
    typeCache.getOrElseUpdate(c, {
      val tc = TypeName(c)
      val (p1, c1) =
        if (localTypeNames(c)) (thisPackage, tc)
        else importedTypes.getOrElse(c, (thisPackage, tc))
      Type.Const.Defined(p1, c1)
    })

  private def nameToCons(c: Constructor): (PackageName, Constructor) =
    consCache.getOrElseUpdate(c, {
      if (localConstructors(c)) (thisPackage, c)
      else resolveImportedCons.getOrElse(c, (thisPackage, c))
    })

  private def apply(decl: Declaration): Result[Expr[Declaration]] = {
    implicit val parAp = SourceConverter.parallelIor
    decl match {
      case Annotation(term, tpe) =>
        apply(term).map(Expr.Annotation(_, toType(tpe), decl))
      case Apply(fn, args, _) =>
        (apply(fn), args.toList.traverse(apply(_)))
          .mapN { Expr.buildApp(_, _, decl) }
      case ao@ApplyOp(left, op, right) =>
        val opVar: Expr[Declaration] = Expr.Var(None, op, ao.opVar)
        (apply(left), apply(right)).mapN { (l, r) =>
          Expr.buildApp(opVar, l :: r :: Nil, decl)
        }
      case Binding(BindingStatement(pat, value, prest@Padding(_, rest))) =>
        val erest = apply(rest)

        def solvePat(pat: Pattern.Parsed, rrhs: Result[Expr[Declaration]]): Result[Expr[Declaration]] =
          pat match {
            case Pattern.Var(arg) =>
              (erest, rrhs).mapN { (e, rhs) =>
                Expr.Let(arg, rhs, e, RecursionKind.NonRecursive, decl)
              }
            case Pattern.Annotation(pat, tpe) =>
              val realTpe = toType(tpe)
              // move the annotation to the right
              val newRhs = rrhs.map(Expr.Annotation(_, realTpe, decl))
              solvePat(pat, newRhs)
            case Pattern.Named(nm, p) =>
               // this is the same as creating a let nm = value first
              (solvePat(p, rrhs), rrhs).mapN { (inner, rhs) =>
                Expr.Let(nm, rhs, inner, RecursionKind.NonRecursive, decl)
              }
            case pat =>
              // TODO: we need the region on the pattern...
              (unTuplePattern(pat, decl.region), erest, rrhs).mapN { (newPattern, e, rhs) =>
                val expBranches = NonEmptyList.of((newPattern, e))
                Expr.Match(rhs, expBranches, decl)
              }
          }

        solvePat(pat, apply(value))
      case Comment(CommentStatement(_, Padding(_, decl))) =>
        apply(decl).map(_.as(decl))
      case DefFn(defstmt@DefStatement(_, _, _, _)) =>
        val inExpr = defstmt.result match {
          case (_, Padding(_, in)) => apply(in)
        }
        // TODO
        val lambda = defstmt.toLambdaExpr({ res => apply(res._1.get) }, success(decl))(
          unTuplePattern(_, decl.region), { t => success(toType(t)) })
        (inExpr, lambda).mapN { (in, lam) =>
          // We rely on DefRecursionCheck to rule out bad recursions
          val boundName = defstmt.name
          val rec =
            if (UnusedLetCheck.freeBound(lam).contains(boundName)) RecursionKind.Recursive
            else RecursionKind.NonRecursive
          Expr.Let(boundName, lam, in, recursive = rec, decl)
        }
      case IfElse(ifCases, elseCase) =>
        def apply0(ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])], elseC: Expr[Declaration]): Expr[Declaration] =
          ifs match {
            case NonEmptyList((cond, ifTrue), Nil) =>
              Expr.ifExpr(cond, ifTrue, elseC, decl)
            case NonEmptyList(ifTrue, h :: tail) =>
              val elseC1 = apply0(NonEmptyList(h, tail), elseC)
              apply0(NonEmptyList.of(ifTrue), elseC1)
          }
        val if1 = ifCases.traverse { case (d0, d1) =>
          apply(d0).product(apply(d1.get))
        }
        val else1 = apply(elseCase.get)

        (if1, else1).mapN(apply0(_, _))
      case Lambda(args, body) =>
        val argsRes = args.traverse(unTuplePattern(_, decl.region))
        val bodyRes = apply(body)
        (argsRes, bodyRes).mapN { (args, body) =>
          Expr.buildPatternLambda(args, body, decl)
        }
      case Literal(lit) =>
        success(Expr.Literal(lit, decl))
      case Parens(p) =>
        apply(p).map(_.as(decl))
      case Var(ident) =>
        success(Expr.Var(None, ident, decl))
      case Match(_, arg, branches) =>
        /*
         * The recursion kind is only there for DefRecursionCheck, once
         * that passes, the expr only cares if lets are recursive or not
         */
        val expBranches = branches.get.traverse { case (pat, oidecl) =>
          val decl = oidecl.get
          val newPattern = unTuplePattern(pat, decl.region)
          newPattern.product(apply(decl))
        }
        (apply(arg), expBranches).mapN(Expr.Match(_, _, decl))
      case Matches(a, p) =>
        sys.error(s"TODO: support Matches($a, $p)")
      case tc@TupleCons(its) =>
        val tup0: Expr[Declaration] = Expr.Var(Some(PackageName.PredefName), Identifier.Constructor("Unit"), tc)
        val tup2: Expr[Declaration] = Expr.Var(Some(PackageName.PredefName), Identifier.Constructor("TupleCons"), tc)
        def tup(args: List[Declaration]): Result[Expr[Declaration]] =
          args match {
            case Nil => success(tup0)
            case h :: tail =>
              val tailExp = tup(tail)
              val headExp = apply(h)
              (headExp, tailExp).mapN { (h, t) =>
                Expr.buildApp(tup2, h :: t :: Nil, tc)
              }
          }

        tup(its)
      case s@StringDecl(parts) =>
        // a single string item should be converted
        // to that thing,
        // two or more should be converted this to concat_String([items])
        val decls = parts.map {
          case Right((r, str)) => Literal(Lit(str))(r)
          case Left(decl) => decl
        }

        decls match {
          case NonEmptyList(one, Nil) =>
            apply(one)
          case twoOrMore =>
            val lldecl =
              ListDecl(ListLang.Cons(twoOrMore.toList.map(SpliceOrItem.Item(_))))(s.region)

            apply(lldecl).map { listExpr =>

              val fnName: Expr[Declaration] =
                Expr.Var(Some(PackageName.PredefName), Identifier.Name("concat_String"), s)

              Expr.buildApp(fnName, listExpr.as(s: Declaration) :: Nil, s)
            }
        }
      case l@ListDecl(list) =>
        list match {
          case ListLang.Cons(items) =>
            val revDecs: Result[List[SpliceOrItem[Expr[Declaration]]]] =
              items.reverse.traverse {
                case SpliceOrItem.Splice(s) =>
                  apply(s).map(SpliceOrItem.Splice(_))
                case SpliceOrItem.Item(item) =>
                  apply(item).map(SpliceOrItem.Item(_))
              }

            val pn = Option(PackageName.PredefName)
            def mkC(c: String): Expr[Declaration] =
              Expr.Var(pn, Identifier.Constructor(c), l)
            def mkN(c: String): Expr[Declaration] =
              Expr.Var(pn, Identifier.Name(c), l)

            val empty: Expr[Declaration] = mkC("EmptyList")
            def cons(head: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
              Expr.buildApp(mkC("NonEmptyList"), head :: tail :: Nil, l)

            def concat(headList: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
              Expr.buildApp(mkN("concat"), headList :: tail :: Nil, l)

            revDecs.map(_.foldLeft(empty) {
              case (tail, SpliceOrItem.Item(i)) =>
                cons(i, tail)
              case (tail, SpliceOrItem.Splice(s)) =>
                concat(s, tail)
            })
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
            val pn = Option(PackageName.PredefName)
            val opName = (res, filter) match {
              case (SpliceOrItem.Item(_), None) =>
                "map_List"
              case (SpliceOrItem.Item(_) | SpliceOrItem.Splice(_), _) =>
                "flat_map_List"
            }
            val opExpr: Expr[Declaration] = Expr.Var(pn, Identifier.Name(opName), l)
            val resExpr: Result[Expr[Declaration]] =
              filter match {
                case None => apply(res.value)
                case Some(cond) =>
                  // To do filters, we lift all results into lists,
                  // so single items must be made singleton lists
                  val empty: Expr[Declaration] =
                    Expr.Var(pn, Identifier.Constructor("EmptyList"), cond)
                  val ressing = res match {
                    case SpliceOrItem.Item(r) =>
                      val rdec: Declaration = r
                      // here we lift the result into a a singleton list
                      apply(r).map { ritem =>
                        Expr.App(
                          Expr.App(Expr.Var(pn, Identifier.Constructor("NonEmptyList"), rdec), ritem, rdec),
                          empty,
                          rdec)
                      }
                    case SpliceOrItem.Splice(r) => apply(r)
                  }

                  (apply(cond), ressing).mapN { (c, sing) =>
                    Expr.ifExpr(c, sing, empty, cond)
                  }
              }
            (unTuplePattern(binding, decl.region),
              resExpr,
              apply(in)).mapN { (newPattern, resExpr, in) =>
              val fnExpr: Expr[Declaration] =
                Expr.buildPatternLambda(NonEmptyList.of(newPattern), resExpr, l)
              Expr.buildApp(opExpr, in :: fnExpr :: Nil, l)
            }
        }
      case l@DictDecl(dict) =>
        val pn = Option(PackageName.PredefName)
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
            val revDecs: Result[List[KVPair[Expr[Declaration]]]] =
              items.reverse.traverse {
                case KVPair(k, v) =>
                  (apply(k), apply(v)).mapN(KVPair(_, _))
              }
            revDecs.map(_.foldLeft(empty) {
              case (dict, KVPair(k, v)) => add(dict, k, v)
            })
          case ListLang.Comprehension(KVPair(k, v), binding, in, filter) =>
            /*
             * { x: y for p in z} ==
             * z.foldLeft(empty_Dict(stringOrder), \dict, p ->
             *   dict.add_key(x, y)
             *   )
             *
             * { x: y for p in z if w } =
             * z.foldLeft(empty_Dict(stringOrder), \dict, p ->
             *   if w: dict.add_key(x, y)
             *   else: dict
             *   )
             */

            val pn = Option(PackageName.PredefName)
            val opExpr: Expr[Declaration] = Expr.Var(pn, Identifier.Name("foldLeft"), l)
            val dictSymbol = unusedNames(decl.allNames).next
            val init: Expr[Declaration] = Expr.Var(None, dictSymbol, l)
            val added = (apply(k), apply(v)).mapN(add(init, _, _))

            val resExpr: Result[Expr[Declaration]] = filter match {
              case None => added
              case Some(cond0) =>
                (added, apply(cond0)).mapN { (added, cond) =>
                  Expr.ifExpr(cond,
                    added,
                    init,
                    cond0)
                }
            }
            val newPattern = unTuplePattern(binding, decl.region)
            (newPattern, resExpr, apply(in)).mapN { (pat, res, in) =>
              val foldFn = Expr.Lambda(dictSymbol,
                Expr.buildPatternLambda(
                  NonEmptyList(pat, Nil),
                  res,
                  l),
                l)
              Expr.buildApp(opExpr, in :: empty :: foldFn :: Nil, l)
            }
          }
        case rc@RecordConstructor(name, args) =>
          val (p, c) = nameToCons(name)
          localTypeEnv.flatMap(_.getConstructor(p, c) match {
            case Some((params, _, _)) =>
              def argExpr(arg: RecordArg): (Bindable, Result[Expr[Declaration]]) =
                arg match {
                  case RecordArg.Simple(b) =>
                    (b, success(Expr.Var(None, b, rc)))
                  case RecordArg.Pair(k, v) =>
                    (k, apply(v))
                }
              val mappingList = args.toList.map(argExpr)
              val mapping = mappingList.toMap

              lazy val present =
                mappingList
                  .iterator
                  .map(_._1)
                  .foldLeft(SortedSet.empty[Bindable])(_ + _)

              def get(b: Bindable): Result[Expr[Declaration]] =
                mapping.get(b) match {
                  case Some(expr) => expr
                  case None =>
                    SourceConverter.failure(
                      SourceConverter.MissingArg(name, rc, present, b, rc.region))
                }
              val cons: Expr[Declaration] = Expr.Var(None, name, rc)
              val exprArgs = params.traverse { case (b, _) => get(b) }

              val res = exprArgs.map { args =>
                Expr.buildApp(cons, args.toList, rc)
              }
              // we also need to check that there are no unused or duplicated
              // fields
              val paramNamesList = params.map(_._1)
              val paramNames = paramNamesList.toSet
              // here are all the fields we don't understand
              val extra = mappingList.collect { case (k, _) if !paramNames(k) => k }
              // Check that the mapping is exactly the right size
              NonEmptyList.fromList(extra) match {
                case None => res
                case Some(extra) =>
                  SourceConverter
                    .addError(res,
                      SourceConverter.UnexpectedField(name, rc, extra, paramNamesList, rc.region))
              }
            case None =>
              SourceConverter.failure(SourceConverter.UnknownConstructor(name, rc, decl.region))
          })
    }
  }

  private def toType(t: TypeRef): Type =
    TypeRefConverter[cats.Id](t)(nameToType _)

  def toDefinition(pname: PackageName, tds: TypeDefinitionStatement): Result[rankn.DefinedType[Unit]] = {
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
      typeParams0: List[Type.Var.Bound]): Result[List[Type.Var.Bound]] =
        typeArgs match {
          case None => success(typeParams0)
          case Some(decl) =>
            val neBound = decl.map(_.toBoundVar)
            val declTV: List[Type.Var.Bound] = neBound.toList
            val declSet = declTV.toSet
            val missingFromDecl = typeParams0.filterNot(declSet)
            val bestEffort = declTV.distinct ::: missingFromDecl
            if ((declSet.size != declTV.size) || missingFromDecl.nonEmpty) {
              // we have a lint that fails if declTV is not
              // a superset of what you would derive from the args
              // the purpose here is to control the *order* of
              // and to allow introducing phantom parameters, not
              // it is confusing if some are explicit, but some are not
              SourceConverter.partial(
                SourceConverter.InvalidTypeParameters(neBound, typeParams0, tds),
                bestEffort)
            }
            else success(bestEffort)
        }

    tds match {
      case Struct(nm, typeArgs, args) =>
        val argsType = deep.map(args)(toType(_))
        val initVars = existingVars(argsType)
        val initState = ((initVars.toSet, initVars.reverse), 0L)
        val (((_, typeVars), _), params) = buildParams(argsType).run(initState).value
        // we reverse to make sure we see in traversal order
        val typeParams0 = typeVars.reverseMap { tv =>
          tv.toVar match {
            case b@Type.Var.Bound(_) => b
            // $COVERAGE-OFF$ this should be unreachable
            case unexpected =>
              sys.error(s"unexpectedly parsed a non bound var: $unexpected")
            // $COVERAGE-ON$
          }
        }

        updateInferedWithDecl(typeArgs, typeParams0).map { typeParams =>
          val tname = TypeName(nm)
          val consFn = rankn.ConstructorFn.build(pname, tname, typeParams, nm, params)

          rankn.DefinedType(pname,
            tname,
            typeParams.map((_, ())),
            consFn :: Nil)
        }
      case Enum(nm, typeArgs, items) =>
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
            // $COVERAGE-OFF$ this should be unreachable
            case unexpected => sys.error(s"unexpectedly parsed a non bound var: $unexpected")
            // $COVERAGE-ON$
          }
        }
        updateInferedWithDecl(typeArgs, typeParams0).map { typeParams =>
          val tname = TypeName(nm)
          val finalCons = constructors.toList.map { case (c, params) =>
            rankn.ConstructorFn.build(pname, tname, typeParams, c, params)
          }
          rankn.DefinedType(pname, TypeName(nm), typeParams.map((_, ())), finalCons)
        }
      case ExternalStruct(nm, targs) =>
        // TODO make a real check here
        success(rankn.DefinedType(pname, TypeName(nm), targs.map { case TypeRef.TypeVar(v) => (Type.Var.Bound(v), ()) }, Nil))
    }
  }

  /**
   * Tuples are converted into standard types using an HList strategy
   */
  private def unTuplePattern(pat: Pattern.Parsed, region: Region): Result[Pattern[(PackageName, Constructor), rankn.Type]] =
    pat.traverseStruct[Result, (PackageName, Constructor)] {
      case (Pattern.StructKind.Tuple, args) =>
        // this is a tuple pattern
        def loop(args: List[Pattern[(PackageName, Constructor), TypeRef]]): Pattern[(PackageName, Constructor), TypeRef] =
          args match {
            case Nil =>
              // ()
              Pattern.PositionalStruct(
                (PackageName.PredefName, Constructor("Unit")),
                Nil)
            case h :: tail =>
              val tailP = loop(tail)
              Pattern.PositionalStruct(
                (PackageName.PredefName, Constructor("TupleCons")),
                h :: tailP :: Nil)
          }

        args.map(loop(_))
      case (Pattern.StructKind.Named(nm, Pattern.StructKind.Style.TupleLike), rargs) =>
        rargs.flatMap { args =>
          val pc@(p, c) = nameToCons(nm)
          localTypeEnv.flatMap(_.getConstructor(p, c) match {
            case Some((params, _, _)) =>
              val argLen = args.size
              val paramLen = params.size
              if (argLen == paramLen) {
                SourceConverter.success(Pattern.PositionalStruct(pc, args))
              }
              else {
                // do the best we can
                val fixedArgs = (args ::: List.fill(paramLen - argLen)(Pattern.WildCard)).take(paramLen)
                SourceConverter.partial(
                  SourceConverter.InvalidArgCount(nm, pat, argLen, paramLen, region),
                  Pattern.PositionalStruct(pc, fixedArgs))
              }
            case None =>
              SourceConverter.failure(SourceConverter.UnknownConstructor(nm, pat, region))
          })
        }
      case (Pattern.StructKind.NamedPartial(nm, Pattern.StructKind.Style.TupleLike), rargs) =>
        rargs.flatMap { args =>
          val pc@(p, c) = nameToCons(nm)
          localTypeEnv.flatMap(_.getConstructor(p, c) match {
            case Some((params, _, _)) =>
              val argLen = args.size
              val paramLen = params.size
              if (argLen <= paramLen) {
                val fixedArgs = if (argLen < paramLen) (args ::: List.fill(paramLen - argLen)(Pattern.WildCard)) else args
                SourceConverter.success(Pattern.PositionalStruct(pc, fixedArgs))
              }
              else {
                // we have too many
                val fixedArgs = args.take(paramLen)
                SourceConverter.partial(
                  SourceConverter.InvalidArgCount(nm, pat, argLen, paramLen, region),
                  Pattern.PositionalStruct(pc, fixedArgs))
              }
            case None =>
              SourceConverter.failure(SourceConverter.UnknownConstructor(nm, pat, region))
          })
        }
      case (Pattern.StructKind.Named(nm, Pattern.StructKind.Style.RecordLike(fs)), rargs) =>
        rargs.flatMap { args =>
          val pc@(p, c) = nameToCons(nm)
          localTypeEnv.flatMap(_.getConstructor(p, c) match {
            case Some((params, _, _)) =>
              val mapping = fs.toList.iterator.map(_.field).zip(args.iterator).toMap
              lazy val present = SortedSet(fs.toList.iterator.map(_.field).toList: _*)
              def get(b: Bindable): Result[Pattern[(PackageName, Constructor), TypeRef]] =
                mapping.get(b) match {
                  case Some(pat) =>
                    SourceConverter.success(pat)
                  case None =>
                    SourceConverter.partial(SourceConverter.MissingArg(nm, pat, present, b, region), Pattern.WildCard)
                }
              val mapped =
                params
                  .traverse { case (b, _) => get(b) }(SourceConverter.parallelIor)
                  .map(Pattern.PositionalStruct(pc, _))

              val paramNamesList = params.map(_._1)
              val paramNames = paramNamesList.toSet
              // here are all the fields we don't understand
              val extra = fs.toList.iterator.map(_.field).filterNot(paramNames).toList
              // Check that the mapping is exactly the right size
              NonEmptyList.fromList(extra) match {
                case None => mapped
                case Some(extra) =>
                  SourceConverter
                    .addError(mapped,
                      SourceConverter.UnexpectedField(nm, pat, extra, paramNamesList, region))
              }
            case None =>
              SourceConverter.failure(SourceConverter.UnknownConstructor(nm, pat, region))
          })
        }
      case (Pattern.StructKind.NamedPartial(nm, Pattern.StructKind.Style.RecordLike(fs)), rargs) =>
        rargs.flatMap { args =>
          val pc@(p, c) = nameToCons(nm)
          localTypeEnv.flatMap(_.getConstructor(p, c) match {
            case Some((params, _, _)) =>
              val mapping = fs.toList.iterator.map(_.field).zip(args.iterator).toMap
              def get(b: Bindable): Pattern[(PackageName, Constructor), TypeRef] =
                mapping.get(b) match {
                  case Some(pat) => pat
                  case None => Pattern.WildCard
                }
              val derefArgs = params.map { case (b, _) => get(b) }
              val res0 = SourceConverter.success(Pattern.PositionalStruct(pc, derefArgs))

              val paramNamesList = params.map(_._1)
              val paramNames = paramNamesList.toSet
              // here are all the fields we don't understand
              val extra = fs.toList.iterator.map(_.field).filterNot(paramNames).toList
              // Check that the mapping is exactly the right size
              NonEmptyList.fromList(extra) match {
                case None => res0
                case Some(extra) =>
                  SourceConverter
                    .addError(res0,
                      SourceConverter.UnexpectedField(nm, pat, extra, paramNamesList, region))
              }
            case None =>
              SourceConverter.failure(SourceConverter.UnknownConstructor(nm, pat, region))
          })
        }
    }(SourceConverter.parallelIor) // use the parallel, not the default Applicative which is Monadic
    .map(_.mapType(toType))

  private lazy val toTypeEnv: Result[ParsedTypeEnv[Unit]] =
    localDefs
      .foldM(ParsedTypeEnv.empty[Unit]) { (te, d) =>
        toDefinition(thisPackage, d)
          .map(te.addDefinedType(_))
      }

  private lazy val localTypeEnv: Result[TypeEnv[Any]] =
    toTypeEnv.map { p => importedTypeEnv ++ TypeEnv.fromParsed(p) }

  private def unusedNames(allNames: Bindable => Boolean): Iterator[Bindable] =
    rankn.Type
      .allBinders
      .iterator
      .map(_.name)
      .map(Identifier.Name(_))
      .filterNot(allNames)

  /**
   * Externals are not permitted to be shadowed at the top level
   */
  private def checkExternalDefShadowing(values: Stream[Statement.ValueStatement]): Result[Unit] = {
    val extDefNames =
      values.collect {
        case ed@Statement.ExternalDef(name, _, _) => (name, ed.region)
      }

    val sunit = success(())

    if (extDefNames.isEmpty) sunit
    else {
      val grouped = extDefNames.groupBy(_._1)
      val extDefNamesSet = grouped.keySet
      val dupExts = grouped.filter { case (_, vs) => vs.lengthCompare(1) > 0 }

      val dupRes = grouped.toList.traverse_ { case (name, dups) =>
        dups.toList match {
          case Nil | (_ :: Nil) => sunit
          case (_, r1) :: (_, r2) :: rest =>
            SourceConverter.partial(
              SourceConverter.ExtDefDuplicate(name, r1, NonEmptyList(r2, rest.map(_._2))),
              ())
        }
      }

      def bindOrDef(s: Statement.ValueStatement): Option[Either[Statement.Bind, Statement.Def]] =
        s match {
          case b@Statement.Bind(_) => Some(Left(b))
          case d@Statement.Def(_) => Some(Right(d))
          case Statement.ExternalDef(_, _, _) => None
        }

      def checkDefBind(s: Statement.ValueStatement): Result[Unit] =
        bindOrDef(s) match {
          case None => sunit
          case Some(either) =>
            val names = either.fold(_.names, _.names)

            val shadows = names.filter(extDefNamesSet)
            NonEmptyList.fromList(shadows) match {
              case None => sunit
              case Some(nel) =>
                // we are shadowing
                SourceConverter.partial(
                  SourceConverter.ExtDefShadow(
                    SourceConverter.BindKind.Bind,
                    nel,
                    s.region),
                  ())
              }
        }

      dupRes *> values.traverse_(checkDefBind)
    }
  }

  /**
   * Return the lets in order they appear
   */
  private def toLets(stmts: Stream[Statement.ValueStatement]): Result[List[(Bindable, RecursionKind, Expr[Declaration])]] = {
    import Statement._

    // Each time we need a name, we can call anonNames.next()
    // it is mutable, but in a limited scope
    // this is lazy, because not all statements need anonymous names
    lazy val anonNames: Iterator[Bindable] = {
      // this is safe as a set because we only use it to filter
      val allNames =
        stmts
          .flatMap { v => v.names.iterator ++ v.allNames.iterator }
          .toSet

      unusedNames(allNames)
    }

    def bindings(
      b: Pattern[(PackageName, Constructor), Type],
      decl: Expr[Declaration]): NonEmptyList[(Bindable, Expr[Declaration])] =
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

          val tail: List[(Bindable, Expr[Declaration])] =
            complex.names.map { nm =>
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

      stmts.traverse {
        case b@Bind(BindingStatement(bound, decl, _)) =>
          val pat = unTuplePattern(bound, b.region)
          val rdec = apply(decl)
          (pat, rdec).mapN { (p, d) =>
            bindings(p, d)
              .toList
              .map { case (n, d) => (n, RecursionKind.NonRecursive, d) }
          }
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          // using body for the outer here is a bummer, but not really a good outer otherwise
          val lam = defstmt.toLambdaExpr(
            { res => apply(res.get) },
            success(defstmt.result.get))(
              unTuplePattern(_, defstmt.result.get.region),
              { t => success(toType(t)) })

          lam.map { l =>
            // We rely on DefRecursionCheck to rule out bad recursions
            val boundName = defstmt.name
            val rec =
              if (UnusedLetCheck.freeBound(l).contains(boundName)) RecursionKind.Recursive
              else RecursionKind.NonRecursive
            (boundName, rec, l) :: Nil
          }
        case ExternalDef(_, _, _) =>
          success(Nil)
      }
      .map(_.toList.flatten)
  }

  def toProgram(ss: List[Statement]): Result[Program[(TypeEnv[Variance], ParsedTypeEnv[Unit]), Expr[Declaration], List[Statement]]] = {
    val stmts = Statement.valuesOf(ss)
    val exts = stmts.collect {
      case Statement.ExternalDef(name, params, result) =>
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
          case s@rankn.Type.Var.Skolem(_, _) =>
            // $COVERAGE-OFF$ this should be unreachable
            sys.error(s"invariant violation: parsed a skolem var: $s")
            // $COVERAGE-ON$
        }
        val maybeForAll = rankn.Type.forAll(freeBound, tpe)
        (name, maybeForAll)
      }

    val pte1 = toTypeEnv.map { p =>
      exts.foldLeft(p) { case (pte, (name, tpe)) =>
        pte.addExternalValue(thisPackage, name, tpe)
      }
    }

    implicit val parallel = SourceConverter.parallelIor
    (checkExternalDefShadowing(stmts), toLets(stmts), pte1).mapN { (_, binds, pte1) =>
      Program((importedTypeEnv, pte1), binds, exts.map(_._1).toList, ss)
    }
  }
}

object SourceConverter {

  type Result[+A] = Ior[NonEmptyChain[Error], A]

  def success[A](a: A): Result[A] = Ior.Right(a)
  def partial[A](err: Error, a: A): Result[A] = Ior.Both(NonEmptyChain.one(err), a)
  def failure[A](err: Error): Result[A] = Ior.Left(NonEmptyChain.one(err))

  def addError[A](r: Result[A], err: Error): Result[A] =
    parallelIor.<*(r)(failure(err))

  // use this when we want to accumulate errors in parallel
  private val parallelIor: Applicative[Result] =
    Ior.catsDataParallelForIor[NonEmptyChain[Error]].applicative

  def apply(
    thisPackage: PackageName,
    imports: List[Import[PackageName, NonEmptyList[Referant[Variance]]]],
    localDefs: Stream[TypeDefinitionStatement]): SourceConverter =
    new SourceConverter(thisPackage, imports, localDefs)

  sealed abstract class Error {
    def region: Region
    def message: String
  }

  sealed abstract class ConstructorError extends Error {
    def name: Constructor
    def syntax: ConstructorSyntax
  }

  sealed abstract class BindKind(val asString: String)
  object BindKind {
    final case object Def extends BindKind("def")
    final case object Bind extends BindKind("bind")
  }

  final case class ExtDefShadow(kind: BindKind, names: NonEmptyList[Bindable], region: Region) extends Error {
    def message = {
      val ns = names.toList.iterator.map(_.sourceCodeRepr).mkString(", ")
      s"${kind.asString} names $ns shadow external def"
    }
  }

  final case class ExtDefDuplicate(name: Bindable, region: Region, duplicates: NonEmptyList[Region]) extends Error {
    def message = {
      s"${name.sourceCodeRepr} defined multiple times"
    }
  }

  sealed abstract class ConstructorSyntax {
    def toDoc: Doc
  }
  object ConstructorSyntax {
    final case class Pat(toPattern: Pattern.Parsed) extends ConstructorSyntax {
      def toDoc = Document[Pattern.Parsed].document(toPattern)
    }
    final case class RecCons(toDeclaration: Declaration.RecordConstructor) extends ConstructorSyntax {
      def toDoc = toDeclaration.toDoc
    }

    implicit def fromPattern(p: Pattern.Parsed): ConstructorSyntax =
      Pat(p)

    implicit def fromRC(c: Declaration.RecordConstructor): ConstructorSyntax =
      RecCons(c)
  }

  final case class UnknownConstructor(name: Constructor, syntax: ConstructorSyntax, region: Region) extends ConstructorError {
    def message = {
      val maybeDoc = syntax match {
        case ConstructorSyntax.Pat(Pattern.PositionalStruct(Pattern.StructKind.Named(n, Pattern.StructKind.Style.TupleLike), Nil)) if n == name =>
          // the pattern is just name
          Doc.empty
        case _ =>
          Doc.text(" in") + Doc.lineOrSpace + syntax.toDoc
      }
      (Doc.text(s"unknown constructor ${name.asString}") + maybeDoc).render(80)
    }
  }
  final case class InvalidArgCount(name: Constructor, syntax: ConstructorSyntax, argCount: Int, expected: Int, region: Region) extends ConstructorError {
    def message =
      (Doc.text(s"invalid argument count in ${name.asString}, found $argCount expected $expected") + Doc.lineOrSpace + syntax.toDoc).render(80)
  }
  final case class MissingArg(name: Constructor, syntax: ConstructorSyntax, present: SortedSet[Bindable], missing: Bindable, region: Region) extends ConstructorError {
    def message =
      (Doc.text(s"missing field ${missing.asString} in ${name.asString}") + Doc.lineOrSpace + syntax.toDoc).render(80)
  }
  final case class UnexpectedField(name: Constructor, syntax: ConstructorSyntax, unexpected: NonEmptyList[Bindable], expected: List[Bindable], region: Region) extends ConstructorError {
    def message = {
      val plural = if (unexpected.tail.isEmpty) "field" else "fields"
      val unexDoc = Doc.intercalate(Doc.comma + Doc.lineOrSpace, unexpected.toList.map { b => Doc.text(b.asString) })
      val exDoc = Doc.intercalate(Doc.comma + Doc.lineOrSpace, expected.map { b => Doc.text(b.asString) })
      (Doc.text(s"unexpected $plural: ") + unexDoc + Doc.lineOrSpace +
        Doc.text(s"in ${name.asString}, expected: ") + exDoc + Doc.lineOrSpace + syntax.toDoc).render(80)
      }
  }

  final case class InvalidTypeParameters(
    declaredParams: NonEmptyList[Type.Var.Bound],
    discoveredTypes: List[Type.Var.Bound],
    statement: TypeDefinitionStatement) extends Error {

    def region = statement.region
    def message = {
      def tstr(l: List[Type.Var.Bound]): String =
        l.iterator.map(_.name).mkString("[", ", ", "]")

      val decl = tstr(declaredParams.toList)
      val disc = tstr(discoveredTypes)
      s"${statement.name.asString} found declared: $decl, not a superset of $disc"
    }
  }
}
