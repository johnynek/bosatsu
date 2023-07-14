package org.bykn.bosatsu

import cats.{Applicative, Traverse}
import cats.data.{Chain, Ior, NonEmptyChain, NonEmptyList, State}
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

/** Convert a source types (a syntactic expression) into the internal
  * representations
  */
final class SourceConverter(
    thisPackage: PackageName,
    imports: List[Import[PackageName, NonEmptyList[Referant[Kind.Arg]]]],
    localDefs: List[TypeDefinitionStatement]
) {
  /*
   * We should probably error for non-predef name collisions.
   * Maybe we should even error even or predef collisions that
   * are not renamed
   */
  private val localTypeNames = localDefs.map(_.name).toSet
  private val localConstructors = localDefs.flatMap(_.constructors).toSet

  private val typeCache: MMap[Constructor, Type.Const] = MMap.empty
  private val consCache: MMap[Constructor, (PackageName, Constructor)] =
    MMap.empty

  private val importedTypes: Map[Identifier, (PackageName, TypeName)] =
    Referant.importedTypes(imports)

  private val resolveImportedCons: Map[Identifier, (PackageName, Constructor)] =
    Referant.importedConsNames(imports)

  private val importedNames: Map[Identifier, (PackageName, Identifier)] =
    imports.iterator.flatMap(_.resolveToGlobal).toMap

  val importedTypeEnv: TypeEnv[Kind.Arg] =
    Referant.importedTypeEnv(imports)(identity)

  private def nameToType(
      c: Constructor,
      region: Region
  ): Result[rankn.Type.Const] =
    typeCache.get(c) match {
      case Some(r) => success(r)
      case None =>
        val tc = TypeName(c)
        if (localTypeNames(c)) {
          val res = Type.Const.Defined(thisPackage, tc)
          typeCache.update(c, res)
          success(res)
        } else {
          importedTypes.get(c) match {
            case Some((p, t)) =>
              val res = Type.Const.Defined(p, t)
              typeCache.update(c, res)
              success(res)
            case None =>
              // this is an error
              val bestEffort = Type.Const.Defined(thisPackage, tc)
              SourceConverter.partial(
                SourceConverter.UnknownTypeName(c, region),
                bestEffort
              )
          }
        }
    }

  private def nameToCons(c: Constructor): (PackageName, Constructor) =
    consCache.getOrElseUpdate(
      c, {
        if (localConstructors(c)) (thisPackage, c)
        else resolveImportedCons.getOrElse(c, (thisPackage, c))
      }
    )

  /*
   * This ignores the name completely and just returns the lambda expression here
   */
  private def toLambdaExpr[B](
      ds: DefStatement[Pattern.Parsed, B],
      region: Region,
      tag: Result[Declaration]
  )(resultExpr: B => Result[Expr[Declaration]]): Result[Expr[Declaration]] = {
    val unTypedBody = resultExpr(ds.result)
    val bodyExp =
      ds.retType.fold(unTypedBody) { t =>
        (unTypedBody, toType(t, region), tag).parMapN(Expr.Annotation(_, _, _))
      }

    (ds.args.traverse(convertPattern(_, region)), bodyExp, tag).parMapN {
      (as, b, t) =>
        val lambda = Expr.buildPatternLambda(as, b, t)
        ds.typeArgs match {
          case None => success(lambda)
          case Some(args) =>
            val bs = args.map { case (tr, optK) =>
              (
                tr.toBoundVar,
                optK match {
                  case None    => Kind.Type
                  case Some(k) => k
                }
              )
            }
            val gen = Expr.Generic(bs, lambda)
            val freeVarsList = Expr.freeBoundTyVars(lambda)
            val freeVars = freeVarsList.toSet
            val notFreeDecl = bs.exists { case (a, _) => !freeVars(a) }
            if (notFreeDecl) {
              // we have a lint that fails if declTV is not
              // a superset of what you would derive from the args
              // the purpose here is to control the *order* of
              // and to allow introducing phantom parameters, not
              // it is confusing if some are explicit, but some are not
              SourceConverter.partial(
                SourceConverter.InvalidDefTypeParameters(
                  args,
                  freeVarsList,
                  ds,
                  region
                ),
                gen
              )
            } else success(gen)
        }
    }.flatten
  }
  private def resolveToVar[A](
      ident: Identifier,
      decl: A,
      bound: Set[Bindable],
      topBound: Set[Bindable]
  ): Expr[A] =
    ident match {
      case c @ Constructor(_) =>
        val (p, cons) = nameToCons(c)
        Expr.Global(p, cons, decl)
      case b: Bindable =>
        if (bound(b)) Expr.Local(b, decl)
        else if (topBound(b)) {
          // local top level bindings can shadow imports after they are imported
          Expr.Global(thisPackage, b, decl)
        } else {
          importedNames.get(ident) match {
            case Some((p, n)) => Expr.Global(p, n, decl)
            case None         =>
              // this is an error, but it will be caught later
              // at type-checking
              Expr.Local(b, decl)
          }
        }
    }

  private def apply(
      decl: Declaration,
      bound: Set[Bindable],
      topBound: Set[Bindable]
  ): Result[Expr[Declaration]] = {
    implicit val parAp = SourceConverter.parallelIor
    def loop(decl: Declaration) = apply(decl, bound, topBound)
    def withBound(decl: Declaration, newB: Iterable[Bindable]) =
      apply(decl, bound ++ newB, topBound)

    decl match {
      case Annotation(term, tpe) =>
        (loop(term), toType(tpe, decl.region))
          .parMapN(Expr.Annotation(_, _, decl))
      case Apply(fn, args, _) =>
        (loop(fn), args.toList.traverse(loop(_)))
          .parMapN { Expr.buildApp(_, _, decl) }
      case ao @ ApplyOp(left, op, right) =>
        val opVar: Expr[Declaration] =
          resolveToVar(op, ao.opVar, bound, topBound)
        (loop(left), loop(right)).parMapN { (l, r) =>
          Expr.buildApp(opVar, l :: r :: Nil, decl)
        }
      case Binding(BindingStatement(pat, value, Padding(_, rest))) =>
        val erest = withBound(rest, pat.names)

        def solvePat(
            pat: Pattern.Parsed,
            rrhs: Result[Expr[Declaration]]
        ): Result[Expr[Declaration]] =
          pat match {
            case Pattern.Var(arg) =>
              (erest, rrhs).parMapN { (e, rhs) =>
                Expr.Let(arg, rhs, e, RecursionKind.NonRecursive, decl)
              }
            case Pattern.Annotation(pat, tpe) =>
              toType(tpe, decl.region).flatMap { realTpe =>
                // move the annotation to the right
                val newRhs = rrhs.map(Expr.Annotation(_, realTpe, decl))
                solvePat(pat, newRhs)
              }
            case Pattern.Named(nm, p) =>
              // this is the same as creating a let nm = value first
              (solvePat(p, rrhs), rrhs).parMapN { (inner, rhs) =>
                Expr.Let(nm, rhs, inner, RecursionKind.NonRecursive, decl)
              }
            case pat =>
              // TODO: we need the region on the pattern...
              (convertPattern(pat, decl.region), erest, rrhs).parMapN {
                (newPattern, e, rhs) =>
                  val expBranches = NonEmptyList.of((newPattern, e))
                  Expr.Match(rhs, expBranches, decl)
              }
          }

        solvePat(pat, loop(value))
      case Comment(CommentStatement(_, Padding(_, decl))) =>
        loop(decl).map(_.as(decl))
      case CommentNB(CommentStatement(_, Padding(_, decl))) =>
        loop(decl).map(_.as(decl))
      case DefFn(defstmt @ DefStatement(_, _, _, _, _)) =>
        val inExpr = defstmt.result match {
          case (_, Padding(_, in)) => withBound(in, defstmt.name :: Nil)
        }
        val newBindings = defstmt.name :: defstmt.args.patternNames
        // TODO
        val lambda = toLambdaExpr(defstmt, decl.region, success(decl))({ res =>
          withBound(res._1.get, newBindings)
        })

        (inExpr, lambda).parMapN { (in, lam) =>
          // We rely on DefRecursionCheck to rule out bad recursions
          val boundName = defstmt.name
          val rec =
            if (UnusedLetCheck.freeBound(lam).contains(boundName))
              RecursionKind.Recursive
            else RecursionKind.NonRecursive
          Expr.Let(boundName, lam, in, recursive = rec, decl)
        }
      case IfElse(ifCases, elseCase) =>
        def loop0(
            ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])],
            elseC: Expr[Declaration]
        ): Expr[Declaration] =
          ifs match {
            case NonEmptyList((cond, ifTrue), Nil) =>
              Expr.ifExpr(cond, ifTrue, elseC, decl)
            case NonEmptyList(ifTrue, h :: tail) =>
              val elseC1 = loop0(NonEmptyList(h, tail), elseC)
              loop0(NonEmptyList.of(ifTrue), elseC1)
          }
        val if1 = ifCases.traverse { case (d0, d1) =>
          loop(d0).product(loop(d1.get))
        }
        val else1 = loop(elseCase.get)

        (if1, else1).parMapN(loop0(_, _))
      case tern @ Ternary(t, c, f) =>
        loop(
          IfElse(NonEmptyList((c, OptIndent.same(t)), Nil), OptIndent.same(f))(
            tern.region
          )
        )
      case Lambda(args, body) =>
        val argsRes = args.traverse(convertPattern(_, decl.region))
        val bodyRes = withBound(body, args.patternNames)
        (argsRes, bodyRes).parMapN { (args, body) =>
          Expr.buildPatternLambda(args, body, decl)
        }
      case la @ LeftApply(_, _, _, _) =>
        loop(la.rewrite).map(_.as(decl))
      case Literal(lit) =>
        success(Expr.Literal(lit, decl))
      case Parens(p) =>
        loop(p).map(_.as(decl))
      case Var(ident) =>
        success(resolveToVar(ident, decl, bound, topBound))
      case Match(_, arg, branches) =>
        /*
         * The recursion kind is only there for DefRecursionCheck, once
         * that passes, the expr only cares if lets are recursive or not
         */
        val expBranches = branches.get.traverse { case (pat, oidecl) =>
          val decl = oidecl.get
          val newPattern = convertPattern(pat, decl.region)
          newPattern.product(withBound(decl, pat.names))
        }
        (loop(arg), expBranches).parMapN(Expr.Match(_, _, decl))
      case m @ Matches(a, p) =>
        // x matches p ==
        // match x:
        //   p: True
        //   _: False
        val True: Expr[Declaration] =
          Expr.Global(PackageName.PredefName, Identifier.Constructor("True"), m)
        val False: Expr[Declaration] = Expr.Global(
          PackageName.PredefName,
          Identifier.Constructor("False"),
          m
        )
        (loop(a), convertPattern(p, m.region)).mapN { (a, p) =>
          val branches =
            NonEmptyList((p, True), (Pattern.WildCard, False) :: Nil)
          Expr.Match(a, branches, m)
        }
      case tc @ TupleCons(its) =>
        val tup0: Expr[Declaration] = Expr.Global(
          PackageName.PredefName,
          Identifier.Constructor("Unit"),
          tc
        )
        val tup2: Expr[Declaration] = Expr.Global(
          PackageName.PredefName,
          Identifier.Constructor("TupleCons"),
          tc
        )
        def tup(args: List[Declaration]): Result[Expr[Declaration]] =
          args match {
            case Nil => success(tup0)
            case h :: tail =>
              val tailExp = tup(tail)
              val headExp = loop(h)
              (headExp, tailExp).mapN { (h, t) =>
                Expr.buildApp(tup2, h :: t :: Nil, tc)
              }
          }

        tup(its)
      case s @ StringDecl(parts) =>
        // a single string item should be converted
        // to that thing,
        // two or more should be converted this to concat_String([items])
        val decls = parts.map {
          case Right((r, str)) => Literal(Lit(str))(r)
          case Left(decl)      => decl
        }

        decls match {
          case NonEmptyList(one, Nil) =>
            loop(one)
          case twoOrMore =>
            val lldecl =
              ListDecl(
                ListLang.Cons(twoOrMore.toList.map(SpliceOrItem.Item(_)))
              )(s.region)

            loop(lldecl).map { listExpr =>
              val fnName: Expr[Declaration] =
                Expr.Global(
                  PackageName.PredefName,
                  Identifier.Name("concat_String"),
                  s
                )

              Expr.buildApp(fnName, listExpr.as(s: Declaration) :: Nil, s)
            }
        }
      case l @ ListDecl(list) =>
        list match {
          case ListLang.Cons(items) =>
            val revDecs: Result[List[SpliceOrItem[Expr[Declaration]]]] =
              items.reverse.traverse {
                case SpliceOrItem.Splice(s) =>
                  loop(s).map(SpliceOrItem.Splice(_))
                case SpliceOrItem.Item(item) =>
                  loop(item).map(SpliceOrItem.Item(_))
              }

            val pn = PackageName.PredefName
            def mkC(c: String): Expr[Declaration] =
              Expr.Global(pn, Identifier.Constructor(c), l)
            def mkN(c: String): Expr[Declaration] =
              Expr.Global(pn, Identifier.Name(c), l)

            val Empty: Expr[Declaration] = mkC("EmptyList")
            def cons(
                head: Expr[Declaration],
                tail: Expr[Declaration]
            ): Expr[Declaration] =
              Expr.buildApp(mkC("NonEmptyList"), head :: tail :: Nil, l)

            def concat(
                headList: Expr[Declaration],
                tail: Expr[Declaration]
            ): Expr[Declaration] =
              Expr.buildApp(mkN("concat"), headList :: tail :: Nil, l)

            revDecs.map(_.foldLeft(Empty) {
              case (tail, SpliceOrItem.Item(i)) =>
                cons(i, tail)
              case (Empty, SpliceOrItem.Splice(s)) =>
                // concat(s, Empty) = s
                s
              case (tail, SpliceOrItem.Splice(s)) =>
                concat(s, tail)
            })
          case ListLang.Comprehension(res, binding, in, filter) =>
            /*
             * [x for y in z] ==
             * z.map_List(y ->
             *   x)
             *
             * [x for y in z if w] =
             * z.flat_map_List(y ->
             *   if w: [x]
             *   else: []
             * )
             *
             * [*x for y in z] =
             * z.flat_map_List(y ->
             *   x
             * )
             *
             * [*x for y in z if w] =
             * z.flat_map_List(y ->
             *   if w: x
             *   else: []
             * )
             */
            val pn = PackageName.PredefName
            val opName = (res, filter) match {
              case (SpliceOrItem.Item(_), None) =>
                "map_List"
              case (SpliceOrItem.Item(_) | SpliceOrItem.Splice(_), _) =>
                "flat_map_List"
            }
            val newBound = binding.names
            val opExpr: Expr[Declaration] =
              Expr.Global(pn, Identifier.Name(opName), l)
            val resExpr: Result[Expr[Declaration]] =
              filter match {
                case None       => withBound(res.value, newBound)
                case Some(cond) =>
                  // To do filters, we lift all results into lists,
                  // so single items must be made singleton lists
                  val empty: Expr[Declaration] =
                    Expr.Global(pn, Identifier.Constructor("EmptyList"), cond)
                  val ressing = res match {
                    case SpliceOrItem.Item(r) =>
                      val rdec: Declaration = r
                      // here we lift the result into a a singleton list
                      withBound(r, newBound).map { ritem =>
                        Expr.App(
                          Expr.App(
                            Expr.Global(
                              pn,
                              Identifier.Constructor("NonEmptyList"),
                              rdec
                            ),
                            ritem,
                            rdec
                          ),
                          empty,
                          rdec
                        )
                      }
                    case SpliceOrItem.Splice(r) => withBound(r, newBound)
                  }

                  (withBound(cond, newBound), ressing).mapN { (c, sing) =>
                    Expr.ifExpr(c, sing, empty, cond)
                  }
              }
            (convertPattern(binding, decl.region), resExpr, loop(in)).mapN {
              (newPattern, resExpr, in) =>
                val fnExpr: Expr[Declaration] =
                  Expr.buildPatternLambda(
                    NonEmptyList.of(newPattern),
                    resExpr,
                    l
                  )
                Expr.buildApp(opExpr, in :: fnExpr :: Nil, l)
            }
        }
      case l @ DictDecl(dict) =>
        val pn = PackageName.PredefName
        def mkN(n: String): Expr[Declaration] =
          Expr.Global(pn, Identifier.Name(n), l)
        val empty: Expr[Declaration] =
          Expr.App(mkN("empty_Dict"), mkN("string_Order"), l)

        def add(
            dict: Expr[Declaration],
            k: Expr[Declaration],
            v: Expr[Declaration]
        ): Expr[Declaration] = {
          val fn = mkN("add_key")
          Expr.buildApp(fn, dict :: k :: v :: Nil, l)
        }
        dict match {
          case ListLang.Cons(items) =>
            val revDecs: Result[List[KVPair[Expr[Declaration]]]] =
              items.reverse.traverse { case KVPair(k, v) =>
                (loop(k), loop(v)).mapN(KVPair(_, _))
              }
            revDecs.map(_.foldLeft(empty) { case (dict, KVPair(k, v)) =>
              add(dict, k, v)
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

            val newBound = binding.names
            val pn = PackageName.PredefName
            val opExpr: Expr[Declaration] =
              Expr.Global(pn, Identifier.Name("foldLeft"), l)
            val dictSymbol = unusedNames(decl.allNames).next()
            val init: Expr[Declaration] = Expr.Local(dictSymbol, l)
            val added = (withBound(k, newBound), withBound(v, newBound)).mapN(
              add(init, _, _)
            )

            val resExpr: Result[Expr[Declaration]] = filter match {
              case None => added
              case Some(cond0) =>
                (added, withBound(cond0, newBound)).mapN { (added, cond) =>
                  Expr.ifExpr(cond, added, init, cond0)
                }
            }
            val newPattern = convertPattern(binding, decl.region)
            (newPattern, resExpr, loop(in)).mapN { (pat, res, in) =>
              val foldFn = Expr.Lambda(
                dictSymbol,
                None,
                Expr.buildPatternLambda(NonEmptyList(pat, Nil), res, l),
                l
              )
              Expr.buildApp(opExpr, in :: empty :: foldFn :: Nil, l)
            }
        }
      case rc @ RecordConstructor(name, args) =>
        val (p, c) = nameToCons(name)
        val cons: Expr[Declaration] = Expr.Global(p, c, rc)
        localTypeEnv.flatMap(_.getConstructorParams(p, c) match {
          case Some(params) =>
            def argExpr(arg: RecordArg): (Bindable, Result[Expr[Declaration]]) =
              arg match {
                case RecordArg.Simple(b) =>
                  (b, success(resolveToVar(b, rc, bound, topBound)))
                case RecordArg.Pair(k, v) =>
                  (k, loop(v))
              }
            val mappingList = args.toList.map(argExpr)
            val mapping = mappingList.toMap

            lazy val present =
              mappingList.iterator
                .map(_._1)
                .foldLeft(SortedSet.empty[Bindable])(_ + _)

            def get(b: Bindable): Result[Expr[Declaration]] =
              mapping.get(b) match {
                case Some(expr) => expr
                case None =>
                  SourceConverter.failure(
                    SourceConverter.MissingArg(name, rc, present, b, rc.region)
                  )
              }
            val exprArgs = params.traverse { case (b, _) => get(b) }

            val res = exprArgs.map { args =>
              Expr.buildApp(cons, args.toList, rc)
            }
            // we also need to check that there are no unused or duplicated
            // fields
            val paramNamesList = params.map(_._1)
            val paramNames = paramNamesList.toSet
            // here are all the fields we don't understand
            val extra = mappingList.collect {
              case (k, _) if !paramNames(k) => k
            }
            // Check that the mapping is exactly the right size
            NonEmptyList.fromList(extra) match {
              case None => res
              case Some(extra) =>
                SourceConverter
                  .addError(
                    res,
                    SourceConverter.UnexpectedField(
                      name,
                      rc,
                      extra,
                      paramNamesList,
                      rc.region
                    )
                  )
            }
          case None =>
            SourceConverter.failure(
              SourceConverter.UnknownConstructor(name, rc, decl.region)
            )
        })
    }
  }

  private def toType(t: TypeRef, region: Region): Result[Type] =
    TypeRefConverter[Result](t)(nameToType(_, region))

  def toDefinition(
      pname: PackageName,
      tds: TypeDefinitionStatement
  ): Result[rankn.DefinedType[Option[Kind.Arg]]] = {
    import Statement._

    type StT = ((Set[Type.TyVar], List[Type.TyVar]), LazyList[Type.TyVar])
    type VarState[A] = State[StT, A]

    val nextVar: VarState[Type.TyVar] =
      State[StT, Type.TyVar] { case ((set, lst), vs) =>
        val vs1 = vs.dropWhile(set)
        val v = vs1.head
        val vs2 = vs1.tail
        (((set + v, v :: lst), vs2), v)
      }

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

    def buildParams(
        args: List[(Bindable, Option[Type])]
    ): VarState[List[(Bindable, Type)]] =
      args.traverse(buildParam _)

    // This is a traverse on List[(Bindable, Option[A])]
    val deep =
      Traverse[List].compose(Traverse[(Bindable, *)]).compose(Traverse[Option])

    def updateInferedWithDecl(
        typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]],
        typeParams0: List[Type.Var.Bound]
    ): Result[List[(Type.Var.Bound, Option[Kind.Arg])]] =
      typeArgs match {
        case None => success(typeParams0.map((_, None)))
        case Some(decl) =>
          val neBound = decl.map { case (v, k) => (v.toBoundVar, k) }
          val declSet = neBound.toList.iterator.map(_._1).toSet
          val missingFromDecl = typeParams0.filterNot(declSet)
          if ((declSet.size != neBound.size) || missingFromDecl.nonEmpty) {
            val bestEffort =
              neBound.toList.distinctBy(_._1) ::: missingFromDecl.map((_, None))
            // we have a lint that fails if declTV is not
            // a superset of what you would derive from the args
            // the purpose here is to control the *order* of
            // and to allow introducing phantom parameters, not
            // it is confusing if some are explicit, but some are not
            SourceConverter.partial(
              SourceConverter.InvalidTypeParameters(decl, typeParams0, tds),
              bestEffort
            )
          } else success(neBound.toList ::: missingFromDecl.map((_, None)))
      }

    tds match {
      case Struct(nm, typeArgs, args) =>
        deep
          .traverse(args)(toType(_, tds.region))
          .flatMap { argsType =>
            val declVars = typeArgs.iterator.flatMap(_.toList).map { p =>
              Type.TyVar(p._1.toBoundVar)
            }
            val initVars = existingVars(argsType)
            val initState = (
              (initVars.toSet ++ declVars, initVars.reverse),
              Type.allBinders.map(Type.TyVar)
            )
            val (((_, typeVars), _), params) =
              buildParams(argsType).run(initState).value
            // we reverse to make sure we see in traversal order
            val typeParams0 = reverseMap(typeVars) { tv =>
              tv.toVar match {
                case b @ Type.Var.Bound(_) => b
                // $COVERAGE-OFF$ this should be unreachable
                case unexpected =>
                  sys.error(s"unexpectedly parsed a non bound var: $unexpected")
                // $COVERAGE-ON$
              }
            }

            updateInferedWithDecl(typeArgs, typeParams0).map { typeParams =>
              val tname = TypeName(nm)
              val consFn = rankn.ConstructorFn(nm, params)

              rankn.DefinedType(pname, tname, typeParams, consFn :: Nil)
            }
          }
      case Enum(nm, typeArgs, items) =>
        items.get
          .traverse { case (nm, args) =>
            deep
              .traverse(args)(toType(_, tds.region))
              .map((nm, _))
          }
          .flatMap { conArgs =>
            val constructorsS = conArgs.traverse { case (nm, argsType) =>
              buildParams(argsType).map { params =>
                (nm, params)
              }
            }
            val declVars = typeArgs.iterator.flatMap(_.toList).map { p =>
              Type.TyVar(p._1.toBoundVar)
            }
            val initVars = existingVars(conArgs.toList.flatMap(_._2))
            val initState = (
              (initVars.toSet ++ declVars, initVars.reverse),
              Type.allBinders.map(Type.TyVar)
            )
            val (((_, typeVars), _), constructors) =
              constructorsS.run(initState).value
            // we reverse to make sure we see in traversal order
            val typeParams0 = reverseMap(typeVars) { tv =>
              tv.toVar match {
                case b @ Type.Var.Bound(_) => b
                // $COVERAGE-OFF$ this should be unreachable
                case unexpected =>
                  sys.error(s"unexpectedly parsed a non bound var: $unexpected")
                // $COVERAGE-ON$
              }
            }
            updateInferedWithDecl(typeArgs, typeParams0).map { typeParams =>
              val finalCons = constructors.toList.map { case (c, params) =>
                rankn.ConstructorFn(c, params)
              }
              rankn.DefinedType(pname, TypeName(nm), typeParams, finalCons)
            }
          }
      case ExternalStruct(nm, targs) =>
        // TODO make a real check here of allowed kinds
        success(
          rankn.DefinedType(
            pname,
            TypeName(nm),
            targs.map { case (TypeRef.TypeVar(v), optK) =>
              (Type.Var.Bound(v), optK)
            },
            Nil
          )
        )
    }
  }

  private def reverseMap[A, B](as: List[A])(fn: A => B): List[B] = {
    @annotation.tailrec
    def loop(as: List[A], acc: List[B]): List[B] =
      as match {
        case Nil => acc
        case a :: tail =>
          loop(tail, fn(a) :: acc)
      }

    loop(as, Nil)
  }

  private def convertPattern(
      pat: Pattern.Parsed,
      region: Region
  ): Result[Pattern[(PackageName, Constructor), rankn.Type]] = {
    val nonTupled = unTuplePattern(pat, region)
    val collisions = pat.collisionBinds
    NonEmptyList.fromList(collisions) match {
      case None => nonTupled
      case Some(nel) =>
        SourceConverter.addError(
          nonTupled,
          SourceConverter.PatternShadow(nel, pat, region)
        )
    }
  }

  private[this] val empty = Pattern.PositionalStruct(
    (PackageName.PredefName, Constructor("EmptyList")),
    Nil
  )
  private[this] val nonEmpty =
    (PackageName.PredefName, Constructor("NonEmptyList"))

  /** As much as possible, convert a list pattern into a normal enum pattern
    * which simplifies matching, and possibly allows us to more easily
    * statically remove more of the match
    */
  private def unlistPattern(
      parts: List[
        Pattern.ListPart[Pattern[(PackageName, Constructor), rankn.Type]]
      ]
  ): Pattern[(PackageName, Constructor), rankn.Type] = {
    def loop(
        parts: List[
          Pattern.ListPart[Pattern[(PackageName, Constructor), rankn.Type]]
        ],
        topLevel: Boolean
    ): Pattern[(PackageName, Constructor), rankn.Type] =
      parts match {
        case Nil => empty
        case Pattern.ListPart.Item(h) :: tail =>
          val tailPat = loop(tail, false)
          Pattern.PositionalStruct(nonEmpty, h :: tailPat :: Nil)
        case Pattern.ListPart.WildList :: Nil =>
          if (topLevel) {
            // this pattern shows we have a list of something, but we don't know what
            // changing to _ would allow more things to typecheck, which we can't do
            // and we can't annotate because we don't know the type of the list
            Pattern.ListPat(parts)
          } else {
            // we are already in the tail of a list, so we can just put _ here
            Pattern.WildCard
          }
        case Pattern.ListPart.NamedList(n) :: Nil =>
          if (topLevel) {
            // this pattern shows we have a list of something, but we don't know what
            // changing to _ would allow more things to typecheck, which we can't do
            // and we can't annotate because we don't know the type of the list
            Pattern.ListPat(parts)
          } else {
            // we are already in the tail of a list, so we can just put n here
            Pattern.Var(n)
          }
        case (Pattern.ListPart.WildList :: (i @ Pattern.ListPart.Item(
              Pattern.WildCard
            )) :: tail) =>
          // [*_, _, x...] = [_, *_, x...]
          loop(i :: Pattern.ListPart.WildList :: tail, topLevel)
        case (Pattern.ListPart.WildList | Pattern.ListPart.NamedList(_)) :: _ =>
          // this kind can't be simplified s
          Pattern.ListPat(parts)
      }

    loop(parts, true)
  }

  /** Tuples are converted into standard types using an HList strategy
    */
  private def unTuplePattern(
      pat: Pattern.Parsed,
      region: Region
  ): Result[Pattern[(PackageName, Constructor), rankn.Type]] =
    pat.traversePattern[Result, (PackageName, Constructor), rankn.Type](
      {
        case (Pattern.StructKind.Tuple, args) =>
          // this is a tuple pattern
          def loop[A](
              args: List[Pattern[(PackageName, Constructor), A]]
          ): Pattern[(PackageName, Constructor), A] =
            args match {
              case Nil =>
                // ()
                Pattern.PositionalStruct(
                  (PackageName.PredefName, Constructor("Unit")),
                  Nil
                )
              case h :: tail =>
                val tailP = loop(tail)
                Pattern.PositionalStruct(
                  (PackageName.PredefName, Constructor("TupleCons")),
                  h :: tailP :: Nil
                )
            }

          args.map(loop(_))
        case (
              Pattern.StructKind.Named(nm, Pattern.StructKind.Style.TupleLike),
              rargs
            ) =>
          rargs.flatMap { args =>
            val pc @ (p, c) = nameToCons(nm)
            localTypeEnv.flatMap(_.getConstructorParams(p, c) match {
              case Some(params) =>
                val argLen = args.size
                val paramLen = params.size
                if (argLen == paramLen) {
                  SourceConverter.success(Pattern.PositionalStruct(pc, args))
                } else {
                  // do the best we can
                  val fixedArgs =
                    (args ::: List.fill(paramLen - argLen)(Pattern.WildCard))
                      .take(paramLen)
                  SourceConverter.partial(
                    SourceConverter
                      .InvalidArgCount(nm, pat, argLen, paramLen, region),
                    Pattern.PositionalStruct(pc, fixedArgs)
                  )
                }
              case None =>
                SourceConverter.failure(
                  SourceConverter.UnknownConstructor(nm, pat, region)
                )
            })
          }
        case (
              Pattern.StructKind
                .NamedPartial(nm, Pattern.StructKind.Style.TupleLike),
              rargs
            ) =>
          rargs.flatMap { args =>
            val pc @ (p, c) = nameToCons(nm)
            localTypeEnv.flatMap(_.getConstructorParams(p, c) match {
              case Some(params) =>
                val argLen = args.size
                val paramLen = params.size
                if (argLen <= paramLen) {
                  val fixedArgs =
                    if (argLen < paramLen)
                      (args ::: List.fill(paramLen - argLen)(Pattern.WildCard))
                    else args
                  SourceConverter.success(
                    Pattern.PositionalStruct(pc, fixedArgs)
                  )
                } else {
                  // we have too many
                  val fixedArgs = args.take(paramLen)
                  SourceConverter.partial(
                    SourceConverter
                      .InvalidArgCount(nm, pat, argLen, paramLen, region),
                    Pattern.PositionalStruct(pc, fixedArgs)
                  )
                }
              case None =>
                SourceConverter.failure(
                  SourceConverter.UnknownConstructor(nm, pat, region)
                )
            })
          }
        case (
              Pattern.StructKind
                .Named(nm, Pattern.StructKind.Style.RecordLike(fs)),
              rargs
            ) =>
          rargs.flatMap { args =>
            val pc @ (p, c) = nameToCons(nm)
            localTypeEnv.flatMap(_.getConstructorParams(p, c) match {
              case Some(params) =>
                val mapping =
                  fs.toList.iterator.map(_.field).zip(args.iterator).toMap
                lazy val present =
                  SortedSet(fs.toList.iterator.map(_.field).toList: _*)
                def get(
                    b: Bindable
                ): Result[Pattern[(PackageName, Constructor), rankn.Type]] =
                  mapping.get(b) match {
                    case Some(pat) =>
                      SourceConverter.success(pat)
                    case None =>
                      SourceConverter.partial(
                        SourceConverter.MissingArg(nm, pat, present, b, region),
                        Pattern.WildCard
                      )
                  }
                val mapped =
                  params
                    .traverse { case (b, _) => get(b) }(
                      SourceConverter.parallelIor
                    )
                    .map(Pattern.PositionalStruct(pc, _))

                val paramNamesList = params.map(_._1)
                val paramNames = paramNamesList.toSet
                // here are all the fields we don't understand
                val extra =
                  fs.toList.iterator.map(_.field).filterNot(paramNames).toList
                // Check that the mapping is exactly the right size
                NonEmptyList.fromList(extra) match {
                  case None => mapped
                  case Some(extra) =>
                    SourceConverter
                      .addError(
                        mapped,
                        SourceConverter.UnexpectedField(
                          nm,
                          pat,
                          extra,
                          paramNamesList,
                          region
                        )
                      )
                }
              case None =>
                SourceConverter.failure(
                  SourceConverter.UnknownConstructor(nm, pat, region)
                )
            })
          }
        case (
              Pattern.StructKind
                .NamedPartial(nm, Pattern.StructKind.Style.RecordLike(fs)),
              rargs
            ) =>
          rargs.flatMap { args =>
            val pc @ (p, c) = nameToCons(nm)
            localTypeEnv.flatMap(_.getConstructorParams(p, c) match {
              case Some(params) =>
                val mapping =
                  fs.toList.iterator.map(_.field).zip(args.iterator).toMap
                def get(
                    b: Bindable
                ): Pattern[(PackageName, Constructor), rankn.Type] =
                  mapping.get(b) match {
                    case Some(pat) => pat
                    case None      => Pattern.WildCard
                  }
                val derefArgs = params.map { case (b, _) => get(b) }
                val res0 = SourceConverter.success(
                  Pattern.PositionalStruct(pc, derefArgs)
                )

                val paramNamesList = params.map(_._1)
                val paramNames = paramNamesList.toSet
                // here are all the fields we don't understand
                val extra =
                  fs.toList.iterator.map(_.field).filterNot(paramNames).toList
                // Check that the mapping is exactly the right size
                NonEmptyList.fromList(extra) match {
                  case None => res0
                  case Some(extra) =>
                    SourceConverter
                      .addError(
                        res0,
                        SourceConverter.UnexpectedField(
                          nm,
                          pat,
                          extra,
                          paramNamesList,
                          region
                        )
                      )
                }
              case None =>
                SourceConverter.failure(
                  SourceConverter.UnknownConstructor(nm, pat, region)
                )
            })
          }
      },
      { t => toType(t, region) },
      { items => items.map(unlistPattern) }
    )(
      SourceConverter.parallelIor
    ) // use the parallel, not the default Applicative which is Monadic

  private lazy val toTypeEnv: Result[ParsedTypeEnv[Option[Kind.Arg]]] = {
    val sunit = success(())

    val dupTypes = localDefs
      .groupByNel(_.name)
      .toList
      .traverse { case (n, tes) =>
        if (tes.tail.isEmpty) sunit
        else {
          val dupRegions = tes.map(_.region)
          SourceConverter.partial(
            SourceConverter
              .Duplication(n, SourceConverter.DupKind.TypeName, dupRegions),
            ()
          )
        }
      }

    val dupCons = localDefs
      .flatMap { ts => ts.constructors.map { c => (c, ts) } }
      .groupByNel(_._1)
      .toList
      .traverse { case (n, tes) =>
        if (tes.tail.isEmpty) sunit
        else if (tes.iterator.map(_._2.name).toSet.size == 1) {
          // these are colliding constructors, but if they also collide on type
          // name we have already reported it above
          sunit
        } else {
          val dupRegions = tes.map(_._2.region)
          SourceConverter.partial(
            SourceConverter
              .Duplication(n, SourceConverter.DupKind.Constructor, dupRegions),
            ()
          )
        }
      }

    val pd = localDefs
      .foldM(ParsedTypeEnv.empty[Option[Kind.Arg]]) { (te, d) =>
        toDefinition(thisPackage, d)
          .map(te.addDefinedType(_))
      }

    dupTypes >> dupCons >> pd
  }

  private lazy val localTypeEnv: Result[TypeEnv[Any]] =
    toTypeEnv.map { p => importedTypeEnv ++ TypeEnv.fromParsed(p) }

  private def anonNameStrings(): Iterator[String] =
    rankn.Type.allBinders.iterator
      .map(_.name)

  private def unusedNames(allNames: Bindable => Boolean): Iterator[Bindable] =
    anonNameStrings()
      .map(Identifier.Name(_))
      .filterNot(allNames)

  /** Externals are not permitted to be shadowed at the top level
    */
  private def checkExternalDefShadowing(
      values: List[Statement.ValueStatement]
  ): Result[Unit] = {
    val extDefNames =
      values.collect { case ed @ Statement.ExternalDef(name, _, _) =>
        (name, ed.region)
      }

    val sunit = success(())

    if (extDefNames.isEmpty) sunit
    else {
      val grouped = extDefNames.groupByNel(_._1)
      val extDefNamesSet = grouped.keySet

      val dupRes = grouped.toList.traverse_ { case (name, dups) =>
        dups match {
          case NonEmptyList(_, Nil) => sunit
          case NonEmptyList((_, r1), (_, r2) :: rest) =>
            SourceConverter.partial(
              SourceConverter.Duplication(
                name,
                SourceConverter.DupKind.ExtDef,
                NonEmptyList(r1, r2 :: rest.map(_._2))
              ),
              ()
            )
        }
      }

      def bindOrDef(
          s: Statement.ValueStatement
      ): Option[Either[Statement.Bind, Statement.Def]] =
        s match {
          case b @ Statement.Bind(_)          => Some(Left(b))
          case d @ Statement.Def(_)           => Some(Right(d))
          case Statement.ExternalDef(_, _, _) => None
        }

      def checkDefBind(s: Statement.ValueStatement): Result[Unit] =
        bindOrDef(s) match {
          case None => sunit
          case Some(either) =>
            val names = either.fold(_.names, _.names)

            val shadows = names.filter(extDefNamesSet)
            NonEmptyList.fromList(shadows) match {
              case None      => sunit
              case Some(nel) =>
                // we are shadowing
                SourceConverter.partial(
                  SourceConverter
                    .ExtDefShadow(SourceConverter.BindKind.Bind, nel, s.region),
                  ()
                )
            }
        }

      dupRes *> values.traverse_(checkDefBind)
    }
  }

  // Flatten pattern bindings out
  private def bindingsDecl(b: Pattern.Parsed, decl: Declaration)(
      alloc: () => Bindable
  ): NonEmptyList[(Bindable, Declaration)] =
    b match {
      case Pattern.Var(nm) =>
        NonEmptyList((nm, decl), Nil)
      case Pattern.Annotation(p, tpe) =>
        // we can just move the annotation to the expr:
        bindingsDecl(p, Annotation(decl.toNonBinding, tpe)(decl.region))(alloc)
      case Pattern.WildCard =>
        // this is silly, but maybe some kind of comment, or side-effecting
        // debug, or type checking of the rhs
        // _ = x
        // just rewrite to an anonymous variable
        val ident = alloc()
        NonEmptyList((ident, decl), Nil)
      case complex =>
        // TODO, flattening the pattern (a, b, c, d) = (1, 2, 3, 4) might be nice...
        // that is not done yet, it will allocate the tuple, just to destructure it
        val (prefix, rightHandSide) =
          if (decl.isCheap) {
            // no need to make a new var to point to a var
            (Nil, decl)
          } else {
            val ident = alloc()
            val v = Var(ident)(decl.region)
            ((ident, decl) :: Nil, v)
          }

        val rhsNB: NonBinding = rightHandSide.toNonBinding

        def makeMatch(pat: Pattern.Parsed, res: Declaration): Declaration = {
          val resOI = OptIndent.same(res)
          Match(
            RecursionKind.NonRecursive,
            rhsNB,
            OptIndent.same(NonEmptyList((pat, resOI), Nil))
          )(decl.region)
        }

        val tail: List[(Bindable, Declaration)] =
          complex.names.map { nm =>
            val pat = complex.filterVars(_ == nm)
            (nm, makeMatch(pat, Var(nm)(decl.region)))
          }

        NonEmptyList.fromList(tail) match {
          case Some(netail) =>
            SourceConverter.concat(prefix, netail)
          case None =>
            // there are no names to bind here, but we still need to typecheck the match
            val dummy = alloc()
            val pat = complex.unbind
            val unitDecl = TupleCons(Nil)(decl.region)
            val matchD = makeMatch(pat, unitDecl)
            val shapeMatch = (dummy, matchD)
            SourceConverter.concat(prefix, NonEmptyList(shapeMatch, Nil))
        }
    }

  private def parFold[F[_], S, A, B](s0: S, as: List[A])(
      fn: (S, A) => (S, F[B])
  )(implicit F: Applicative[F]): F[List[B]] = {
    val avec = as.toVector
    def loop(start: Int, end: Int, s: S): (S, F[Chain[B]]) =
      if (start >= end) (s, F.pure(Chain.empty))
      else if (start == (end - 1)) {
        val (s1, fb) = fn(s, avec(start))
        (s1, fb.map(Chain.one(_)))
      } else {
        val mid = start + (end - start) / 2
        val (s1, f1) = loop(start, mid, s)
        val (s2, f2) = loop(mid, end, s1)
        (s2, F.map2(f1, f2)(_ ++ _))
      }

    loop(0, avec.size, s0)._2.map(_.toList)
  }

  /** Return the lets in order they appear
    */
  private def toLets(
      stmts: Seq[Statement.ValueStatement]
  ): Result[List[(Bindable, RecursionKind, Expr[Declaration])]] = {
    import Statement._

    val newName: () => Bindable = {
      lazy val allNames: Set[Bindable] =
        stmts.flatMap { v => v.names.iterator ++ v.allNames.iterator }.toSet

      // Each time we need a name, we can call anonNames.next()
      // it is mutable, but in a limited scope
      // this is lazy, because not all statements need anonymous names
      lazy val anonNames: Iterator[Bindable] = unusedNames(allNames)
      () => anonNames.next()
    }

    type Flattened = Either[Def, (Bindable, Declaration)]

    val flatList: List[(Bindable, RecursionKind, Flattened)] =
      stmts.toList.flatMap {
        case d @ Def(_) =>
          (d.defstatement.name, RecursionKind.Recursive, Left(d)) :: Nil
        case ExternalDef(_, _, _) =>
          // we don't allow external defs to shadow at all, so skip it here
          Nil
        case Bind(BindingStatement(bound, decl, _)) =>
          bindingsDecl(bound, decl)(newName).toList
            .map { case pair @ (b, _) =>
              (b, RecursionKind.NonRecursive, Right(pair))
            }
      }

    val flatIn: List[(Bindable, RecursionKind, Flattened)] =
      SourceConverter.makeLetsUnique(flatList) { (bind, _) =>
        // rename all but the last item
        // TODO make a better name, close to the original, but also not colliding
        // by using idx
        val newNameV: Bindable = newName()
        val fn: Flattened => Flattened = {
          case Left(d @ Def(dstmt)) =>
            val d1 =
              if (dstmt.name === bind) dstmt.copy(name = newNameV) else dstmt
            val res =
              if (dstmt.args.iterator.flatMap(_.names).exists(_ == bind)) {
                // the args are shadowing the binding, so we don't need to substitute
                dstmt.result
              } else {
                dstmt.result.map { body =>
                  Declaration.substitute(
                    bind,
                    Var(newNameV)(body.region),
                    body
                  ) match {
                    case Some(body1) => body1
                    case None        =>
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        "we know newName can't mask"
                      )
                    // $COVERAGE-ON$
                  }
                }
              }
            Left(Def(d1.copy(result = res))(d.region))
          case Right((b0, d)) =>
            // we don't need to update b0, we discard it anyway
            Declaration.substitute(bind, Var(newNameV)(d.region), d) match {
              case Some(d1) => Right((b0, d1))
              // $COVERAGE-OFF$
              case None =>
                throw new IllegalStateException("we know newName can't mask")
              // $COVERAGE-ON$
            }
        }

        (newNameV, fn)
      }

    val withEx: List[Either[ExternalDef, Flattened]] =
      stmts.collect { case e @ ExternalDef(_, _, _) => Left(e) }.toList :::
        flatIn.map {
          case (b, _, Left(d @ Def(dstmt))) =>
            Right(Left(Def(dstmt.copy(name = b))(d.region)))
          case (b, _, Right((_, d))) => Right(Right((b, d)))
        }

    parFold(Set.empty[Bindable], withEx) { case (topBound, stmt) =>
      stmt match {
        case Right(Right((nm, decl))) =>
          val r = apply(decl, Set.empty, topBound).map(
            (nm, RecursionKind.NonRecursive, _) :: Nil
          )
          (topBound + nm, r)

        case Right(Left(d @ Def(defstmt @ DefStatement(_, _, pat, _, _)))) =>
          // using body for the outer here is a bummer, but not really a good outer otherwise

          val boundName = defstmt.name
          // defs are in scope for their body
          val topBound1 = topBound + boundName

          val lam: Result[Expr[Declaration]] =
            toLambdaExpr[OptIndent[Declaration]](
              defstmt,
              d.region,
              success(defstmt.result.get)
            )({ (res: OptIndent[Declaration]) =>
              apply(
                res.get,
                pat.iterator.flatMap(_.names).toSet + boundName,
                topBound1
              )
            })

          val r = lam.map { (l: Expr[Declaration]) =>
            // We rely on DefRecursionCheck to rule out bad recursions
            val rec =
              if (UnusedLetCheck.freeBound(l).contains(boundName))
                RecursionKind.Recursive
              else RecursionKind.NonRecursive
            (boundName, rec, l) :: Nil
          }
          (topBound1, r)
        case Left(ExternalDef(n, _, _)) =>
          (topBound + n, success(Nil))
      }
    }(SourceConverter.parallelIor)
      .map(_.flatten)
  }

  def toProgram(
      ss: List[Statement]
  ): Result[Program[(TypeEnv[Kind.Arg], ParsedTypeEnv[Option[Kind.Arg]]), Expr[
    Declaration
  ], List[Statement]]] = {
    val stmts = Statement.valuesOf(ss).toList
    stmts
      .collect { case ed @ Statement.ExternalDef(name, params, result) =>
        (
          params.traverse { p => toType(p._2, ed.region) },
          toType(result, ed.region)
        )
          .mapN { (paramTypes, resType) =>
            val tpe: rankn.Type = {
              def buildType(ts: List[rankn.Type]): rankn.Type =
                ts match {
                  case Nil       => resType
                  case h :: tail => rankn.Type.Fun(h, buildType(tail))
                }
              buildType(paramTypes)
            }

            val freeVars = rankn.Type.freeTyVars(tpe :: Nil)
            // these vars were parsed so they are never skolem vars
            val freeBound = freeVars.map {
              case b @ rankn.Type.Var.Bound(_)        => b
              case s @ rankn.Type.Var.Skolem(_, _, _) =>
                // $COVERAGE-OFF$ this should be unreachable
                sys.error(s"invariant violation: parsed a skolem var: $s")
              // $COVERAGE-ON$
            }
            // TODO: Kind support parsing kinds
            val maybeForAll =
              rankn.Type.forAll(freeBound.map { n => (n, Kind.Type) }, tpe)
            (name, maybeForAll)
          }
      }
      .sequence
      .flatMap { exts =>
        val pte1 = toTypeEnv.map { p =>
          exts.foldLeft(p) { case (pte, (name, tpe)) =>
            pte.addExternalValue(thisPackage, name, tpe)
          }
        }

        implicit val parallel = SourceConverter.parallelIor
        (checkExternalDefShadowing(stmts), toLets(stmts), pte1).mapN {
          (_, binds, pte1) =>
            Program((importedTypeEnv, pte1), binds, exts.map(_._1).toList, ss)
        }
      }
  }
}

object SourceConverter {

  type Result[+A] = Ior[NonEmptyChain[Error], A]

  def success[A](a: A): Result[A] = Ior.Right(a)
  def partial[A](err: Error, a: A): Result[A] =
    Ior.Both(NonEmptyChain.one(err), a)
  def failure[A](err: Error): Result[A] = Ior.Left(NonEmptyChain.one(err))

  def addError[A](r: Result[A], err: Error): Result[A] =
    parallelIor.<*(r)(failure(err))

  // use this when we want to accumulate errors in parallel
  private val parallelIor: Applicative[Result] =
    Ior.catsDataParallelForIor[NonEmptyChain[Error]].applicative

  def toProgram(
      thisPackage: PackageName,
      imports: List[Import[PackageName, NonEmptyList[Referant[Kind.Arg]]]],
      stmts: List[Statement]
  ): Result[Program[(TypeEnv[Kind.Arg], ParsedTypeEnv[Option[Kind.Arg]]), Expr[
    Declaration
  ], List[Statement]]] =
    (new SourceConverter(
      thisPackage,
      imports,
      Statement.definitionsOf(stmts).toList
    )).toProgram(stmts)

  private def concat[A](ls: List[A], tail: NonEmptyList[A]): NonEmptyList[A] =
    ls match {
      case Nil    => tail
      case h :: t => NonEmptyList(h, t ::: tail.toList)
    }

  /** For all duplicate binds, for all but the final value, rename them
    */
  def makeLetsUnique[D](lets: List[(Bindable, RecursionKind, D)])(
      newName: (Bindable, Int) => (Bindable, D => D)
  ): List[(Bindable, RecursionKind, D)] =
    NonEmptyList.fromList(lets) match {
      case None         => Nil
      case Some(nelets) =>
        // there is at least 1 let, but maybe no duplicates
        val dups: Map[Bindable, Int] =
          nelets
            .foldLeft(Map.empty[Bindable, Int]) { case (bound, (b, _, _)) =>
              bound.get(b) match {
                case Some(c) => bound.updated(b, c + 1)
                case None    => bound.updated(b, 1)
              }
            }
            .filter { case (_, v) => v > 1 }

        if (dups.isEmpty) {
          // no duplicated top level names
          lets
        } else {
          // we rename all but the last name for each duplicate
          type BRD = (Bindable, RecursionKind, D)

          /*
           * Invariant, lets.exists(_._1 == name) == true
           * if this is false, this method will throw
           */
          @annotation.tailrec
          def renameUntilNext(
              name: Bindable,
              lets: NonEmptyList[BRD],
              acc: List[BRD]
          )(fn: D => D): NonEmptyList[BRD] = {
            // note this is a total match:
            val NonEmptyList(head @ (b, r, d), tail) = lets

            if (b == name) {
              val head1 =
                if (r.isRecursive) {
                  // the new b is in scope right away
                  head
                } else {
                  // the old b1 is in scope for this one
                  (b, r, fn(d))
                }
              NonEmptyList(head1, acc).reverse.concat(tail)
            } else {
              // if b != name, then that implies there is
              // at least one item in the tail with b,
              // so tail cannot be empty
              val netail = NonEmptyList.fromListUnsafe(tail)
              renameUntilNext(name, netail, (b, r, fn(d)) :: acc)(fn)
            }
          }

          @annotation.tailrec
          def loop(
              lets: NonEmptyList[BRD],
              state: Map[Bindable, (Int, Int)],
              acc: List[BRD]
          ): NonEmptyList[BRD] = {
            val head = lets.head
            NonEmptyList.fromList(lets.tail) match {
              case Some(netail) =>
                val (b, r, d) = head
                state.get(b) match {
                  case Some((cnt, sz)) if cnt < (sz - 1) =>
                    val newState = state.updated(b, (cnt + 1, sz))
                    // we have to rename until the next bind
                    val (b1, renamer) = newName(b, cnt)
                    val d1 =
                      if (r.isRecursive) renamer(d)
                      else d

                    val head1 = (b1, r, d1)
                    // since cnt < (sz - 1) we know that
                    // b must occur at least once in netail
                    val tail1 = renameUntilNext(b, netail, Nil)(renamer)
                    loop(tail1, newState, head1 :: acc)
                  case _ =>
                    // this is the last one or not a duplicate, we don't change it
                    loop(netail, state, head :: acc)
                }
              case None =>
                // the last one is never renamed
                NonEmptyList(head, acc).reverse
            }
          }

          // there are duplicates
          val dupState: Map[Bindable, (Int, Int)] =
            dups.iterator.map { case (k, sz) => (k, (0, sz)) }.toMap

          loop(nelets, dupState, Nil).toList
        }
    }

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

  final case class ExtDefShadow(
      kind: BindKind,
      names: NonEmptyList[Bindable],
      region: Region
  ) extends Error {
    def message = {
      val ns = names.toList.iterator.map(_.sourceCodeRepr).mkString(", ")
      s"${kind.asString} names $ns shadow external def"
    }
  }

  sealed abstract class DupKind(val asString: String)
  object DupKind {
    case object ExtDef extends DupKind("external def")
    case object TypeName extends DupKind("type name")
    case object Constructor extends DupKind("constructor")
  }

  final case class Duplication(
      name: Identifier,
      kind: DupKind,
      duplicates: NonEmptyList[Region]
  ) extends Error {
    def region = duplicates.head
    def message =
      s"${kind.asString}: ${name.sourceCodeRepr} defined multiple times"
  }

  final case class PatternShadow(
      names: NonEmptyList[Bindable],
      pattern: Pattern.Parsed,
      region: Region
  ) extends Error {
    def message = {
      val str = names.toList.map(_.sourceCodeRepr).mkString(", ")
      "repeated bindings in pattern: " + str
    }
  }

  sealed abstract class ConstructorSyntax {
    def toDoc: Doc
  }
  object ConstructorSyntax {
    final case class Pat(toPattern: Pattern.Parsed) extends ConstructorSyntax {
      def toDoc = Document[Pattern.Parsed].document(toPattern)
    }
    final case class RecCons(toDeclaration: Declaration.RecordConstructor)
        extends ConstructorSyntax {
      def toDoc = toDeclaration.toDoc
    }

    implicit def fromPattern(p: Pattern.Parsed): ConstructorSyntax =
      Pat(p)

    implicit def fromRC(c: Declaration.RecordConstructor): ConstructorSyntax =
      RecCons(c)
  }

  final case class UnknownConstructor(
      name: Constructor,
      syntax: ConstructorSyntax,
      region: Region
  ) extends ConstructorError {
    def message = {
      val maybeDoc = syntax match {
        case ConstructorSyntax.Pat(
              Pattern.PositionalStruct(
                Pattern.StructKind.Named(n, Pattern.StructKind.Style.TupleLike),
                Nil
              )
            ) if n == name =>
          // the pattern is just name
          Doc.empty
        case _ =>
          Doc.text(" in") + Doc.lineOrSpace + syntax.toDoc
      }
      (Doc.text(s"unknown constructor ${name.asString}") + maybeDoc).render(80)
    }
  }
  final case class InvalidArgCount(
      name: Constructor,
      syntax: ConstructorSyntax,
      argCount: Int,
      expected: Int,
      region: Region
  ) extends ConstructorError {
    def message =
      (Doc.text(
        s"invalid argument count in ${name.asString}, found $argCount expected $expected"
      ) + Doc.lineOrSpace + syntax.toDoc).render(80)
  }
  final case class MissingArg(
      name: Constructor,
      syntax: ConstructorSyntax,
      present: SortedSet[Bindable],
      missing: Bindable,
      region: Region
  ) extends ConstructorError {
    def message =
      (Doc.text(
        s"missing field ${missing.asString} in ${name.asString}"
      ) + Doc.lineOrSpace + syntax.toDoc).render(80)
  }
  final case class UnexpectedField(
      name: Constructor,
      syntax: ConstructorSyntax,
      unexpected: NonEmptyList[Bindable],
      expected: List[Bindable],
      region: Region
  ) extends ConstructorError {
    def message = {
      val plural = if (unexpected.tail.isEmpty) "field" else "fields"
      val unexDoc = Doc.intercalate(
        Doc.comma + Doc.lineOrSpace,
        unexpected.toList.map { b => Doc.text(b.asString) }
      )
      val exDoc = Doc.intercalate(
        Doc.comma + Doc.lineOrSpace,
        expected.map { b => Doc.text(b.asString) }
      )
      (Doc.text(s"unexpected $plural: ") + unexDoc + Doc.lineOrSpace +
        Doc.text(
          s"in ${name.asString}, expected: "
        ) + exDoc + Doc.lineOrSpace + syntax.toDoc).render(80)
    }
  }

  final case class InvalidTypeParameters(
      declaredParams: NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])],
      discoveredTypes: List[Type.Var.Bound],
      statement: TypeDefinitionStatement
  ) extends Error {

    def region = statement.region
    def message = {
      def tstr(l: List[Type.Var.Bound]): String =
        l.iterator.map(_.name).mkString("[", ", ", "]")

      val decl =
        TypeRef
          .docTypeArgs(declaredParams.toList) {
            case None     => Doc.empty
            case Some(ka) => Doc.text(": ") + Kind.argDoc(ka)
          }
          .renderTrim(80)
      val disc = tstr(discoveredTypes)
      s"${statement.name.asString} found declared: $decl, not a superset of $disc"
    }
  }

  final case class InvalidDefTypeParameters[B](
      declaredParams: NonEmptyList[(TypeRef.TypeVar, Option[Kind])],
      free: List[Type.Var.Bound],
      defstmt: DefStatement[Pattern.Parsed, B],
      region: Region
  ) extends Error {

    def message = {
      def tstr(l: List[Type.Var.Bound]): String =
        l.iterator.map(_.name).mkString("[", ", ", "]")

      val decl = TypeRef
        .docTypeArgs(declaredParams.toList) {
          case None    => Doc.empty
          case Some(k) => Doc.text(": ") + Kind.toDoc(k)
        }
        .renderTrim(80)

      val freeStr = tstr(free)
      s"${defstmt.name.asString} found declared types: $decl, not a subset of $freeStr"
    }
  }

  final case class UnknownTypeName(tpe: Constructor, region: Region)
      extends Error {
    def message = s"unknown type: ${tpe.asString}"
  }
}
