package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.{Foldable, Monad, MonadError}
import cats.data.{NonEmptyList, ReaderT, StateT}
import cats.parse.{Parser => P}
//import cats.effect.IO
import dev.bosatsu.graph.Memoize
import dev.bosatsu.rankn.{DefinedType, Type, TypeEnv}
import dev.bosatsu.tool.CliException
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag
import scala.collection.immutable.SortedMap
import scala.annotation.unused

import Identifier.{Bindable, Constructor}

import cats.implicits._

/** convert TypedExpr to and from Protobuf representation
  */
object ProtoConverter {
  @unused private given canEqualTypeValue
      : CanEqual[proto.Type.Value, proto.Type.Value] =
    CanEqual.derived
  @unused private given canEqualPatternValue
      : CanEqual[proto.Pattern.Value, proto.Pattern.Value] =
    CanEqual.derived
  @unused private given canEqualListPartValue
      : CanEqual[proto.ListPart.Value, proto.ListPart.Value] =
    CanEqual.derived
  @unused private given canEqualStrPartValue
      : CanEqual[proto.StrPart.Value, proto.StrPart.Value] =
    CanEqual.derived
  @unused private given canEqualRecursionKind
      : CanEqual[proto.RecursionKind, proto.RecursionKind] =
    CanEqual.derived
  @unused private given canEqualTypedExprValue
      : CanEqual[proto.TypedExpr.Value, proto.TypedExpr.Value] =
    CanEqual.derived
  @unused private given canEqualLiteralValue
      : CanEqual[proto.Literal.Value, proto.Literal.Value] =
    CanEqual.derived
  @unused private given canEqualVariance
      : CanEqual[proto.Variance, proto.Variance] =
    CanEqual.derived
  @unused private given canEqualKindValue
      : CanEqual[proto.Kind.Value, proto.Kind.Value] =
    CanEqual.derived
  @unused private given canEqualDefinedTypeRefValue: CanEqual[
    proto.DefinedTypeReference.Value,
    proto.DefinedTypeReference.Value
  ] =
    CanEqual.derived
  @unused private given canEqualConstructorRefValue: CanEqual[
    proto.ConstructorReference.Value,
    proto.ConstructorReference.Value
  ] =
    CanEqual.derived
  @unused private given canEqualReferantValue
      : CanEqual[proto.Referant.Referant, proto.Referant.Referant] =
    CanEqual.derived
  @unused private given canEqualExportKind
      : CanEqual[proto.ExportKind, proto.ExportKind] =
    CanEqual.derived
  case class NameParseError(name: String, message: String, error: P.Error)
      extends Exception(message)

  case class IdAssignment[A1, A2](mapping: Map[A1, Int], inOrder: Vector[A2]) {
    def get(a1: A1, a2: => A2): Either[(IdAssignment[A1, A2], Int), Int] =
      mapping.get(a1) match {
        case Some(id) => Right(id)
        case None     =>
          val id = inOrder.size
          val next =
            copy(mapping = mapping.updated(a1, id), inOrder = inOrder :+ a2)
          Left((next, id))
      }

    def indexOf(a1: A1): Option[Int] =
      mapping.get(a1)
  }

  object IdAssignment {
    def empty[A1, A2]: IdAssignment[A1, A2] =
      IdAssignment(Map.empty, Vector.empty)
  }

  case class SerState(
      strings: IdAssignment[String, String],
      types: IdAssignment[Type, proto.Type],
      patterns: IdAssignment[
        Pattern[(PackageName, Constructor), Type],
        proto.Pattern
      ],
      expressions: IdAssignment[TypedExpr[Any], proto.TypedExpr]
  ) {

    def stringId(s: String): Either[(SerState, Int), Int] =
      strings.get(s, s).left.map { case (next, id) =>
        (copy(strings = next), id)
      }

    def typeId(
        t: Type,
        protoType: => proto.Type
    ): Either[(SerState, Int), Int] =
      types.get(t, protoType).left.map { case (next, id) =>
        (copy(types = next), id)
      }
  }

  object SerState {
    val empty: SerState =
      SerState(
        IdAssignment.empty,
        IdAssignment.empty,
        IdAssignment.empty,
        IdAssignment.empty
      )
  }

  type Tab[A] = StateT[Try, SerState, A]

  implicit class TabMethods[A](val self: Tab[A]) extends AnyVal {
    def onFailPrint(message: => String): Tab[A] =
      self.runF match {
        case Success(fn) =>
          StateT(fn.andThen { next =>
            if (next.isFailure) System.err.println(message)
            next
          })

        case Failure(_) =>
          System.err.println(message)
          self
      }
  }

  private def tabFail[S, A](ex: Exception): Tab[A] =
    MonadError[Tab, Throwable].raiseError(ex)
  private def tabPure[S, A](a: A): Tab[A] =
    Monad[Tab].pure(a)

  private def get(fn: SerState => Either[(SerState, Int), Int]): Tab[Int] =
    StateT
      .get[Try, SerState]
      .flatMap { ss =>
        fn(ss) match {
          case Right(idx)      => StateT.pure(idx + 1)
          case Left((ss, idx)) =>
            StateT.set[Try, SerState](ss).as(idx + 1)
        }
      }

  private def getId(s: String): Tab[Int] = get(_.stringId(s))

  private def getTypeId(t: Type, pt: => proto.Type): Tab[Int] =
    get(_.typeId(t, pt))

  private def getProtoTypeTab(t: Type): Tab[Option[Int]] =
    StateT
      .get[Try, SerState]
      .map(_.types.indexOf(t).map(_ + 1))

  private def writePattern(
      p: Pattern[(PackageName, Constructor), Type],
      pp: proto.Pattern
  ): Tab[Int] =
    StateT
      .get[Try, SerState]
      .flatMap { s =>
        s.patterns.get(p, pp) match {
          case Right(_) =>
            // this is a programming error in this code, if this is hit
            tabFail(new Exception(s"expected $p to be absent"))
          case Left((next, id)) =>
            StateT.set[Try, SerState](s.copy(patterns = next)).as(id + 1)
        }
      }

  private def writeExpr(te: TypedExpr[Any], pte: proto.TypedExpr): Tab[Int] =
    StateT
      .get[Try, SerState]
      .flatMap { s =>
        s.expressions.get(te, pte) match {
          case Right(_) =>
            // this is a programming error in this code, if this is hit
            tabFail(new Exception(s"expected $te to be absent"))
          case Left((next, id)) =>
            StateT.set[Try, SerState](s.copy(expressions = next)).as(id + 1)
        }
      }

  def runTab[A](t: Tab[A]): Try[(SerState, A)] =
    t.run(SerState.empty)

  class DecodeState private (
      strings: Array[String],
      types: Array[Type],
      dts: Array[DefinedType[Kind.Arg]],
      patterns: Array[Pattern[(PackageName, Constructor), Type]],
      expr: Array[TypedExpr[Unit]]
  ) {
    def getString(idx: Int): Option[String] =
      if ((0 <= idx) && (idx < strings.length)) Some(strings(idx))
      else None

    def tryString(idx: Int, msg: => String): Try[String] =
      if ((0 <= idx) && (idx < strings.length)) Success(strings(idx))
      else Failure(new Exception(msg))

    def getType(idx: Int): Option[Type] =
      if ((0 <= idx) && (idx < types.length)) Some(types(idx))
      else None

    def tryType(idx: Int, msg: => String): Try[Type] =
      if ((0 <= idx) && (idx < types.length)) Success(types(idx))
      else Failure(new Exception(msg))

    def tryPattern(
        idx: Int,
        msg: => String
    ): Try[Pattern[(PackageName, Constructor), Type]] =
      if ((0 <= idx) && (idx < patterns.length)) Success(patterns(idx))
      else Failure(new Exception(msg))

    def getDt(idx: Int): Option[DefinedType[Kind.Arg]] =
      if ((0 <= idx) && (idx < dts.length)) Some(dts(idx))
      else None

    def getDefinedTypes: List[DefinedType[Kind.Arg]] =
      dts.toList

    def getExpr(idx: Int): Option[TypedExpr[Unit]] =
      if ((0 <= idx) && (idx < expr.length)) Some(expr(idx))
      else None

    def withDefinedTypes(vdts: Seq[DefinedType[Kind.Arg]]): DecodeState =
      new DecodeState(strings, types, vdts.toArray, patterns, expr)

    def withTypes(ary: Array[Type]): DecodeState =
      new DecodeState(strings, ary, dts, patterns, expr)

    def withPatterns(
        ary: Array[Pattern[(PackageName, Constructor), Type]]
    ): DecodeState =
      new DecodeState(strings, types, dts, ary, expr)

    def withExprs(ary: Array[TypedExpr[Unit]]): DecodeState =
      new DecodeState(strings, types, dts, patterns, ary)
  }

  object DecodeState {
    def init(strings: Seq[String]): DecodeState =
      new DecodeState(
        strings.toArray,
        Array.empty,
        Array.empty,
        Array.empty,
        Array.empty
      )
  }

  type DTab[A] = ReaderT[Try, DecodeState, A]

  private def find[A](idx: Int, context: => String)(
      fn: (DecodeState, Int) => Option[A]
  ): DTab[A] =
    ReaderT { decodeState =>
      fn(decodeState, idx - 1) match {
        case Some(s) => Success(s)
        case None => Failure(new Exception(s"invalid index: $idx in $context"))
      }
    }

  private def lookup(idx: Int, context: => String): DTab[String] =
    find(idx, context)(_.getString(_))

  private def lookupType(idx: Int, context: => String): DTab[Type] =
    find(idx, context)(_.getType(_))

  private def lookupDts(
      idx: Int,
      context: => String
  ): DTab[DefinedType[Kind.Arg]] =
    find(idx, context)(_.getDt(_))

  private def lookupExpr(idx: Int, context: => String): DTab[TypedExpr[Unit]] =
    find(idx, context)(_.getExpr(_))

  /** this is code to build tables of serialized dags. We use this for types,
    * patterns, expressions
    */
  private def buildTable[A, B: ClassTag](
      ary: Array[A]
  )(fn: (A, Int => Try[B]) => Try[B]): Try[Array[B]] = {
    val result = new Array[B](ary.length)
    def lookup(a: A, max: Int): Int => Try[B] = { idx =>
      if (idx > 0 && idx <= max) Success(result(idx - 1))
      else
        Failure(
          new Exception(s"while decoding $a, invalid index $idx, max: $max")
        )
    }

    var idx = 0
    var res: Failure[Array[B]] | Null = null
    while ((idx < ary.length) && (res eq null)) {
      val a = ary(idx)
      val lookupFn = lookup(a, idx)
      fn(a, lookupFn) match {
        case Success(b)   => result(idx) = b
        case Failure(err) => res = Failure(err)
      }
      idx = idx + 1
    }
    if (res eq null) Success(result)
    else res
  }

  def buildTypes(types: Seq[proto.Type]): DTab[Array[Type]] =
    ReaderT[Try, DecodeState, Array[Type]] { ds =>
      def typeFromProto(p: proto.Type, tpe: Int => Try[Type]): Try[Type] = {
        import proto.Type.Value
        import bosatsu.TypedAst.{Type => _, _}

        def str(i: Int): Try[String] =
          ds.tryString(i - 1, s"invalid string idx: $i in $p")

        def varKindFromProto(vk: proto.VarKind) =
          (str(vk.varName), kindFromProto(vk.kind))
            .mapN((n, k) => (Type.Var.Bound(n), k))

        p.value match {
          case Value.Empty => Failure(new Exception(s"empty type found in $p"))
          case Value.TypeConst(tc) =>
            val proto.TypeConst(packidx, tidx, _) = tc
            str(packidx)
              .product(str(tidx))
              .flatMap { case (pack, t) =>
                typeConstFromStr(pack, t, p.toString)
                  .map(Type.TyConst(_))
              }
          case Value.TypeVar(tv) =>
            str(tv.varName).map(n => Type.TyVar(Type.Var.Bound(n)))
          case Value.TypeForAll(TypeForAll(varKinds, in, _)) =>
            for {
              inT <- tpe(in)
              args <- varKinds.toList.traverse(varKindFromProto)
            } yield Type.forAll(args, inT)

          case Value.TypeExists(TypeExists(varKinds, in, _)) =>
            for {
              inT <- tpe(in)
              args <- varKinds.toList.traverse(varKindFromProto)
            } yield Type.exists(args, inT)

          case Value.TypeApply(TypeApply(left, right, _)) =>
            (tpe(left), tpe(right)).mapN(Type.apply1(_, _))
        }
      }

      buildTable(types.toArray)(typeFromProto)
    }

  def buildPatterns(
      pats: Seq[proto.Pattern]
  ): DTab[Array[Pattern[(PackageName, Constructor), Type]]] =
    ReaderT[
      Try,
      DecodeState,
      Array[Pattern[(PackageName, Constructor), Type]]
    ] { ds =>
      def patternFromProto(
          p: proto.Pattern,
          pat: Int => Try[Pattern[(PackageName, Constructor), Type]]
      ): Try[Pattern[(PackageName, Constructor), Type]] = {
        import proto.Pattern.Value

        def str(i: Int): Try[String] =
          ds.tryString(i - 1, s"invalid string idx: $i in $p")

        def bindable(i: Int): Try[Bindable] =
          str(i).flatMap(toBindable)

        p.value match {
          case Value.Empty => Failure(new Exception("invalid unset pattern"))
          case Value.WildPat(_) => Success(Pattern.WildCard)
          case Value.LitPat(l)  =>
            litFromProto(l).map(Pattern.Literal(_))
          case Value.VarNamePat(sidx) => bindable(sidx).map(Pattern.Var(_))
          case Value.NamedPat(proto.NamedPat(nidx, pidx, _)) =>
            (bindable(nidx), pat(pidx)).mapN(Pattern.Named(_, _))
          case Value.ListPat(proto.ListPat(lp, _)) =>
            def decodePart(part: proto.ListPart): Try[
              Pattern.ListPart[Pattern[(PackageName, Constructor), Type]]
            ] =
              part.value match {
                case proto.ListPart.Value.Empty =>
                  Failure(new Exception(s"invalid empty list pattern in $p"))
                case proto.ListPart.Value.ItemPattern(p) =>
                  pat(p).map(Pattern.ListPart.Item(_))
                case proto.ListPart.Value.UnnamedList(_) =>
                  Success(Pattern.ListPart.WildList)
                case proto.ListPart.Value.NamedList(idx) =>
                  bindable(idx).map(n => Pattern.ListPart.NamedList(n))
              }

            lp.toList.traverse(decodePart).map(Pattern.ListPat(_))
          case Value.StrPat(proto.StrPat(items, _)) =>
            def decodePart(part: proto.StrPart): Try[Pattern.StrPart] =
              part.value match {
                case proto.StrPart.Value.Empty =>
                  Failure(new Exception(s"invalid empty list pattern in $p"))
                case proto.StrPart.Value.LiteralStr(idx) =>
                  str(idx).map(Pattern.StrPart.LitStr(_))
                case proto.StrPart.Value.UnnamedStr(_) =>
                  Success(Pattern.StrPart.WildStr)
                case proto.StrPart.Value.NamedStr(idx) =>
                  bindable(idx).map(n => Pattern.StrPart.NamedStr(n))
                case proto.StrPart.Value.UnnamedChar(_) =>
                  Success(Pattern.StrPart.WildChar)
                case proto.StrPart.Value.NamedChar(idx) =>
                  bindable(idx).map(n => Pattern.StrPart.NamedChar(n))
              }

            items.toList match {
              case Nil =>
                // this is invalid
                Failure(new Exception(s"empty string pattern found in $p"))
              case h :: tail =>
                NonEmptyList(h, tail)
                  .traverse(decodePart)
                  .map(Pattern.StrPat(_))
            }
          case Value.AnnotationPat(proto.AnnotationPat(pidx, tidx, _)) =>
            (
              pat(pidx),
              ds.tryType(tidx - 1, s"invalid type index $tidx in: $p")
            )
              .mapN(Pattern.Annotation(_, _))
          case Value.StructPat(proto.StructPattern(packIdx, cidx, args, _)) =>
            str(packIdx)
              .product(str(cidx))
              .flatMap { case (p, c) =>
                fullNameFromStr(p, c, s"invalid structpat names: $p, $c")
              }
              .flatMap { pc =>
                args.toList.traverse(pat).map(Pattern.PositionalStruct(pc, _))
              }
          case Value.UnionPat(proto.UnionPattern(pats, _)) =>
            pats.toList match {
              case p0 :: p1 :: prest =>
                (pat(p0), pat(p1), prest.traverse(pat))
                  .mapN { (p0, p1, prest) =>
                    Pattern.Union(p0, NonEmptyList(p1, prest))
                  }

              case notTwo =>
                Failure(
                  new Exception(
                    s"invalid union found size: ${notTwo.size}, expected 2 or more"
                  )
                )
            }
        }
      }

      buildTable(pats.toArray)(patternFromProto)
    }

  def recursionKindFromProto(
      rec: proto.RecursionKind,
      context: => String
  ): Try[RecursionKind] =
    rec match {
      case proto.RecursionKind.NotRec => Success(RecursionKind.NonRecursive)
      case proto.RecursionKind.IsRec  => Success(RecursionKind.Recursive)
      case other                      =>
        Failure(new Exception(s"invalid recursion kind: $other, in $context"))
    }

  def buildExprs(exprs: Seq[proto.TypedExpr]): DTab[Array[TypedExpr[Unit]]] =
    ReaderT[Try, DecodeState, Array[TypedExpr[Unit]]] { ds =>
      def expressionFromProto(
          ex: proto.TypedExpr,
          exprOf: Int => Try[TypedExpr[Unit]]
      ): Try[TypedExpr[Unit]] = {
        import proto.TypedExpr.Value

        def str(i: Int): Try[String] =
          ds.tryString(i - 1, s"invalid string idx: $i in $ex")

        def bindable(i: Int): Try[Bindable] =
          str(i).flatMap(toBindable)

        def ident(i: Int): Try[Identifier] =
          str(i).flatMap(toIdent)

        def typeOf(i: Int): Try[Type] =
          ds.tryType(i - 1, s"invalid type id in $ex")

        ex.value match {
          case Value.Empty => Failure(new Exception("invalid empty TypedExpr"))
          case Value.GenericExpr(proto.GenericExpr(forAlls, exists, expr, _)) =>
            val faList = forAlls.traverse { varKind =>
              (str(varKind.varName), kindFromProto(varKind.kind))
                .mapN((n, k) => (Type.Var.Bound(n), k))
            }

            val exList = exists.traverse { varKind =>
              (str(varKind.varName), kindFromProto(varKind.kind))
                .mapN((n, k) => (Type.Var.Bound(n), k))
            }

            (faList, exList, exprOf(expr))
              .mapN { (fa, ex, e) =>
                TypedExpr.Quantification.fromLists(
                  forallList = fa.toList,
                  existList = ex.toList
                ) match {
                  case Some(q) => TypedExpr.Generic(q, e)
                  case None    => e
                }
              }
          case Value.AnnotationExpr(proto.AnnotationExpr(expr, tpe, _)) =>
            (exprOf(expr), typeOf(tpe))
              .mapN(TypedExpr.Annotation(_, _))
          case Value.LambdaExpr(proto.LambdaExpr(varsName, varsTpe, expr, _)) =>
            (
              varsName.traverse(bindable(_)),
              varsTpe.traverse(typeOf(_)),
              exprOf(expr)
            )
              .flatMapN { (vs, ts, e) =>
                val vsLen = vs.length
                if (vsLen <= 0) {
                  Failure(new Exception(s"no bind names in this lambda: $ex"))
                } else if (vsLen == ts.length) {
                  // we know length > 0 and they match
                  val args = NonEmptyList.fromListUnsafe(
                    vs.iterator.zip(ts.iterator).toList
                  )
                  Success(TypedExpr.AnnotatedLambda(args, e, ()))
                } else {
                  Failure(
                    new Exception(
                      s"type list length didn't match bind name length in $ex"
                    )
                  )
                }
              }
          case Value.VarExpr(proto.VarExpr(pack, varname, tpe, _)) =>
            val tryPack =
              if (pack == 0) Success(None)
              else
                for {
                  ps <- str(pack)
                  pack <- parsePack(ps, s"expression: $ex")
                } yield Some(pack)

            (tryPack, typeOf(tpe)).tupled
              .flatMap {
                case (None, tpe) =>
                  bindable(varname).map(TypedExpr.Local(_, tpe, ()))
                case (Some(p), tpe) =>
                  ident(varname).map(TypedExpr.Global(p, _, tpe, ()))
              }
          case Value.AppExpr(proto.AppExpr(fn, args, resTpe, _)) =>
            (exprOf(fn), args.traverse(exprOf(_)), typeOf(resTpe))
              .flatMapN { case (fn, args, res) =>
                NonEmptyList.fromList(args.toList) match {
                  case Some(args) =>
                    Success(TypedExpr.App(fn, args, res, ()))
                  case None =>
                    Failure(new Exception(s"no arguments to apply: $ex"))
                }
              }
          case Value.LetExpr(proto.LetExpr(nm, nmexpr, inexpr, rec, _)) =>
            val tryRec = recursionKindFromProto(rec, ex.toString)
            (bindable(nm), exprOf(nmexpr), exprOf(inexpr), tryRec)
              .mapN(TypedExpr.Let(_, _, _, _, ()))
          case Value.LoopExpr(proto.LoopExpr(args, bodyExpr, _)) =>
            def decodeArg(
                arg: proto.LoopArg
            ): Try[(Bindable, TypedExpr[Unit])] =
              (bindable(arg.varName), exprOf(arg.initExpr)).tupled

            NonEmptyList.fromList(args.toList) match {
              case Some(nel) =>
                (nel.traverse(decodeArg), exprOf(bodyExpr))
                  .mapN(TypedExpr.Loop(_, _, ()))
              case None =>
                Failure(new Exception(s"invalid empty loop args in $ex"))
            }
          case Value.RecurExpr(proto.RecurExpr(args, tpe, _)) =>
            (args.toList.traverse(exprOf), typeOf(tpe))
              .flatMapN { (as, tpe) =>
                NonEmptyList.fromList(as) match {
                  case Some(nel) =>
                    Success(TypedExpr.Recur(nel, tpe, ()))
                  case None =>
                    Failure(new Exception(s"invalid empty recur args in $ex"))
                }
              }
          case Value.LiteralExpr(proto.LiteralExpr(lit, tpe, _)) =>
            lit match {
              case None =>
                Failure(new Exception(s"invalid missing literal in $ex"))
              case Some(lit) =>
                (litFromProto(lit), typeOf(tpe))
                  .mapN(TypedExpr.Literal(_, _, ()))
            }
          case Value.MatchExpr(proto.MatchExpr(argId, branches, _)) =>
            def buildBranch(b: proto.Branch): Try[
              (Pattern[(PackageName, Constructor), Type], TypedExpr[Unit])
            ] =
              (
                ds.tryPattern(b.pattern - 1, s"invalid pattern in $ex"),
                exprOf(b.resultExpr)
              ).tupled

            NonEmptyList.fromList(branches.toList) match {
              case Some(nel) =>
                (exprOf(argId), nel.traverse(buildBranch))
                  .mapN(TypedExpr.Match(_, _, ()))
              case None =>
                Failure(new Exception(s"invalid empty branches in $ex"))
            }
        }
      }

      buildTable(exprs.toArray)(expressionFromProto)
    }

  private def parsePack(pstr: String, context: => String): Try[PackageName] =
    PackageName.parse(pstr) match {
      case None =>
        Failure(new Exception(s"invalid package name: $pstr, in $context"))
      case Some(pack) => Success(pack)
    }

  private def fullNameFromStr(
      pstr: String,
      tstr: String,
      context: => String
  ): Try[(PackageName, Constructor)] =
    (parsePack(pstr, context), toConstructor(tstr)).tupled

  def typeConstFromStr(
      pstr: String,
      tstr: String,
      context: => String
  ): Try[Type.Const.Defined] =
    fullNameFromStr(pstr, tstr, context).map { case (p, c) =>
      Type.Const.Defined(p, TypeName(c))
    }

  def typeConstFromProto(p: proto.TypeConst): DTab[Type.Const.Defined] = {
    val proto.TypeConst(packidx, tidx, _) = p
    lookup(packidx, s"package in: $p")
      .product(lookup(tidx, s"type in: $p"))
      .flatMapF { case (pack, t) => typeConstFromStr(pack, t, p.toString) }
  }

  def typeConstToProto(tc: Type.Const): Tab[proto.TypeConst] =
    tc match {
      case Type.Const.Defined(p, n) =>
        for {
          pidx <- getId(p.asString)
          nidx <- getId(n.ident.sourceCodeRepr)
        } yield proto.TypeConst(pidx, nidx)
    }

  def typeVarBoundToProto(tvb: Type.Var.Bound): Tab[proto.TypeVar] =
    getId(tvb.name).map(proto.TypeVar(_))

  def typeVarBoundFromProto(tv: proto.TypeVar): DTab[Type.Var.Bound] =
    lookup(tv.varName, s"typevar: $tv").map(Type.Var.Bound(_))

  def typeToProto(p: Type): Tab[Int] = {
    import proto.Type.Value
    import bosatsu.TypedAst.{Type => _, _}

    getProtoTypeTab(p)
      .flatMap {
        case Some(p) => tabPure(p)
        case None    =>
          p match {
            case Type.ForAll(vars, in) =>
              (vars.toList.traverse { case (b, k) => varKindToProto(b, k) }, typeToProto(in))
                .flatMapN { (faids, idx) =>
                  getTypeId(
                    p,
                    proto.Type(Value.TypeForAll(TypeForAll(faids, idx)))
                  )
                }
            case Type.Exists(vars, in) =>
              (vars.toList.traverse { case (b, k) => varKindToProto(b, k) }, typeToProto(in))
                .flatMapN { (exids, idx) =>
                  getTypeId(
                    p,
                    proto.Type(Value.TypeExists(TypeExists(exids, idx)))
                  )
                }
            case Type.TyApply(on, arg) =>
              typeToProto(on)
                .product(typeToProto(arg))
                .flatMap { case (li, ri) =>
                  getTypeId(p, proto.Type(Value.TypeApply(TypeApply(li, ri))))
                }
            case Type.TyConst(tcd) =>
              typeConstToProto(tcd)
                .flatMap { pt =>
                  getTypeId(p, proto.Type(Value.TypeConst(pt)))
                }
            case Type.TyVar(Type.Var.Bound(n)) =>
              getId(n).flatMap { id =>
                getTypeId(p, proto.Type(Value.TypeVar(TypeVar(id))))
              }
            case Type.TyVar(Type.Var.Skolem(_, _, _, _)) | Type.TyMeta(_) =>
              tabFail(new Exception(s"invalid type to serialize: $p"))
          }
      }
  }

  def litToProto(l: Lit): proto.Literal = {
    val protoLit = l match {
      case Lit.Integer(i) =>
        try {
          proto.Literal.Value.IntValueAs64(i.longValueExact)
        } catch {
          case _: ArithmeticException =>
            proto.Literal.Value.IntValueAsString(i.toString)
        }
      case c @ Lit.Chr(_) =>
        proto.Literal.Value.CharValue(c.toCodePoint)
      case Lit.Str(str) =>
        proto.Literal.Value.StringValue(str)
      case f: Lit.Float64 =>
        proto.Literal.Value.Float64ValueAsBits(f.toRawLongBits)
    }
    proto.Literal(protoLit)
  }

  def litFromProto(l: proto.Literal): Try[Lit] =
    l.value match {
      case proto.Literal.Value.Empty =>
        Failure(new Exception("unexpected unset Literal value in pattern"))
      case proto.Literal.Value.StringValue(s) =>
        Success(Lit.Str(s))
      case proto.Literal.Value.CharValue(cp) =>
        Success(Lit.Chr.fromCodePoint(cp))
      case proto.Literal.Value.IntValueAs64(l) =>
        Success(Lit(l))
      case proto.Literal.Value.IntValueAsString(s) =>
        Success(Lit.Integer(new java.math.BigInteger(s)))
      case proto.Literal.Value.Float64ValueAsBits(bits) =>
        Success(Lit.Float64.fromRawLongBits(bits))
    }

  def patternToProto(p: Pattern[(PackageName, Constructor), Type]): Tab[Int] =
    StateT
      .get[Try, SerState]
      .map(_.patterns.indexOf(p))
      .flatMap {
        case Some(idx) => tabPure(idx + 1)
        case None      =>
          p match {
            case Pattern.WildCard =>
              writePattern(
                p,
                proto.Pattern(proto.Pattern.Value.WildPat(proto.WildCardPat()))
              )
            case Pattern.Literal(lit) =>
              val litP = litToProto(lit)
              writePattern(p, proto.Pattern(proto.Pattern.Value.LitPat(litP)))
            case Pattern.Var(n) =>
              getId(n.sourceCodeRepr)
                .flatMap { idx =>
                  writePattern(
                    p,
                    proto.Pattern(proto.Pattern.Value.VarNamePat(idx))
                  )
                }
            case named @ Pattern.Named(n, p) =>
              getId(n.sourceCodeRepr)
                .product(patternToProto(p))
                .flatMap { case (idx, pidx) =>
                  writePattern(
                    named,
                    proto.Pattern(
                      proto.Pattern.Value.NamedPat(proto.NamedPat(idx, pidx))
                    )
                  )
                }
            case Pattern.StrPat(parts) =>
              parts
                .traverse {
                  case Pattern.StrPart.WildStr =>
                    tabPure(
                      proto.StrPart(
                        proto.StrPart.Value.UnnamedStr(proto.WildCardPat())
                      )
                    )
                  case Pattern.StrPart.WildChar =>
                    tabPure(
                      proto.StrPart(
                        proto.StrPart.Value.UnnamedChar(proto.WildCardPat())
                      )
                    )
                  case Pattern.StrPart.NamedStr(n) =>
                    getId(n.sourceCodeRepr).map { idx =>
                      proto.StrPart(proto.StrPart.Value.NamedStr(idx))
                    }
                  case Pattern.StrPart.NamedChar(n) =>
                    getId(n.sourceCodeRepr).map { idx =>
                      proto.StrPart(proto.StrPart.Value.NamedChar(idx))
                    }
                  case Pattern.StrPart.LitStr(s) =>
                    getId(s).map { idx =>
                      proto.StrPart(proto.StrPart.Value.LiteralStr(idx))
                    }
                }
                .flatMap { parts =>
                  writePattern(
                    p,
                    proto.Pattern(
                      proto.Pattern.Value.StrPat(proto.StrPat(parts.toList))
                    )
                  )
                }
            case Pattern.ListPat(items) =>
              items
                .traverse {
                  case Pattern.ListPart.Item(itemPat) =>
                    patternToProto(itemPat).map { pidx =>
                      proto.ListPart(proto.ListPart.Value.ItemPattern(pidx))
                    }
                  case Pattern.ListPart.WildList =>
                    tabPure(
                      proto.ListPart(
                        proto.ListPart.Value.UnnamedList(proto.WildCardPat())
                      )
                    )
                  case Pattern.ListPart.NamedList(bindable) =>
                    getId(bindable.sourceCodeRepr).map { idx =>
                      proto.ListPart(proto.ListPart.Value.NamedList(idx))
                    }
                }
                .flatMap { parts =>
                  writePattern(
                    p,
                    proto.Pattern(
                      proto.Pattern.Value.ListPat(proto.ListPat(parts))
                    )
                  )
                }
            case ann @ Pattern.Annotation(p, tpe) =>
              patternToProto(p)
                .product(typeToProto(tpe))
                .flatMap { case (pidx, tidx) =>
                  writePattern(
                    ann,
                    proto.Pattern(
                      proto.Pattern.Value
                        .AnnotationPat(proto.AnnotationPat(pidx, tidx))
                    )
                  )
                }
            case pos @ Pattern.PositionalStruct((packName, consName), params) =>
              typeConstToProto(Type.Const.Defined(packName, TypeName(consName)))
                .flatMap { ptc =>
                  params
                    .traverse(patternToProto)
                    .flatMap { parts =>
                      writePattern(
                        pos,
                        proto.Pattern(
                          proto.Pattern.Value.StructPat(
                            proto.StructPattern(
                              packageName = ptc.packageName,
                              constructorName = ptc.typeName,
                              params = parts
                            )
                          )
                        )
                      )
                    }
                }

            case Pattern.Union(h, t) =>
              (h :: t.toList)
                .traverse(patternToProto)
                .flatMap { us =>
                  writePattern(
                    p,
                    proto.Pattern(
                      proto.Pattern.Value.UnionPat(proto.UnionPattern(us))
                    )
                  )
                }
          }
      }

  def varKindToProto(v: Type.Var.Bound, k: Kind): Tab[proto.VarKind] =
    getId(v.name).map { id =>
      proto.VarKind(id, Some(kindToProto(k)))
    }

  def typedExprToProto(te: TypedExpr[Any]): Tab[Int] =
    StateT
      .get[Try, SerState]
      .map(_.expressions.indexOf(te))
      .flatMap {
        case Some(idx) => tabPure(idx + 1)
        case None      =>
          import TypedExpr._
          te match {
            case g @ Generic(quant, expr) =>
              val fas = quant.forallList.traverse { case (v, k) =>
                varKindToProto(v, k)
              }
              val exs = quant.existList.traverse { case (v, k) =>
                varKindToProto(v, k)
              }
              (fas, exs, typedExprToProto(expr))
                .flatMapN { (fas, exs, exid) =>
                  val ex = proto.GenericExpr(forAlls = fas, exists = exs, exid)
                  writeExpr(
                    g,
                    proto.TypedExpr(proto.TypedExpr.Value.GenericExpr(ex))
                  )
                }
            case a @ Annotation(term, tpe) =>
              typedExprToProto(term)
                .product(typeToProto(tpe))
                .flatMap { case (term, tpe) =>
                  val ex = proto.AnnotationExpr(term, tpe)
                  writeExpr(
                    a,
                    proto.TypedExpr(proto.TypedExpr.Value.AnnotationExpr(ex))
                  )
                }
            case al @ AnnotatedLambda(args, res, _) =>
              args.toList
                .traverse { case (n, tpe) =>
                  getId(n.sourceCodeRepr).product(typeToProto(tpe))
                }
                .product(typedExprToProto(res))
                .flatMap { case (args, resid) =>
                  val ex =
                    proto.LambdaExpr(args.map(_._1), args.map(_._2), resid)
                  writeExpr(
                    al,
                    proto.TypedExpr(proto.TypedExpr.Value.LambdaExpr(ex))
                  )
                }
            case l @ Local(nm, tpe, _) =>
              getId(nm.sourceCodeRepr)
                .product(typeToProto(tpe))
                .flatMap { case (varId, tpeId) =>
                  val ex = proto.VarExpr(0, varId, tpeId)
                  writeExpr(
                    l,
                    proto.TypedExpr(proto.TypedExpr.Value.VarExpr(ex))
                  )
                }
            case g @ Global(pack, nm, tpe, _) =>
              (
                getId(pack.asString),
                getId(nm.sourceCodeRepr),
                typeToProto(tpe)
              ).tupled
                .flatMap { case (packId, varId, tpeId) =>
                  val ex = proto.VarExpr(packId, varId, tpeId)
                  writeExpr(
                    g,
                    proto.TypedExpr(proto.TypedExpr.Value.VarExpr(ex))
                  )
                }
            case a @ App(fn, args, resTpe, _) =>
              typedExprToProto(fn)
                .product(args.traverse(typedExprToProto(_)))
                .product(typeToProto(resTpe))
                .flatMap { case ((fn, args), resTpe) =>
                  val ex = proto.AppExpr(fn, args.toList, resTpe)
                  writeExpr(
                    a,
                    proto.TypedExpr(proto.TypedExpr.Value.AppExpr(ex))
                  )
                }
            case let @ Let(nm, nmexpr, inexpr, rec, _) =>
              val prec = rec match {
                case RecursionKind.Recursive    => proto.RecursionKind.IsRec
                case RecursionKind.NonRecursive => proto.RecursionKind.NotRec
              }
              getId(nm.sourceCodeRepr)
                .product(typedExprToProto(nmexpr))
                .product(typedExprToProto(inexpr))
                .flatMap { case ((nm, nmexpr), inexpr) =>
                  val ex = proto.LetExpr(nm, nmexpr, inexpr, prec)
                  writeExpr(
                    let,
                    proto.TypedExpr(proto.TypedExpr.Value.LetExpr(ex))
                  )
                }
            case loop @ Loop(args, bodyExpr, _) =>
              args.toList
                .traverse { case (nm, initExpr) =>
                  (getId(nm.sourceCodeRepr), typedExprToProto(initExpr))
                    .mapN(proto.LoopArg(_, _))
                }
                .product(typedExprToProto(bodyExpr))
                .flatMap { case (pargs, bodyId) =>
                  val ex = proto.LoopExpr(pargs, bodyId)
                  writeExpr(
                    loop,
                    proto.TypedExpr(proto.TypedExpr.Value.LoopExpr(ex))
                  )
                }
            case recur @ Recur(args, tpe0, _) =>
              args.toList
                .traverse(typedExprToProto)
                .product(typeToProto(tpe0))
                .flatMap { case (pargs, tpe) =>
                  val ex = proto.RecurExpr(pargs, tpe)
                  writeExpr(
                    recur,
                    proto.TypedExpr(proto.TypedExpr.Value.RecurExpr(ex))
                  )
                }
            case lit @ Literal(l, tpe, _) =>
              typeToProto(tpe)
                .flatMap { tpe =>
                  val ex = proto.LiteralExpr(Some(litToProto(l)), tpe)
                  writeExpr(
                    lit,
                    proto.TypedExpr(proto.TypedExpr.Value.LiteralExpr(ex))
                  )
                }
            case m @ Match(argE, branches, _) =>
              def encodeBranch(
                  p: (Pattern[(PackageName, Constructor), Type], TypedExpr[Any])
              ): Tab[proto.Branch] =
                (patternToProto(p._1), typedExprToProto(p._2))
                  .mapN((pat, expr) => proto.Branch(pat, expr))

              typedExprToProto(argE)
                .product(branches.toList.traverse(encodeBranch))
                .flatMap { case (argId, branches) =>
                  val ex = proto.MatchExpr(argId, branches)
                  writeExpr(
                    m,
                    proto.TypedExpr(proto.TypedExpr.Value.MatchExpr(ex))
                  )
                }
          }
      }
      .onFailPrint(s"in typedExprToProto: $te")

  def varianceToProto(v: Variance): proto.Variance =
    v match {
      case Variance.Phantom       => proto.Variance.Phantom
      case Variance.Covariant     => proto.Variance.Covariant
      case Variance.Contravariant => proto.Variance.Contravariant
      case Variance.Invariant     => proto.Variance.Invariant
    }

  def varianceFromProto(p: proto.Variance): Try[Variance] =
    p match {
      case proto.Variance.Phantom             => Success(Variance.Phantom)
      case proto.Variance.Covariant           => Success(Variance.Covariant)
      case proto.Variance.Contravariant       => Success(Variance.Contravariant)
      case proto.Variance.Invariant           => Success(Variance.Invariant)
      case proto.Variance.Unrecognized(value) =>
        Failure(new Exception(s"unrecognized value for variance: $value"))
    }

  def kindToProto(kind: Kind): proto.Kind =
    Kind.kindToLong(kind) match {
      case Some(idx) =>
        proto.Kind(proto.Kind.Value.Encoded(idx))
      case None =>
        kind match {
          case Kind.Type => proto.Kind(proto.Kind.Value.Type(proto.TypeKind()))
          case Kind.Cons(Kind.Arg(v, i), o) =>
            val vp = varianceToProto(v)
            val ip = kindToProto(i)
            val op = kindToProto(o)
            proto.Kind(
              proto.Kind.Value.Cons(proto.ConsKind(vp, Some(ip), Some(op)))
            )
        }
    }
  def kindFromProto(kp: Option[proto.Kind]): Try[Kind] =
    kp match {
      case Some(proto.Kind(proto.Kind.Value.Encoded(idx), _)) =>
        Kind.longToKind(idx) match {
          case Some(k) => Success(k)
          case None    =>
            Failure(new Exception(s"could not decode $idx into Kind"))
        }
      case Some(proto.Kind(proto.Kind.Value.Type(proto.TypeKind(_)), _)) =>
        Success(Kind.Type)
      case Some(
            proto.Kind(proto.Kind.Value.Cons(proto.ConsKind(v, i, o, _)), _)
          ) =>
        for {
          variance <- varianceFromProto(v)
          kindI <- kindFromProto(i)
          kindO <- kindFromProto(o)
        } yield Kind.Cons(Kind.Arg(variance, kindI), kindO)
      case None | Some(proto.Kind(proto.Kind.Value.Empty, _)) =>
        Failure(new Exception("missing Kind"))
    }

  def definedTypeToProto(d: DefinedType[Kind.Arg]): Tab[proto.DefinedType] =
    typeConstToProto(d.toTypeConst).flatMap { tc =>
      def paramToProto(tv: (Type.Var.Bound, Kind.Arg)): Tab[proto.TypeParam] =
        typeVarBoundToProto(tv._1)
          .map { tvb =>
            val Kind.Arg(variance, kind) = tv._2
            proto.TypeParam(
              Some(tvb),
              varianceToProto(variance),
              Some(kindToProto(kind))
            )
          }

      val protoTypeParams: Tab[List[proto.TypeParam]] =
        d.annotatedTypeParams.traverse(paramToProto)

      val constructors: Tab[List[proto.ConstructorFn]] =
        d.constructors.traverse { cf =>
          (
            cf.args
              .traverse { case (b, t) =>
                typeToProto(t).flatMap { tidx =>
                  getId(b.sourceCodeRepr)
                    .map { n =>
                      proto.FnParam(n, tidx)
                    }
                }
              },
            cf.exists.traverse(paramToProto)
          ).flatMapN { (params, exists) =>
            getId(cf.name.asString)
              .map { id =>
                proto.ConstructorFn(id, params, exists)
              }
          }
        }

      (protoTypeParams, constructors)
        .mapN(proto.DefinedType(Some(tc), _, _))
    }

  def definedTypeFromProto(
      pdt: proto.DefinedType
  ): DTab[DefinedType[Kind.Arg]] = {
    def paramFromProto(tp: proto.TypeParam): DTab[(Type.Var.Bound, Kind.Arg)] =
      tp.typeVar match {
        case None =>
          ReaderT.liftF(
            Failure(new Exception(s"expected type variable in $tp"))
          )
        case Some(tv) =>
          val ka = for {
            v <- varianceFromProto(tp.variance)
            k <- kindFromProto(tp.kind)
          } yield Kind.Arg(v, k)

          typeVarBoundFromProto(tv)
            .product(ReaderT.liftF(ka))
      }

    def fnParamFromProto(p: proto.FnParam): DTab[(Bindable, Type)] =
      for {
        name <- lookup(p.name, p.toString)
        bn <- ReaderT.liftF(toBindable(name))
        tpe <- lookupType(p.typeOf, s"invalid type id: $p")
      } yield (bn, tpe)

    def consFromProto(c: proto.ConstructorFn): DTab[rankn.ConstructorFn[Kind.Arg]] =
      lookup(c.name, c.toString)
        .flatMap { cname =>
          ReaderT
            .liftF(toConstructor(cname))
            .flatMap { cname =>
              (
                c.params.toList.traverse(fnParamFromProto),
                c.exists.toList.traverse(paramFromProto)
              ).mapN { (fnParams, exists) =>
                rankn.ConstructorFn(cname, fnParams, exists)
              }
            }
        }

    pdt.typeConst match {
      case None =>
        ReaderT.liftF(Failure(new Exception(s"missing typeConst: $pdt")))
      case Some(tc) =>
        for {
          tconst <- typeConstFromProto(tc)
          tparams <- pdt.typeParams.toList.traverse(paramFromProto)
          cons <- pdt.constructors.toList.traverse(consFromProto)
        } yield DefinedType(tconst.packageName, tconst.name, tparams, cons)
    }
  }

  def referantToProto[V](
      allDts: Map[(PackageName, TypeName), (DefinedType[Any], Int)],
      r: Referant[V]
  ): Tab[proto.Referant] =
    r match {
      case Referant.Value(t) =>
        typeToProto(t).map { tpeId =>
          proto.Referant(proto.Referant.Referant.Value(tpeId))
        }
      case Referant.DefinedT(dt) =>
        val key = (dt.packageName, dt.name)
        allDts.get(key) match {
          case Some((_, idx)) =>
            tabPure(
              proto.Referant(
                proto.Referant.Referant.DefinedType(
                  proto.DefinedTypeReference(
                    proto.DefinedTypeReference.Value.LocalDefinedTypePtr(
                      idx + 1
                    )
                  )
                )
              )
            )
          case None =>
            // this is a non-local defined type:
            typeConstToProto(dt.toTypeConst).map { case tc =>
              proto.Referant(
                proto.Referant.Referant.DefinedType(
                  proto.DefinedTypeReference(
                    proto.DefinedTypeReference.Value.ImportedDefinedType(tc)
                  )
                )
              )
            }
        }
      case Referant.Constructor(dt, cf) =>
        val key = (dt.packageName, dt.name)
        allDts.get(key) match {
          case Some((dtV, dtIdx)) =>
            val cIdx = dtV.constructors.indexWhere(_.name == cf.name)
            if (cIdx >= 0) {
              tabPure(
                proto.Referant(
                  proto.Referant.Referant.Constructor(
                    proto.ConstructorReference(
                      proto.ConstructorReference.Value.LocalConstructor(
                        proto.ConstructorPtr(dtIdx + 1, cIdx + 1)
                      )
                    )
                  )
                )
              )
            } else
              tabFail(
                new Exception(
                  s"missing contructor for type $key, ${cf.name}, with local: $dt"
                )
              )
          case None =>
            (
              getId(dt.packageName.asString),
              getId(dt.name.ident.sourceCodeRepr),
              getId(cf.name.sourceCodeRepr)
            ).mapN { (pid, tid, cid) =>
              proto.Referant(
                proto.Referant.Referant.Constructor(
                  proto.ConstructorReference(
                    proto.ConstructorReference.Value.ImportedConstructor(
                      proto.ImportedConstructor(pid, tid, cid)
                    )
                  )
                )
              )
            }
        }
    }

  def expNameToProto[V](
      allDts: Map[(PackageName, TypeName), (DefinedType[Any], Int)],
      e: ExportedName[Referant[V]]
  ): Tab[proto.ExportedName] = {
    val protoRef: Tab[proto.Referant] = referantToProto(allDts, e.tag)
    val exKind: Tab[(Int, proto.ExportKind)] = e match {
      case ExportedName.Binding(b, _) =>
        getId(b.sourceCodeRepr).map((_, proto.ExportKind.Binding))
      case ExportedName.TypeName(n, _) =>
        getId(n.asString).map((_, proto.ExportKind.TypeName))
      case ExportedName.Constructor(n, _) =>
        getId(n.asString).map((_, proto.ExportKind.ConstructorName))
    }

    (protoRef, exKind).mapN { case (ref, (idx, k)) =>
      proto.ExportedName(k, idx, Some(ref))
    }
  }

  private def packageDeps(
      strings: Array[String],
      dt: proto.DefinedType
  ): List[String] =
    dt.typeConst match {
      case Some(tc) =>
        strings(tc.packageName - 1) :: Nil
      case None => Nil
    }

  // what package names does this interface depend on?
  private def ifaceDeps(iface: proto.Interface): List[String] = {
    val ary = iface.strings.toArray
    val thisPack = ary(iface.packageName - 1)
    iface.definedTypes.toList
      .flatMap(packageDeps(ary, _).filterNot(_ == thisPack))
      .distinct
      .sorted
  }

  // what package names does this full package depend on?
  private def packageDeps(pack: proto.Package): List[String] = {
    val ary = pack.strings.toArray
    val thisPack = ary(pack.packageName - 1)
    val dts = pack.definedTypes.toList.flatMap(packageDeps(ary, _))
    def getImp(imp: proto.Imports): String =
      ary(imp.packageName - 1)
    val imps: List[String] = pack.imports.map(getImp).toList
    (dts ::: imps).distinct.sorted.filterNot(_ == thisPack)
  }

  def interfaceToProto(iface: Package.Interface): Try[proto.Interface] = {
    val allDts = DefinedType
      .listToMap(iface.exports.flatMap { ex =>
        /*
         * allDts are the locally defined types to this package
         * so we need to filter those outside this package
         */
        ex.tag.definedType
          .filter(_.packageName == iface.name)
      })
      .mapWithIndex((dt, idx) => (dt, idx))

    val tryProtoDts = allDts
      .traverse { case (dt, _) => definedTypeToProto(dt) }
      .map(_.iterator.map(_._2).toList)

    val tryExports = iface.exports.traverse(expNameToProto(allDts, _))

    val packageId = getId(iface.name.asString)

    val last = packageId.product(tryProtoDts).product(tryExports)

    runTab(last).map { case (serstate, ((nm, dts), exps)) =>
      proto.Interface(
        serstate.strings.inOrder,
        serstate.types.inOrder,
        dts,
        nm,
        exps
      )
    }
  }

  private def referantFromProto(
      loadDT: Type.Const => Try[DefinedType[Kind.Arg]],
      ref: proto.Referant
  ): DTab[Referant[Kind.Arg]] =
    ref.referant match {
      case proto.Referant.Referant.Value(t) =>
        lookupType(t, s"invalid type in $ref").map(Referant.Value(_))
      case proto.Referant.Referant
            .DefinedType(proto.DefinedTypeReference(dt, _)) =>
        dt match {
          case proto.DefinedTypeReference.Value.LocalDefinedTypePtr(idx) =>
            lookupDts(idx, s"invalid defined type in $ref")
              .map(Referant.DefinedT(_))
          case proto.DefinedTypeReference.Value.ImportedDefinedType(tc) =>
            typeConstFromProto(tc)
              .flatMapF(loadDT)
              .map(Referant.DefinedT(_))
          case proto.DefinedTypeReference.Value.Empty =>
            ReaderT.liftF(Failure(new Exception(s"empty referant found: $ref")))
        }
      case proto.Referant.Referant
            .Constructor(proto.ConstructorReference(consRef, _)) =>
        consRef match {
          case proto.ConstructorReference.Value
                .LocalConstructor(proto.ConstructorPtr(dtIdx, cIdx, _)) =>
            lookupDts(dtIdx, s"invalid defined type in $ref").flatMap { dt =>
              // cIdx is 1 based:
              val fixedIdx = cIdx - 1
              ReaderT.liftF(dt.constructors.get(fixedIdx.toLong) match {
                case None =>
                  Failure(
                    new Exception(s"invalid constructor index: $cIdx in: $dt")
                  )
                case Some(cf) =>
                  Success(Referant.Constructor(dt, cf))
              })
            }
          case proto.ConstructorReference.Value.ImportedConstructor(
                proto.ImportedConstructor(packId, typeId, consId, _)
              ) =>
            (
              lookup(packId, s"imported constructor package in $ref"),
              lookup(typeId, s"imported constructor typename in $ref"),
              lookup(consId, s"imported constructor name in $ref")
            ).tupled
              .flatMapF { case (p, t, c) =>
                for {
                  tc <- typeConstFromStr(p, t, s"in $ref decoding ($p, $t)")
                  dt <- loadDT(tc)
                  cons <- toConstructor(c)
                  idx = dt.constructors.indexWhere(_.name == cons)
                  _ <-
                    if (idx < 0)
                      Failure(
                        new Exception(s"invalid constuctor name: $cons for $dt")
                      )
                    else Success(())
                } yield Referant.Constructor(dt, dt.constructors(idx))
              }
          case proto.ConstructorReference.Value.Empty =>
            ReaderT.liftF(Failure(new Exception(s"empty referant found: $ref")))
        }
      case proto.Referant.Referant.Empty =>
        ReaderT.liftF(Failure(new Exception(s"empty referant found: $ref")))
    }

  private def exportedNameFromProto(
      loadDT: Type.Const => Try[DefinedType[Kind.Arg]],
      en: proto.ExportedName
  ): DTab[ExportedName[Referant[Kind.Arg]]] = {
    val tryRef: DTab[Referant[Kind.Arg]] = en.referant match {
      case Some(r) => referantFromProto(loadDT, r)
      case None    =>
        ReaderT.liftF(Failure(new Exception(s"missing referant in $en")))
    }

    tryRef
      .product(lookup(en.name, en.toString))
      .flatMapF { case (ref, n) =>
        en.exportKind match {
          case proto.ExportKind.Binding =>
            toBindable(n).map { n =>
              ExportedName.Binding(n, ref)
            }
          case proto.ExportKind.TypeName =>
            toConstructor(n).map { c =>
              ExportedName.TypeName(c, ref)
            }
          case proto.ExportKind.ConstructorName =>
            toConstructor(n).map { c =>
              ExportedName.Constructor(c, ref)
            }
          case proto.ExportKind.Unrecognized(idx) =>
            Failure(new Exception(s"unknown export kind: $idx in $en"))
        }
      }
  }

  /*
   * Builds up a nested scope of DTabs
   */
  sealed private trait Scoped {
    def finish[A](dtab: DTab[A]): DTab[A] =
      this match {
        case Scoped.Prep(d, fn) =>
          d.flatMap(b => dtab.local[DecodeState](ds => fn(ds, b)))
      }
  }
  private object Scoped {
    case class Prep[A](dtab: DTab[A], fn: (DecodeState, A) => DecodeState)
        extends Scoped

    def apply[A](dtab: DTab[A])(fn: (DecodeState, A) => DecodeState): Scoped =
      Prep(dtab, fn)
    def run[A](s: Scoped*)(dtab: DTab[A]): DTab[A] =
      s.foldRight(dtab)(_.finish(_))
  }

  private def interfaceFromProto0(
      loadDT: Type.Const => Try[DefinedType[Kind.Arg]],
      protoIface: proto.Interface
  ): Try[Package.Interface] = {
    val tab: DTab[Package.Interface] =
      for {
        packageName <- lookup(protoIface.packageName, protoIface.toString)
        pn <- ReaderT.liftF(parsePack(packageName, s"interface: $protoIface"))
        exports <- protoIface.exports.toList.traverse(
          exportedNameFromProto(loadDT, _)
        )
      } yield Package(pn, Nil, exports, ())

    // build up the decoding state by decoding the tables in order
    Scoped
      .run(
        Scoped(buildTypes(protoIface.types))(_.withTypes(_)),
        Scoped(protoIface.definedTypes.toVector.traverse(definedTypeFromProto))(
          _.withDefinedTypes(_)
        )
      )(tab)
      .run(DecodeState.init(protoIface.strings))
  }

  def interfaceFromProto(protoIface: proto.Interface): Try[Package.Interface] =
    interfacesFromProto(proto.Interfaces(protoIface :: Nil)).map(_.head)

  def interfacesToProto[F[_]: Foldable](
      ps: F[Package.Interface]
  ): Try[proto.Interfaces] =
    ps.toList.traverse(interfaceToProto).map { ifs =>
      // sort so we are deterministic
      proto.Interfaces(ifs.sortBy { iface =>
        iface.strings(iface.packageName - 1)
      })
    }

  def interfacesFromProto(ps: proto.Interfaces): Try[List[Package.Interface]] =
    // packagesFromProto can handle just interfaces as well
    packagesFromProto(ps.interfaces, Nil).map(_._1)

  def importedNameToProto(
      allDts: Map[(PackageName, TypeName), (DefinedType[Any], Int)],
      in: ImportedName[NonEmptyList[Referant[Kind.Arg]]]
  ): Tab[proto.ImportedName] = {

    val locName =
      in match {
        case ImportedName.OriginalName(_, _) => None
        case ImportedName.Renamed(_, l, _)   => Some(l)
      }
    for {
      orig <- getId(in.originalName.sourceCodeRepr)
      local <- locName.traverse(ln => getId(ln.sourceCodeRepr))
      refs <- in.tag.toList.traverse(referantToProto(allDts, _))
    } yield proto.ImportedName(orig, local.getOrElse(0), refs)
  }

  def importToProto(
      allDts: Map[(PackageName, TypeName), (DefinedType[Any], Int)],
      i: Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
  ): Tab[proto.Imports] =
    for {
      nm <- getId(i.pack.name.asString)
      imps <- i.items.toList.traverse(importedNameToProto(allDts, _))
    } yield proto.Imports(nm, imps)

  def letToProto(l: (Bindable, RecursionKind, TypedExpr[Any])): Tab[proto.Let] =
    for {
      nm <- getId(l._1.sourceCodeRepr)
      rec =
        if (l._2.isRecursive) proto.RecursionKind.IsRec
        else proto.RecursionKind.NotRec
      tex <- typedExprToProto(l._3)
    } yield proto.Let(nm, rec, tex)

  def extDefToProto(nm: Bindable, opt: Option[Type]): Tab[proto.ExternalDef] =
    opt match {
      case None => tabFail(new Exception(s"external def: $nm has missing type"))
      case Some(t) =>
        (getId(nm.sourceCodeRepr), typeToProto(t)).mapN(proto.ExternalDef(_, _))
    }

  def packageToProto[A](cpack: Package.Typed[A]): Try[proto.Package] = {
    // the Int is in index in the list of definedTypes:
    val allDts
        : SortedMap[(PackageName, TypeName), (DefinedType[Kind.Arg], Int)] =
      cpack.types.definedTypes.mapWithIndex((dt, idx) => (dt, idx))
    val dtVect: Vector[DefinedType[Kind.Arg]] =
      allDts.values.iterator.map(_._1).toVector
    val tab =
      for {
        nmId <- getId(cpack.name.asString)
        imps <- cpack.imports.traverse(importToProto(allDts, _))
        exps <- cpack.exports.traverse(expNameToProto(allDts, _))
        lets <- cpack.lets.traverse(letToProto)
        exdefs <- cpack.externalDefs.traverse { nm =>
          extDefToProto(nm, cpack.types.getValue(cpack.name, nm))
        }
        dts <- dtVect.traverse(definedTypeToProto)
      } yield { (ss: SerState) =>
        proto.Package(
          strings = ss.strings.inOrder,
          types = ss.types.inOrder,
          definedTypes = dts,
          patterns = ss.patterns.inOrder,
          expressions = ss.expressions.inOrder,
          packageName = nmId,
          imports = imps,
          exports = exps,
          lets = lets,
          externalDefs = exdefs
        )
      }

    runTab(tab).map { case (ss, fn) => fn(ss) }
  }

  def packagesToProto[F[_]: Foldable, A](
      ps: F[Package.Typed[A]]
  ): Try[proto.Packages] =
    // sort so we are deterministic
    ps.toList
      .sortBy(_.name)
      .traverse(packageToProto(_))
      .map { packs =>
        proto.Packages(packs)
      }

  private def tryParse[A](p: P[A], str: String): Try[A] =
    p.parseAll(str) match {
      case Right(a)  => Success(a)
      case Left(err) =>
        val message = show"$err"
        Failure(NameParseError(str, message, err))
    }

  def toBindable(str: String): Try[Bindable] =
    tryParse(Identifier.bindableWithSynthetic, str)

  def toIdent(str: String): Try[Identifier] =
    tryParse(Identifier.parserWithSynthetic, str)

  def toConstructor(str: String): Try[Identifier.Constructor] =
    tryParse(Identifier.consParser, str)

  def lookupBindable(idx: Int, context: => String): DTab[Bindable] =
    lookup(idx, context).flatMapF(toBindable)

  def lookupIdentifier(idx: Int, context: => String): DTab[Identifier] =
    lookup(idx, context).flatMapF(toIdent)

  def importedNameFromProto(
      loadDT: Type.Const => Try[DefinedType[Kind.Arg]],
      iname: proto.ImportedName
  ): DTab[ImportedName[NonEmptyList[Referant[Kind.Arg]]]] = {
    def build[A](orig: Identifier, ref: A): DTab[ImportedName[A]] =
      if (iname.localName == 0) {
        ReaderT.pure(ImportedName.OriginalName(originalName = orig, ref))
      } else {
        lookupIdentifier(iname.localName, iname.toString)
          .map(ImportedName.Renamed(originalName = orig, _, ref))
      }

    NonEmptyList.fromList(iname.referant.toList) match {
      case None =>
        ReaderT.liftF(
          Failure(new Exception(s"expected at least one imported name: $iname"))
        )
      case Some(refs) =>
        for {
          orig <- lookupIdentifier(iname.originalName, iname.toString)
          rs <- refs.traverse(referantFromProto(loadDT, _))
          in <- build(orig, rs)
        } yield in
    }
  }

  def importsFromProto(
      imp: proto.Imports,
      lookupIface: PackageName => Try[Package.Interface],
      loadDT: Type.Const => Try[DefinedType[Kind.Arg]]
  ): DTab[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]] =
    NonEmptyList.fromList(imp.names.toList) match {
      case None =>
        ReaderT.liftF(
          Failure(new Exception(s"expected non-empty import names in: $imp"))
        )
      case Some(nei) =>
        for {
          pnameStr <- lookup(imp.packageName, imp.toString)
          pname <- ReaderT.liftF(parsePack(pnameStr, imp.toString))
          iface <- ReaderT.liftF(lookupIface(pname))
          inames <- nei.traverse(importedNameFromProto(loadDT, _))
        } yield Import(iface, inames)
    }

  def letsFromProto(
      let: proto.Let
  ): DTab[(Bindable, RecursionKind, TypedExpr[Unit])] =
    (
      lookupBindable(let.name, let.toString),
      ReaderT.liftF(recursionKindFromProto(let.rec, let.toString)): DTab[
        RecursionKind
      ],
      lookupExpr(let.expr, let.toString)
    ).tupled

  def externalDefsFromProto(ed: proto.ExternalDef): DTab[(Bindable, Type)] =
    (
      lookupBindable(ed.name, ed.toString),
      lookupType(ed.typeOf, ed.toString)
    ).tupled

  def buildProgram(
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[Unit])],
      exts: List[(Bindable, Type)]
  ): DTab[Program[TypeEnv[Kind.Arg], TypedExpr[Unit], Unit]] =
    ReaderT
      .ask[Try, DecodeState]
      .map { ds =>
        // this adds all the types and contructors
        // from the given defined types
        val te0: TypeEnv[Kind.Arg] =
          TypeEnv.fromDefinitions(ds.getDefinedTypes)
        // we need to also add all the external defs
        val te = exts.foldLeft(te0) { case (te, (b, t)) =>
          te.addExternalValue(pack, b, t)
        }
        Program(te, lets, exts.map(_._1), ())
      }

  def iname(p: proto.Interface): String =
    p.strings
      .lift(p.packageName - 1)
      .getOrElse("_unknown_" + p.packageName.toString)

  def pname(p: proto.Package): String =
    p.strings
      .lift(p.packageName - 1)
      .getOrElse("_unknown_" + p.packageName.toString)

  def packagesFromProto(
      ifaces: Iterable[proto.Interface],
      packs: Iterable[proto.Package],
      dependencyIfaces: Iterable[Package.Interface] = Nil
  ): Try[(List[Package.Interface], List[Package.Typed[Unit]])] = {

    type Node = Either[proto.Interface, proto.Package]
    def nodeName(n: Node): String =
      n match {
        case Left(i)  => iname(i)
        case Right(p) => pname(p)
      }

    implicit val ordNode: Ordering[Node] =
      new Ordering[Node] {
        def compare(l: Node, r: Node) =
          (l, r) match {
            case (Left(_), Right(_)) => -1
            case (Right(_), Left(_)) => 1
            case (nl, nr)            => nodeName(nl).compareTo(nodeName(nr))
          }
      }

    val ifaceNodes: List[Node] = ifaces.map(Left(_)).toList
    val packNodes: List[Node] = packs.map(Right(_)).toList
    val nodesByName: Map[String, List[Node]] =
      (ifaceNodes ::: packNodes).groupBy(nodeName)

    val dupNames: List[String] =
      nodesByName.iterator
        .collect {
          case (name, ns)
              if ns.count(_.isLeft) > 1 || ns.count(_.isRight) > 1 =>
            name
        }
        .toList
        .sorted

    val ifacePackMap
        : Map[String, (Option[proto.Interface], Option[proto.Package])] =
      nodesByName.iterator.map { case (name, ns) =>
        val pack = ns.collectFirst { case Right(p) => p }
        val iface = ns.collectFirst { case Left(i) => i }
        (name, (iface, pack))
      }.toMap

    val nodeMap: Map[String, Node] =
      ifacePackMap.iterator.map { case (name, (iface, pack)) =>
        val node = pack.map(Right(_)).orElse(iface.map(Left(_))).get
        (name, node)
      }.toMap
    val nodes: List[Node] = nodeMap.values.toList
    val depIfaceMap: Map[String, List[Package.Interface]] =
      dependencyIfaces.toList.groupBy(_.name.asString)
    val depIfaceDupNames: List[String] =
      depIfaceMap.iterator
        .collect { case (name, _ :: _ :: _) => name }
        .toList
        .sorted
    val depIfaceByName: Map[String, Package.Interface] =
      depIfaceMap.iterator.map { case (name, ifaces) =>
        (name, ifaces.head)
      }.toMap

    def getNodes(n: String, parent: Node): List[Node] =
      nodeMap.get(n) match {
        case Some(node)                                   => node :: Nil
        case None if n == PackageName.PredefName.asString =>
          // we can load the predef below
          Nil
        case None if depIfaceByName.contains(n) =>
          // dependencies can point to external packages not included in this proto blob
          Nil
        case None =>
          sys.error(s"could not find node named: $n a dependency of $parent")
      }

    // we only call this when we have done validation
    // so, the unsafe calls inside are checked before we call
    def dependsOn(n: Node): List[Node] =
      n match {
        case Left(i)  => ifaceDeps(i).flatMap(dep => getNodes(dep, n))
        case Right(p) => packageDeps(p).flatMap(dep => getNodes(dep, n))
      }

    if (dupNames.nonEmpty) {
      Failure(
        CliException.Basic(
          "duplicate package names: " + dupNames.mkString(", ")
        )
      )
    } else if (depIfaceDupNames.nonEmpty) {
      Failure(
        CliException.Basic(
          "duplicate dependency interface package names: " +
            depIfaceDupNames.mkString(", ")
        )
      )
    } else {
      Try(graph.Toposort.sort(nodes)(dependsOn)).flatMap { sorted =>
        if (sorted.isFailure) {
          val loopStr =
            sorted.loopNodes
              .map {
                case Left(i)  => "interface: " + iname(i)
                case Right(p) => "compiled: " + pname(p)
              }
              .mkString(", ")
          Failure(new Exception(s"circular dependencies in packages: $loopStr"))
        } else {
          def makeLoadDT(
              load: String => Try[Either[
                (Package.Interface, TypeEnv[Kind.Arg]),
                Package.Typed[Unit]
              ]]
          ): Type.Const => Try[DefinedType[Kind.Arg]] = {
            case tc @ Type.Const.Defined(p, _) =>
              val res = load(p.asString).map {
                case Left((_, dt)) =>
                  dt.toDefinedType(tc)
                case Right(comp) =>
                  comp.types.toDefinedType(tc)
              }

              res.flatMap {
                case None =>
                  Failure(new Exception(s"unknown type $tc not present"))
                case Some(dt) => Success(dt)
              }
          }

          /*
           * We know we have a dag now, so we can just go through
           * loading them.
           *
           * We will need a list of these an memoize loading them all
           */

          def packFromProtoUncached(
              pack: proto.Package,
              load: String => Try[Either[
                (Package.Interface, TypeEnv[Kind.Arg]),
                Package.Typed[Unit]
              ]]
          ): Try[Package.Typed[Unit]] = {
            val loadIface: PackageName => Try[Package.Interface] = { p =>
              load(p.asString).map {
                case Left((iface, _)) => iface
                case Right(pack)      => Package.interfaceOf(pack)
              }
            }

            val loadDT = makeLoadDT(load)

            val tab: DTab[Package.Typed[Unit]] =
              for {
                packageNameStr <- lookup(pack.packageName, pack.toString)
                packageName <- ReaderT.liftF(
                  parsePack(packageNameStr, pack.toString)
                )
                imps <- pack.imports.toList.traverse(
                  importsFromProto(_, loadIface, loadDT)
                )
                impMap <- ReaderT.liftF(
                  ImportMap.fromImports(imps)((_, _) =>
                    ImportMap.Unify.Error
                  ) match {
                    case (Nil, im) => Success(im)
                    case (nel, _)  =>
                      Failure(
                        new Exception(s"duplicated imports in package: $nel")
                      )
                  }
                )
                exps <- pack.exports.toList.traverse(
                  exportedNameFromProto(loadDT, _)
                )
                lets <- pack.lets.toList.traverse(letsFromProto)
                eds <- pack.externalDefs.toList.traverse(externalDefsFromProto)
                prog <- buildProgram(packageName, lets, eds)
              } yield Package(packageName, imps, exps, (prog, impMap))

            // build up the decoding state by decoding the tables in order
            val tab1 = Scoped.run(
              Scoped(buildTypes(pack.types))(_.withTypes(_)),
              Scoped(pack.definedTypes.toVector.traverse(definedTypeFromProto))(
                _.withDefinedTypes(_)
              ),
              Scoped(buildPatterns(pack.patterns))(_.withPatterns(_)),
              Scoped(buildExprs(pack.expressions))(_.withExprs(_))
            )(tab)

            tab1.run(DecodeState.init(pack.strings))
          }

          val predefIface = {
            val iface = Package.interfaceOf(PackageMap.predefCompiled)
            (iface, ExportedName.typeEnvFromExports(iface.name, iface.exports))
          }

          val load: String => Try[
            Either[(Package.Interface, TypeEnv[Kind.Arg]), Package.Typed[Unit]]
          ] =
            Memoize.memoizeDagHashed[String, Try[
              Either[(Package.Interface, TypeEnv[Kind.Arg]), Package.Typed[
                Unit
              ]]
            ]] { (pack, rec) =>
              ifacePackMap.get(pack) match {
                case Some((Some(iface), Some(p))) =>
                  val loadDT = makeLoadDT(rec)
                  val ifaceRes = interfaceFromProto0(loadDT, iface)
                  packFromProtoUncached(p, rec).flatMap { pack0 =>
                    ifaceRes.flatMap { iface0 =>
                      val derived = Package.interfaceOf(pack0)
                      if (iface0.equals(derived)) Success(Right(pack0))
                      else
                        Failure(
                          CliException.Basic(
                            s"interface mismatch for ${pname(p)}: exported interface does not match compiled package"
                          )
                        )
                    }
                  }
                case Some((Some(iface), None)) =>
                  interfaceFromProto0(makeLoadDT(rec), iface)
                    .map { iface =>
                      Left(
                        (
                          iface,
                          ExportedName.typeEnvFromExports(
                            iface.name,
                            iface.exports
                          )
                        )
                      )
                    }
                case Some((None, Some(p))) =>
                  packFromProtoUncached(p, rec)
                    .map(Right(_))
                case Some((None, None)) =>
                  Failure(
                    new Exception(
                      s"missing interface or compiled: $pack"
                    )
                  )
                case None if pack == PackageName.PredefName.asString =>
                  // if we haven't replaced explicitly, use the built in predef
                  Success(Left(predefIface))
                case None =>
                  depIfaceByName.get(pack) match {
                    case Some(iface) =>
                      Success(
                        Left(
                          (
                            iface,
                            ExportedName.typeEnvFromExports(
                              iface.name,
                              iface.exports
                            )
                          )
                        )
                      )
                    case None        =>
                      Failure(
                        new Exception(
                          s"missing interface or compiled: $pack"
                        )
                      )
                  }
              }
            }

          val deserPack: proto.Package => Try[Package.Typed[Unit]] = { p =>
            load(pname(p)).flatMap {
              case Left((iface, _)) =>
                Failure(
                  new Exception(
                    s"expected compiled for ${iface.name.asString}, found interface"
                  )
                )
              case Right(pack) => Success(pack)
            }
          }
          val deserIface: proto.Interface => Try[Package.Interface] = { p =>
            load(iname(p)).map {
              case Left((iface, _)) => iface
              case Right(pack)      => Package.interfaceOf(pack)
            }
          }

          // use the cached versions down here
          (
            ifaces.toList.traverse(deserIface),
            packs.toList.traverse(deserPack)
          ).tupled
        }
      }
    }
  }
}
