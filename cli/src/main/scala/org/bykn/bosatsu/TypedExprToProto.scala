package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.{Foldable, Monad, MonadError}
import cats.data.{NonEmptyList, ReaderT, StateT}
import cats.effect.IO
import java.nio.file.Path
import java.io.{FileInputStream, FileOutputStream, BufferedInputStream, BufferedOutputStream}
import org.bykn.bosatsu.rankn.{Type, DefinedType}
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag

import Identifier.Constructor

import cats.implicits._

/**
 * convert TypedExpr to and from Protobuf representation
 */
object ProtoConverter {
  case class IdAssignment[A1, A2](mapping: Map[A1, Int], inOrder: Vector[A2]) {
    def get(a1: A1, a2: => A2): Either[(IdAssignment[A1, A2], Int), Int] =
      mapping.get(a1) match {
        case Some(id) => Right(id)
        case None =>
          val id = inOrder.size
          val next = copy(
            mapping = mapping.updated(a1, id),
            inOrder = inOrder :+ a2)
          Left((next, id))
      }

    def indexOf(a1: A1): Option[Int] =
      mapping.get(a1)
  }

  object IdAssignment {
    def empty[A1, A2]: IdAssignment[A1, A2] = IdAssignment(Map.empty, Vector.empty)
  }

  case class SerState(
    strings: IdAssignment[String, String],
    types: IdAssignment[Type, proto.Type],
    patterns: IdAssignment[Pattern[(PackageName, Constructor), Type], proto.Pattern],
    expressions: IdAssignment[TypedExpr[Any], proto.TypedExpr]) {

    def stringId(s: String): Either[(SerState, Int), Int] =
      strings.get(s, s).left.map { case (next, id) => (copy(strings = next), id) }

    def typeId(t: Type, protoType: => proto.Type): Either[(SerState, Int), Int] =
      types.get(t, protoType).left.map { case (next, id) => (copy(types = next), id) }
  }

  object SerState {
    val empty: SerState =
      SerState(IdAssignment.empty, IdAssignment.empty, IdAssignment.empty, IdAssignment.empty)
  }

  type Tab[A] = StateT[Try, SerState, A]

  private def tabFail[S, A](ex: Exception): Tab[A] =
    MonadError[Tab, Throwable].raiseError(ex)

  private def tabPure[S, A](a: A): Tab[A] =
    Monad[Tab].pure(a)

  private def lift[S, A](ta: Try[A]): Tab[A] =
    StateT.liftF(ta)

  private def get(fn: SerState => Either[(SerState, Int), Int]): Tab[Int] =
    StateT.get[Try, SerState]
      .flatMap { ss =>
        fn(ss) match {
           case Right(idx) => StateT.pure(idx + 1)
           case Left((ss, idx)) =>
             StateT.set[Try, SerState](ss).as(idx + 1)
         }
      }

  private def getId(s: String): Tab[Int] = get(_.stringId(s))

  private def getTypeId(t: Type, pt: => proto.Type): Tab[Int] =
    get(_.typeId(t, pt))

  private def getProtoTypeTab(t: Type): Tab[Option[Int]] =
    StateT.get[Try, SerState]
      .map(_.types.indexOf(t).map(_ + 1))

  private def writePattern(p: Pattern[(PackageName, Constructor), Type], pp: proto.Pattern): Tab[Int] =
    StateT.get[Try, SerState]
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
    StateT.get[Try, SerState]
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
    dts: Array[DefinedType[Variance]],
    patterns: Array[Pattern[(PackageName, Constructor), Type]],
    expr: Array[TypedExpr[Any]]) {
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

    def tryPattern(idx: Int, msg: => String): Try[Pattern[(PackageName, Constructor), Type]] =
      if ((0 <= idx) && (idx < patterns.length)) Success(patterns(idx))
      else Failure(new Exception(msg))

    def getDt(idx: Int): Option[DefinedType[Variance]] =
      if ((0 <= idx) && (idx < dts.length)) Some(dts(idx))
      else None

    def withDefinedTypes(vdts: Seq[DefinedType[Variance]]): DecodeState =
      new DecodeState(strings, types, vdts.toArray, patterns, expr)

    def withTypes(ary: Array[Type]): DecodeState =
      new DecodeState(strings, ary, dts, patterns, expr)

    def withPatterns(ary: Array[Pattern[(PackageName, Constructor), Type]]): DecodeState =
      new DecodeState(strings, types, dts, ary, expr)
  }

  object DecodeState {
    def init(strings: Seq[String]): DecodeState =
      new DecodeState(strings.toArray, Array.empty, Array.empty, Array.empty, Array.empty)
  }

  type DTab[A] = ReaderT[Try, DecodeState, A]

  private def find[A](idx: Int, context: => String)(fn: (DecodeState, Int) => Option[A]): DTab[A] =
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

  private def lookupDts(idx: Int, context: => String): DTab[DefinedType[Variance]] =
    find(idx, context)(_.getDt(_))

  /**
   * this is code to build tables of serialized dags. We use this for types, patterns, expressions
   */
  private def buildTable[A, B: ClassTag](ary: Array[A])(fn: (A, Int => Try[B]) => Try[B]): Try[Array[B]] = {
    val result = new Array[B](ary.length)
    def lookup(a: A, max: Int): Int => Try[B] = { idx =>
      if (idx > 0 && idx <= max) Success(result(idx - 1))
      else Failure(new Exception(s"while decoding $a, invalid index $idx, max: $max"))
    }

    var idx = 0
    var res: Failure[Array[B]] = null
    while((idx < ary.length) && (res eq null)) {
      val a = ary(idx)
      val lookupFn = lookup(a, idx)
      fn(a, lookupFn) match {
        case Success(b) => result(idx) = b
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

        p.value match {
          case Value.Empty => Failure(new Exception(s"empty type found in $p"))
          case Value.TypeConst(tc) =>
            val proto.TypeConst(packidx, tidx) = tc
            str(packidx)
              .product(str(tidx))
              .flatMap { case (pack, t) =>
                typeConstFromStr(pack, t, p.toString)
                  .map(Type.TyConst(_))
              }
          case Value.TypeVar(tv) =>
            str(tv.varName).map { n => Type.TyVar(Type.Var.Bound(n)) }
          case Value.TypeForAll(TypeForAll(args, in)) =>
            for {
              inT <- tpe(in)
              args <- args.toList.traverse(str(_).map(Type.Var.Bound(_)))
            } yield Type.forAll(args, inT)

          case Value.TypeApply(TypeApply(left, right)) =>
            (tpe(left), tpe(right)).mapN(Type.TyApply(_, _))
        }
      }

      buildTable(types.toArray)(typeFromProto _)
    }

  def buildPatterns(pats: Seq[proto.Pattern]): DTab[Array[Pattern[(PackageName, Constructor), Type]]] =
    ReaderT[Try, DecodeState, Array[Pattern[(PackageName, Constructor), Type]]] { ds =>

      def patternFromProto(p: proto.Pattern, pat: Int => Try[Pattern[(PackageName, Constructor), Type]]): Try[Pattern[(PackageName, Constructor), Type]] = {
        import proto.Pattern.Value

        def str(i: Int): Try[String] =
          ds.tryString(i - 1, s"invalid string idx: $i in $p")

        def bindable(i: Int): Try[Identifier.Bindable] =
          str(i).flatMap { name =>
            Try(Identifier.unsafeParse(Identifier.bindableParser, name))
          }

        p.value match {
          case Value.Empty => Failure(new Exception("invalid unset pattern"))
          case Value.WildPat(_) => Success(Pattern.WildCard)
          case Value.LitPat(l) =>
            litFromProto(l).map(Pattern.Literal(_))
          case Value.VarNamePat(sidx) => bindable(sidx).map(Pattern.Var(_))
          case Value.NamedPat(proto.NamedPat(nidx, pidx)) =>
            (bindable(nidx), pat(pidx)).mapN(Pattern.Named(_, _))
          case Value.ListPat(proto.ListPat(lp)) =>
            def decodePart(part: proto.ListPart): Try[Either[Option[Identifier.Bindable], Pattern[(PackageName, Constructor), Type]]] =
              part.value match {
                case proto.ListPart.Value.Empty => Failure(new Exception(s"invalid empty list pattern in $p"))
                case proto.ListPart.Value.ItemPattern(p) => pat(p).map(Right(_))
                case proto.ListPart.Value.UnnamedList(_) => Success(Left(None))
                case proto.ListPart.Value.NamedList(idx) => bindable(idx).map { n => Left(Some(n)) }
              }

            lp.toList.traverse(decodePart).map(Pattern.ListPat(_))
          case Value.AnnotationPat(proto.AnnotationPat(pidx, tidx)) =>
            (pat(pidx), ds.tryType(tidx - 1, s"invalid type index $tidx in: $p"))
              .mapN(Pattern.Annotation(_, _))
          case Value.StructPat(proto.StructPattern(packIdx, cidx, args)) =>
            str(packIdx)
              .product(str(cidx))
              .flatMap { case (p, c) => fullNameFromStr(p, c, s"invalid structpat names: $p, $c") }
              .flatMap { pc =>
                args.toList.traverse(pat).map(Pattern.PositionalStruct(pc, _))
              }
          case Value.UnionPat(proto.UnionPattern(pats)) =>
            pats.toList match {
              case p0 :: p1 :: prest =>
                (pat(p0), pat(p1), prest.traverse(pat))
                  .mapN { (p0, p1, prest) =>
                    Pattern.Union(p0, NonEmptyList(p1, prest))
                  }

              case notTwo =>
                Failure(new Exception(s"invalid union found size: ${notTwo.size}, expected 2 or more"))
            }
        }
      }

      buildTable(pats.toArray)(patternFromProto _)
    }

  def buildExprs(exprs: Seq[proto.TypedExpr]): DTab[Array[TypedExpr[Unit]]] =
    ReaderT[Try, DecodeState, Array[TypedExpr[Unit]]] { ds =>
      def expressionFromProto(ex: proto.TypedExpr, exprOf: Int => Try[TypedExpr[Unit]]): Try[TypedExpr[Unit]] = {
        import proto.TypedExpr.Value

        def str(i: Int): Try[String] =
          ds.tryString(i - 1, s"invalid string idx: $i in $ex")

        def bindable(i: Int): Try[Identifier.Bindable] =
          str(i).flatMap { name =>
            Try(Identifier.unsafeParse(Identifier.bindableParser, name))
          }

        def ident(i: Int): Try[Identifier] =
          str(i).flatMap { name =>
            Try(Identifier.unsafeParse(Identifier.parser, name))
          }

        def typeOf(i: Int): Try[Type] =
          ds.tryType(i - 1, s"invalid type id in $ex")

        ex.value match {
          case Value.Empty => Failure(new Exception("invalid empty TypedExpr"))
          case Value.GenericExpr(proto.GenericExpr(typeParams, expr)) =>
            NonEmptyList.fromList(typeParams.toList) match {
              case Some(nel) =>
                (nel.traverse(str), exprOf(expr))
                  .mapN { (strs, expr) =>
                    val bs = strs.map(Type.Var.Bound(_))
                    TypedExpr.Generic(bs, expr, ())
                  }
              case None => Failure(new Exception(s"invalid empty type params in generic: $ex"))
            }
          case Value.AnnotationExpr(proto.AnnotationExpr(expr, tpe)) =>
            (exprOf(expr), typeOf(tpe))
              .mapN(TypedExpr.Annotation(_, _, ()))
          case Value.LambdaExpr(proto.LambdaExpr(varName, varTpe, expr)) =>
            (bindable(varName), typeOf(varTpe), exprOf(expr))
              .mapN(TypedExpr.AnnotatedLambda(_, _, _, ()))
          case Value.VarExpr(proto.VarExpr(pack, varname, tpe)) =>
            val tryPack = if (pack == 0) Success(None)
            else
              str(pack).flatMap { pstr =>
                PackageName.parse(pstr) match {
                  case None =>
                    Failure(new Exception(s"invalid package name: $pstr, in $ex"))
                  case some => Success(some)
                }
              }

            (tryPack, ident(varname), typeOf(tpe))
              .mapN(TypedExpr.Var(_, _, _, ()))
          case Value.AppExpr(proto.AppExpr(fn, arg, resTpe)) =>
            (exprOf(fn), exprOf(arg), typeOf(resTpe))
              .mapN(TypedExpr.App(_, _, _, ()))
          case Value.LetExpr(proto.LetExpr(nm, nmexpr, inexpr, rec)) =>
            val tryRec: Try[RecursionKind] =
              rec match {
                case proto.RecursionKind.NotRec => Success(RecursionKind.NonRecursive)
                case proto.RecursionKind.IsRec => Success(RecursionKind.Recursive)
                case other => Failure(new Exception(s"invalid recursion kind: $other, in $ex"))
              }
            (bindable(nm), exprOf(nmexpr), exprOf(inexpr), tryRec)
              .mapN(TypedExpr.Let(_, _, _, _, ()))
          case Value.LiteralExpr(proto.LiteralExpr(lit, tpe)) =>
            lit match {
              case None => Failure(new Exception(s"invalid missing literal in $ex"))
              case Some(lit) =>
                (litFromProto(lit), typeOf(tpe))
                  .mapN(TypedExpr.Literal(_, _, ()))
            }
          case Value.MatchExpr(proto.MatchExpr(argId, branches)) =>
            def buildBranch(b: proto.Branch): Try[(Pattern[(PackageName, Constructor), Type], TypedExpr[Unit])] =
              (ds.tryPattern(b.pattern - 1, s"invalid pattern in $ex"), exprOf(b.resultExpr)).tupled

            NonEmptyList.fromList(branches.toList) match {
              case Some(nel) =>
                (exprOf(argId), nel.traverse(buildBranch))
                  .mapN(TypedExpr.Match(_, _, ()))
              case None =>
                Failure(new Exception(s"invalid empty branches in $ex"))
            }
        }
      }

      buildTable(exprs.toArray)(expressionFromProto _)
    }

  private def fullNameFromStr(pstr: String, tstr: String, context: => String): Try[(PackageName, Constructor)] =
    PackageName.parse(pstr) match {
      case None =>
        Failure(new Exception(s"invalid package name: $pstr, in $context"))
      case Some(pack) =>
        Try {
          val cons = Identifier.unsafeParse(Identifier.consParser, tstr)
          (pack, cons)
        }
    }

  def typeConstFromStr(pstr: String, tstr: String, context: => String): Try[Type.Const.Defined] =
    fullNameFromStr(pstr, tstr, context).map { case (p, c) => Type.Const.Defined(p, TypeName(c)) }

  def typeConstFromProto(p: proto.TypeConst): DTab[Type.Const.Defined] = {
    val proto.TypeConst(packidx, tidx) = p
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
        case None =>
          p match {
            case Type.ForAll(bs, t) =>
              typeToProto(t).flatMap { idx =>
                bs.toList
                  .traverse { b => getId(b.name) }
                  .flatMap { ids =>
                    getTypeId(p, proto.Type(Value.TypeForAll(TypeForAll(ids, idx))))
                  }
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
            case Type.TyVar(Type.Var.Skolem(_, _)) | Type.TyMeta(_) =>
              tabFail(new Exception(s"invalid type to serialize: $p"))
          }
      }
  }

  def litToProto(l: Lit): proto.Literal = {
    val protoLit = l match {
      case Lit.Integer(i) =>
        try {
          proto.Literal.Value.IntValueAs64(i.longValueExact)
        }
        catch {
          case ae: ArithmeticException =>
            proto.Literal.Value.IntValueAsString(i.toString)
        }
      case Lit.Str(str) =>
        proto.Literal.Value.StringValue(str)
    }
    proto.Literal(protoLit)
  }

  def litFromProto(l: proto.Literal): Try[Lit] =
    l.value match {
      case proto.Literal.Value.Empty =>
        Failure(new Exception("unexpected unset Literal value in pattern"))
      case proto.Literal.Value.StringValue(s) =>
        Success(Lit.Str(s))
      case proto.Literal.Value.IntValueAs64(l) =>
        Success(Lit(l))
      case proto.Literal.Value.IntValueAsString(s) =>
        Success(Lit.Integer(new java.math.BigInteger(s)))
    }

  def patternToProto(p: Pattern[(PackageName, Constructor), Type]): Tab[Int] =
    StateT.get[Try, SerState]
      .map(_.patterns.indexOf(p))
      .flatMap {
        case Some(idx) => tabPure(idx + 1)
        case None =>
          p match {
            case Pattern.WildCard =>
              writePattern(p, proto.Pattern(proto.Pattern.Value.WildPat(proto.WildCardPat())))
            case Pattern.Literal(lit) =>
              val litP = litToProto(lit)
              writePattern(p, proto.Pattern(proto.Pattern.Value.LitPat(litP)))
            case Pattern.Var(n) =>
              getId(n.sourceCodeRepr)
                .flatMap { idx =>
                  writePattern(p, proto.Pattern(proto.Pattern.Value.VarNamePat(idx)))
                }
            case named@Pattern.Named(n, p) =>
              getId(n.sourceCodeRepr)
                .product(patternToProto(p))
                .flatMap { case (idx, pidx) =>
                  writePattern(named, proto.Pattern(proto.Pattern.Value.NamedPat(proto.NamedPat(idx, pidx))))
                }
            case Pattern.ListPat(items) =>
              items.traverse {
                case Right(itemPat) =>
                  patternToProto(itemPat).map { pidx =>
                    proto.ListPart(proto.ListPart.Value.ItemPattern(pidx))
                  }
                case Left(None) =>
                  // unnamed list wildcard
                  tabPure(proto.ListPart(proto.ListPart.Value.UnnamedList(proto.WildCardPat())))
                case Left(Some(bindable)) =>
                  // named list wildcard
                  getId(bindable.sourceCodeRepr).map { idx =>
                    proto.ListPart(proto.ListPart.Value.NamedList(idx))
                  }
                }
                .flatMap { parts =>
                  writePattern(p, proto.Pattern(proto.Pattern.Value.ListPat(proto.ListPat(parts))))
                }
            case ann@Pattern.Annotation(p, tpe) =>
              patternToProto(p)
                .product(typeToProto(tpe))
                .flatMap { case (pidx, tidx) =>
                  writePattern(ann, proto.Pattern(proto.Pattern.Value.AnnotationPat(proto.AnnotationPat(pidx, tidx))))
                }
            case pos@Pattern.PositionalStruct((packName, consName), params) =>
              typeConstToProto(Type.Const.Defined(packName, TypeName(consName)))
                .flatMap { ptc =>
                  params
                    .traverse(patternToProto)
                    .flatMap { parts =>
                      writePattern(pos,
                        proto.Pattern(proto.Pattern.Value.StructPat(
                          proto.StructPattern(
                            packageName = ptc.packageName,
                            constructorName = ptc.typeName,
                            params = parts))))
                    }
                }

            case Pattern.Union(h, t) =>
              (h :: t.toList)
                .traverse(patternToProto)
                .flatMap { us =>
                  writePattern(p, proto.Pattern(proto.Pattern.Value.UnionPat(proto.UnionPattern(us))))
                }
          }
      }

  def typedExprToProto(te: TypedExpr[Any]): Tab[Int] =
    StateT.get[Try, SerState]
      .map(_.expressions.indexOf(te))
      .flatMap {
        case Some(idx) => tabPure(idx + 1)
        case None =>
          import TypedExpr._
          te match {
            case g@Generic(tvars, expr, _) =>
              tvars.toList.traverse { v => getId(v.name) }
                .product(typedExprToProto(expr))
                .flatMap { case (tparams, exid) =>
                  val ex = proto.GenericExpr(tparams, exid)
                  writeExpr(g, proto.TypedExpr(proto.TypedExpr.Value.GenericExpr(ex)))
                }
            case a@Annotation(term, tpe, _) =>
              typedExprToProto(term)
                .product(typeToProto(tpe))
                .flatMap { case (term, tpe) =>
                  val ex = proto.AnnotationExpr(term, tpe)
                  writeExpr(a, proto.TypedExpr(proto.TypedExpr.Value.AnnotationExpr(ex)))
                }
            case al@AnnotatedLambda(n, tpe, res, _) =>
              getId(n.sourceCodeRepr)
                .product(typeToProto(tpe))
                .product(typedExprToProto(res))
                .flatMap { case ((vid, tid), resid) =>
                  val ex = proto.LambdaExpr(vid, tid, resid)
                  writeExpr(al, proto.TypedExpr(proto.TypedExpr.Value.LambdaExpr(ex)))
                }
            case v@Var(optPack, nm, tpe, _) =>
              optPack.traverse { p => getId(p.asString) }
                .product(getId(nm.sourceCodeRepr))
                .product(typeToProto(tpe))
                .flatMap { case ((optPackId, varId), tpeId) =>
                  val ex = proto.VarExpr(optPackId.getOrElse(0), varId, tpeId)
                  writeExpr(v, proto.TypedExpr(proto.TypedExpr.Value.VarExpr(ex)))
                }
            case a@App(fn, arg, resTpe, _) =>
              typedExprToProto(fn)
                .product(typedExprToProto(arg))
                .product(typeToProto(resTpe))
                .flatMap { case ((fn, arg), resTpe) =>
                  val ex = proto.AppExpr(fn, arg, resTpe)
                  writeExpr(a, proto.TypedExpr(proto.TypedExpr.Value.AppExpr(ex)))
                }
            case let@Let(nm, nmexpr, inexpr, rec, _) =>
              val prec = rec match {
                case RecursionKind.Recursive => proto.RecursionKind.IsRec
                case RecursionKind.NonRecursive => proto.RecursionKind.NotRec
              }
              getId(nm.sourceCodeRepr)
                .product(typedExprToProto(nmexpr))
                .product(typedExprToProto(inexpr))
                .flatMap { case ((nm, nmexpr), inexpr) =>
                  val ex = proto.LetExpr(nm, nmexpr, inexpr, prec)
                  writeExpr(let, proto.TypedExpr(proto.TypedExpr.Value.LetExpr(ex)))
                }
            case lit@Literal(l, tpe, _) =>
              typeToProto(tpe)
                .flatMap { tpe =>
                  val ex = proto.LiteralExpr(Some(litToProto(l)), tpe)
                  writeExpr(lit, proto.TypedExpr(proto.TypedExpr.Value.LiteralExpr(ex)))
                }
            case m@Match(argE, branches, _) =>
              def encodeBranch(p: (Pattern[(PackageName, Constructor), Type], TypedExpr[Any])): Tab[proto.Branch] =
                (patternToProto(p._1), typedExprToProto(p._2))
                  .mapN { (pat, expr) => proto.Branch(pat, expr) }

              typedExprToProto(argE)
                .product(branches.toList.traverse(encodeBranch))
                .flatMap { case (argId, branches) =>
                  val ex = proto.MatchExpr(argId, branches)
                  writeExpr(m, proto.TypedExpr(proto.TypedExpr.Value.MatchExpr(ex)))
                }
          }
      }

  def varianceToProto(v: Variance): proto.Variance =
    v match {
      case Variance.Phantom => proto.Variance.Phantom
      case Variance.Covariant => proto.Variance.Covariant
      case Variance.Contravariant => proto.Variance.Contravariant
      case Variance.Invariant => proto.Variance.Invariant
    }

  def varianceFromProto(p: proto.Variance): Try[Variance] =
    p match {
      case proto.Variance.Phantom => Success(Variance.Phantom)
      case proto.Variance.Covariant => Success(Variance.Covariant)
      case proto.Variance.Contravariant => Success(Variance.Contravariant)
      case proto.Variance.Invariant => Success(Variance.Invariant)
      case proto.Variance.Unrecognized(value) => Failure(new Exception(s"unrecognized value for variance: $value"))
    }

  def definedTypeToProto(d: DefinedType[Variance]): Tab[proto.DefinedType] =
    typeConstToProto(d.toTypeConst).flatMap { tc =>
      def paramToProto(tv: (Type.Var.Bound, Variance)): Tab[proto.TypeParam] =
        typeVarBoundToProto(tv._1)
          .map { tvb =>
            proto.TypeParam(Some(tvb), varianceToProto(tv._2))
          }

      val protoTypeParams: Tab[List[proto.TypeParam]] =
        d.annotatedTypeParams.traverse(paramToProto)

      val constructors: Tab[List[proto.ConstructorFn]] =
        d.constructors.traverse { case (c, tp, _) =>
          tp.traverse { case (b, t) =>
            typeToProto(t).flatMap { tidx =>
              getId(b.sourceCodeRepr)
                .map { n =>
                  proto.FnParam(n, tidx)
                }
            }
          }
          .flatMap { params =>
            getId(c.asString)
              .map { id =>
                proto.ConstructorFn(id, params)
              }
          }
        }

      (protoTypeParams, constructors)
        .mapN(proto.DefinedType(Some(tc), _, _))
    }

  def definedTypeFromProto(pdt: proto.DefinedType): DTab[DefinedType[Variance]] = {
    def paramFromProto(tp: proto.TypeParam): DTab[(Type.Var.Bound, Variance)] =
      tp.typeVar match {
        case None => ReaderT.liftF(Failure(new Exception(s"expected type variable in $tp")))
        case Some(tv) =>
          typeVarBoundFromProto(tv)
            .product(ReaderT.liftF(varianceFromProto(tp.variance)))
      }

    def fnParamFromProto(p: proto.FnParam): DTab[(Identifier.Bindable, Type)] =
      for {
        name <- lookup(p.name, p.toString)
        bn <- ReaderT.liftF(Try(Identifier.unsafeParse(Identifier.bindableParser, name)))
        tpe <- lookupType(p.typeOf, s"invalid type id: $p")
      } yield (bn, tpe)

    def consFromProto(
      tc: Type.Const.Defined,
      tp: List[Type.Var.Bound],
      c: proto.ConstructorFn): DTab[(Identifier.Constructor, List[(Identifier.Bindable, Type)], Type)] =
      lookup(c.name, c.toString)
        .flatMap { cname =>
          ReaderT.liftF(Try(Identifier.unsafeParse(Identifier.consParser, cname)))
            .flatMap { cname =>
              //def
              c.params.toList.traverse(fnParamFromProto)
                .map { fnParams =>
                  val fnType = DefinedType.constructorValueType(tc.packageName, tc.name, tp, fnParams.map(_._2))
                  (cname, fnParams, fnType)
                }
            }
        }

    pdt.typeConst match {
      case None => ReaderT.liftF(Failure(new Exception(s"missing typeConst: $pdt")))
      case Some(tc) =>
        for {
          tconst <- typeConstFromProto(tc)
          tparams <- pdt.typeParams.toList.traverse(paramFromProto)
          cons <- pdt.constructors.toList.traverse(consFromProto(tconst, tparams.map(_._1), _))
        } yield DefinedType(tconst.packageName, tconst.name, tparams, cons)
    }
  }

  def interfaceToProto(iface: Package.Interface): Try[proto.Interface] = {
    val allDts = DefinedType.listToMap(
      iface.exports.flatMap { ex =>
        ex.tag match {
          case Referant.Value(_) => Nil
          case Referant.DefinedT(dt) => dt :: Nil
          case Referant.Constructor(_, dt, _, _) => dt :: Nil
        }
      }).mapWithIndex { (dt, idx) => (dt, idx) }

    val tryProtoDts = allDts
      .traverse { case (dt, _) => definedTypeToProto(dt) }
      .map(_.iterator.map(_._2).toList)

    def expNameToProto(e: ExportedName[Referant[Variance]]): Tab[proto.ExportedName] = {
      val protoRef: Tab[proto.Referant] =
        e.tag match {
          case Referant.Value(t) =>
            typeToProto(t).map { tpeId =>
              proto.Referant(proto.Referant.Referant.Value(tpeId))
            }
          case Referant.DefinedT(dt) =>
            val key = (dt.packageName, dt.name)
            allDts.get(key) match {
              case Some((_, idx)) =>
                tabPure(
                  proto.Referant(proto.Referant.Referant.DefinedTypePtr(idx + 1))
                )
              case None => tabFail(new Exception(s"missing defined type for $key in $e"))
            }
          case Referant.Constructor(nm, dt, _, _) =>
            val key = (dt.packageName, dt.name)
            allDts.get(key) match {
              case Some((dtV, dtIdx)) =>
                val cIdx = dtV.constructors.indexWhere { case (c, _, _) => c == nm }
                if (cIdx >= 0) {
                  tabPure(
                    proto.Referant(
                      proto.Referant.Referant.Constructor(
                        proto.ConstructorPtr(dtIdx + 1, cIdx + 1))))
                }
                else tabFail(new Exception(s"missing contructor for type $key, $nm, with local: $dt"))
              case None => tabFail(new Exception(s"missing defined type for $key in $e"))
            }
        }
      val exKind: Tab[(Int, proto.ExportKind)] = e match {
        case ExportedName.Binding(b, _) =>
          getId(b.sourceCodeRepr).map((_, proto.ExportKind.Binding))
        case ExportedName.TypeName(n, _) =>
          getId(n.asString).map((_, proto.ExportKind.TypeName))
        case ExportedName.Constructor(n, _) =>
          getId(n.asString).map((_, proto.ExportKind.ConstructorName))
      }

      (protoRef, exKind).mapN { case (ref, (idx, k)) => proto.ExportedName(k, idx, Some(ref)) }
    }

    val tryExports = iface.exports.traverse(expNameToProto)

    val packageId = getId(iface.name.asString)

    val last = packageId.product(tryProtoDts).product(tryExports)

    runTab(last).map { case (serstate, ((nm, dts), exps)) =>
      proto.Interface(serstate.strings.inOrder, serstate.types.inOrder, dts, nm, exps)
    }
  }

  private def referantFromProto(ref: proto.Referant): DTab[Referant[Variance]] =
    ref.referant match {
      case proto.Referant.Referant.Value(t) =>
        lookupType(t, s"invalid type in $ref").map(Referant.Value(_))
      case proto.Referant.Referant.DefinedTypePtr(idx) =>
        lookupDts(idx, s"invalid defined type in $ref").map(Referant.DefinedT(_))
      case proto.Referant.Referant.Constructor(proto.ConstructorPtr(dtIdx, cIdx)) =>
        lookupDts(dtIdx, s"invalid defined type in $ref").flatMap { dt =>
          // cIdx is 1 based:
          val fixedIdx = cIdx - 1
          ReaderT.liftF(dt.constructors.get(fixedIdx.toLong) match {
            case None =>
              Failure(new Exception(s"invalid constructor index: $cIdx in: $dt"))
            case Some((c, a, t)) =>
              Success(Referant.Constructor(c, dt, a, t))
          })
        }
      case proto.Referant.Referant.Empty =>
        ReaderT.liftF(Failure(new Exception(s"empty referant found: $ref")))
    }

  private def exportedNameFromProto(en: proto.ExportedName): DTab[ExportedName[Referant[Variance]]] = {
    val tryRef: DTab[Referant[Variance]] = en.referant match {
      case Some(r) => referantFromProto(r)
      case None => ReaderT.liftF(Failure(new Exception(s"missing referant in $en")))
    }

    tryRef.product(lookup(en.name, en.toString))
      .flatMapF { case (ref, n) =>
        en.exportKind match {
          case proto.ExportKind.Binding =>
            Success(ExportedName.Binding(Identifier.unsafeParse(Identifier.bindableParser, n), ref))
          case proto.ExportKind.TypeName =>
            Success(ExportedName.TypeName(Identifier.unsafeParse(Identifier.consParser, n), ref))
          case proto.ExportKind.ConstructorName =>
            Success(ExportedName.Constructor(Identifier.unsafeParse(Identifier.consParser, n), ref))
          case proto.ExportKind.Unrecognized(idx) =>
           Failure(new Exception(s"unknown export kind: $idx in $en"))
        }
      }
  }

  private def defTypes[A](dts: Seq[proto.DefinedType], dtab: DTab[A]): DTab[A] =
    dts.toVector
      .traverse(definedTypeFromProto)
      .flatMap { dtVect =>
        dtab.local { ds: DecodeState => ds.withDefinedTypes(dtVect) }
      }

  def interfaceFromProto(protoIface: proto.Interface): Try[Package.Interface] = {
    val tab: DTab[Package.Interface] = lookup(protoIface.packageName, protoIface.toString)
      .flatMap { packageName =>
        PackageName.parse(packageName) match {
          case None =>
            ReaderT.liftF(
              Failure(new Exception(s"bad package name: $packageName in: $protoIface")))
          case Some(pn) =>
            defTypes(protoIface.definedTypes,
                protoIface
                  .exports
                  .toList
                  .traverse(exportedNameFromProto)
                  .map { exports =>
                    Package(pn, Nil, exports, ())
                  })
        }
      }

    (for {
      tpes <- buildTypes(protoIface.types)
      pack <- tab.local[DecodeState](_.withTypes(tpes))
    } yield pack).run(DecodeState.init(protoIface.strings))
  }

  def interfacesToProto[F[_]: Foldable](ps: F[Package.Interface]): Try[proto.Interfaces] =
    ps.toList.traverse(interfaceToProto).map { ifs =>
      // sort so we are deterministic
      proto.Interfaces(ifs.sortBy { iface => iface.strings(iface.packageName - 1) })
    }

  def interfacesFromProto(ps: proto.Interfaces): Try[List[Package.Interface]] =
    ps.interfaces.toList.traverse(interfaceFromProto)

  def readInterfaces(path: Path): IO[List[Package.Interface]] =
    IO {
      val f = path.toFile
      val ios = new BufferedInputStream(new FileInputStream(f))
      try proto.Interfaces.parseFrom(ios)
      finally {
        ios.close
      }
    }.flatMap { proto => IO.fromTry(interfacesFromProto(proto)) }

  def writeInterfaces(interfaces: List[Package.Interface], path: Path): IO[Unit] =
    IO.fromTry(interfacesToProto(interfaces))
      .flatMap { protoIfs =>
        IO {
          val f = path.toFile
          val os = new BufferedOutputStream(new FileOutputStream(f))
          try protoIfs.writeTo(os)
          finally {
            os.close
          }
        }
      }
}
