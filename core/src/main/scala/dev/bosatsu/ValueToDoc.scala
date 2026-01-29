package dev.bosatsu

import cats.Eval
import cats.implicits._
import java.math.BigInteger
import dev.bosatsu.rankn.{DefinedType, Type, DataFamily}
import org.typelevel.paiges.{Doc, Document}
import scala.collection.mutable.{Map => MMap}

import Value._
import Identifier.Constructor

import JsonEncodingError.IllTyped

case class ValueToDoc(getDefinedType: Type.Const => Option[DefinedType[Any]]) {

  /** Convert a typechecked value to a Document representation
    *
    * Note, we statically build the conversion function if it is possible at
    * all, after that, only value errors can occur
    *
    * this code ASSUMES the type is correct. If not, we may return incorrect
    * data if it is not clearly illtyped
    */
  def toDoc(tpe: Type): Value => Either[IllTyped, Doc] = {

    type Fn = Value => Either[IllTyped, Doc]
    // when we complete a custom type, we put it in here
    val successCache: MMap[Type, Eval[Fn]] = MMap()

    def commaBlock(items: Iterable[Doc]): Doc =
      Doc.intercalate(Doc.char(',') + Doc.line, items).grouped

    def loop(tpe: Type, revPath: List[Type]): Eval[Fn] =
      // we know we can support this, so when we recurse it
      // is safe to call .right.get
      successCache.get(tpe) match {
        case Some(fn) => fn
        case None     =>
          val res: Eval[Fn] = Eval.later(tpe match {
            case Type.IntType => {
              case ExternalValue(v: BigInteger) =>
                Right(Doc.str(v))
              case other =>
                // $COVERAGE-OFF$this should be unreachable
                Left(IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
            }
            case Type.StrType => {
              case ExternalValue(v: String) =>
                Right(Document[Lit].document(Lit.Str(v)))
              case other =>
                // $COVERAGE-OFF$this should be unreachable
                Left(IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
            }
            case Type.UnitType =>
              // encode this as null
              {
                case UnitValue => Right(Doc.text("()"))
                case other     =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.ListT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

              {
                case VList(vs) =>
                  vs.traverse(inner)
                    .map { inners =>
                      Doc.char('[') + (Doc.lineOrEmpty + commaBlock(
                        inners
                      ) + Doc.lineOrEmpty).aligned + Doc.char(']')
                    }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.DictT(Type.StrType, vt) =>
              lazy val inner = loop(vt, tpe :: revPath).value
              val docStr = Document[Lit]

              {
                case VDict(d) =>
                  d.toList
                    .traverse { case (k, v) =>
                      k match {
                        case Str(kstr) =>
                          inner(v).map { vdoc =>
                            (docStr.document(Lit.Str(kstr)) + (Doc.char(
                              ':'
                            ) + Doc.line + vdoc).nested(4)).grouped
                          }
                        case other =>
                          // $COVERAGE-OFF$this should be unreachable
                          Left(IllTyped(revPath.reverse, tpe, other))
                        // $COVERAGE-ON$
                      }
                    }
                    .map { kvs =>
                      Doc.char('{') + (Doc.lineOrEmpty + commaBlock(
                        kvs
                      ) + Doc.lineOrEmpty).aligned + Doc.char('}')
                    }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.Tuple(ts) =>
              val p1 = tpe :: revPath
              lazy val inners = ts.traverse(t => loop(t, p1)).value
              val tsize = ts.size

              {
                case Tuple(as) if as.size == tsize =>
                  as.zip(inners)
                    .toVector
                    .traverse { case (a, fn) => fn(a) }
                    .map { items =>
                      Doc.char('(') + (Doc.lineOrEmpty + commaBlock(items) + Doc
                        .char(',') + Doc.lineOrEmpty).aligned + Doc.char(')')
                    }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }

            case Type.ForAll(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath).value
            case Type.TyVar(_) =>
              // we don't really know what to do with
              { _ => Right(Doc.text("<unknown>")) }
            case fn @ Type.Fun(_, _) =>
              def arity(fn: Type): Int =
                fn match {
                  case Type.Fun(_, dest) =>
                    arity(dest) + 1
                  case Type.ForAll(_, inner) =>
                    arity(inner)
                  case _ => 0
                }

              {
                case FnValue(_) =>
                  Right(Doc.text(s"<fn arity=${arity(fn)}>"))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case _ =>
              // We can have complicated recursion here, we
              // need to be careful with Eval.later/lazy to tie the knot
              val fullPath = tpe :: revPath

              val dtOpt =
                Type.rootConst(tpe).flatMap { case Type.TyConst(const) =>
                  getDefinedType(const)
                }

              dtOpt match {
                case None =>
                  // an unknown type, just print unknown
                  { _ => Right(Doc.text("<unknown>")) }
                case Some(dt) =>
                  val cons = dt.constructors
                  val (_, targs) = Type.unapplyAll(tpe)
                  val replaceMap =
                    dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  lazy val resInner
                      : Map[Int, (Constructor, List[(String, Fn)])] =
                    cons.zipWithIndex
                      .traverse { case (cf, idx) =>
                        val rec = cf.args.traverse { case (field, t) =>
                          val subsT = Type.substituteVar(t, replaceMap)
                          val next = loop(subsT, fullPath)
                          next.map(fn => (field.asString, fn))
                        }
                        rec.map(fields => (idx, (cf.name, fields)))
                      }
                      .map(_.toMap)
                      .value

                  def params(
                      variant: Int,
                      params: List[Value],
                      src: Value
                  ): Either[IllTyped, Doc] =
                    resInner.get(variant) match {
                      case None =>
                        Left(IllTyped(revPath.reverse, tpe, src))
                      case Some((name, fields)) =>
                        if (fields.size == params.size) {
                          params
                            .zip(fields)
                            .traverse { case (v, (nm, fn)) =>
                              fn(v).map { vdoc =>
                                (Doc.text(nm) + Doc.char(
                                  ':'
                                ) + Doc.lineOrSpace + vdoc).nested(4)
                              }
                            }
                            .map { paramsDoc =>
                              val nm = Doc.text(name.asString)
                              if (paramsDoc.isEmpty) nm
                              else {
                                nm + Doc.space +
                                  (Doc.char('{') + (Doc.line + commaBlock(
                                    paramsDoc
                                  )).nested(4) + Doc.line + Doc
                                    .char('}')).grouped
                              }
                            }
                        } else Left(IllTyped(revPath.reverse, tpe, src))
                    }

                  dt.dataFamily match {
                    case DataFamily.NewType =>
                      // the outer wrapping is so we add it back
                      { v => params(0, v :: Nil, v) }
                    case DataFamily.Struct => {
                      case prod: ProductValue =>
                        params(0, prod.values.toList, prod)

                      case other =>
                        Left(IllTyped(revPath.reverse, tpe, other))
                    }
                    case DataFamily.Enum => {
                      case s: SumValue =>
                        params(s.variant, s.value.values.toList, s)
                      case a =>
                        Left(IllTyped(revPath.reverse, tpe, a))
                    }
                    case DataFamily.Nat =>
                      // this is nat-like
                      // TODO, maybe give a warning
                      {
                        case ExternalValue(b: BigInteger) =>
                          Right(Doc.str(b))
                        case other =>
                          Left(IllTyped(revPath.reverse, tpe, other))
                      }
                  }
              }
          })
          // put the result in the cache before we compute it
          // so we can recurse
          successCache.put(tpe, res)
          res
      }

    loop(tpe, Nil).value
  }

}
