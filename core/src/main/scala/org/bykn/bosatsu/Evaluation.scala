package org.bykn.bosatsu

import cats.Eval
import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}
import scala.collection.mutable.{Map => MMap}

import cats.implicits._

import Identifier.{Bindable, Constructor}

object Evaluation {
  import Value._

  type Env = Map[Identifier, Eval[Value]]

  sealed abstract class Scoped {
    import Scoped.fromFn

    def inEnv(env: Env): Eval[Value]

    def letNameIn(name: Bindable, in: Scoped): Scoped =
      Scoped.Let(name, this, in)

    def branch(fn: (Value, Env) => Eval[Value]): Scoped =
      fromFn { env =>
        inEnv(env).flatMap { v =>
          fn(v, env)
        }
      }

    def asLambda(name: Bindable): Scoped =
      fromFn { env =>
        import cats.Now
        val fn =
          FnValue {
            case n@Now(v) =>
              inEnv(env.updated(name, n))
            case v => v.flatMap { v0 =>
              inEnv(env.updated(name, Eval.now(v0)))
            }
          }

        Eval.now(fn)
      }

    def applyArg(arg: Scoped): Scoped =
      fromFn { env =>
        val fnE = inEnv(env).memoize
        val argE = arg.inEnv(env)
        fnE.flatMap { fn =>
          // safe because we typecheck
          fn.asLazyFn(argE)
        }
      }
  }

  object Scoped {
    def const(e: Eval[Value]): Scoped =
      fromFn(_ => e)

    def fromEnv(identifier: Identifier): Scoped =
      fromFn { env =>
        env.get(identifier) match {
          case Some(e) => e
          case None => sys.error(s"could not find $identifier in the environment with keys: ${env.keys.toList.sorted}")
        }
      }

    def recursive(name: Bindable, item: Scoped): Scoped =
      fromFn { env =>
        lazy val res: Eval[Value] =
          Eval.defer(item.inEnv(env1)).memoize
        lazy val env1: Map[Identifier, Eval[Value]] =
          env.updated(name, res)
        res
      }

    private case class Let(name: Bindable, arg: Scoped, in: Scoped) extends Scoped {
      def inEnv(env: Env) = {
        val let = arg.inEnv(env)
        in.inEnv(env.updated(name, let))
      }
    }

    private def fromFn(fn: Env => Eval[Value]): Scoped =
      FromFn(fn)

    private case class FromFn(fn: Env => Eval[Value]) extends Scoped {
      def inEnv(env: Env) = fn(env)
    }
  }

}

case class Evaluation[T](pm: PackageMap.Typed[T], externals: Externals) {
  import Evaluation.{Scoped, Env}
  import Value._

  /**
   * Holds the final value of the environment for each Package
   */
  private[this] val envCache: MMap[PackageName, Env] =
    MMap.empty

  private def importedEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value]] =
    p.imports.iterator.flatMap { imp =>
      val pack = pm.toMap.get(imp.pack.name) match {
        case Some(p) => p
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown package: ${imp.pack.name}")
          // $COVERAGE-ON$
      }
      imp.items
        .toList
        .iterator
        .filter { in =>
          // We can ignore type imports, they aren't values
          in.tag.exists {
            case Referant.Value(_) | Referant.Constructor(_, _) => true
            case Referant.DefinedT(_) => false
          }
        }
        .map { in =>
          val value = getValue(pack, in.originalName)
          (in.localName, value)
        }
    }
    .toMap

  private def externalEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value]] = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
          // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, ext.call(tpe))
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
          // $COVERAGE-ON$
      }
    }
    .toMap
  }

  private def constructorEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value]] =
    p.program
      .types
      .allDefinedTypes
      .iterator
      .filter(_.packageName == p.name)
      .flatMap { dt =>
        dt.constructors.iterator.map { cf =>
          (cf.name, constructor(cf.name, dt))
        }
      }
      .toMap

  private def addLet(env: Env, let: (Bindable, RecursionKind, TypedExpr[T])): Env = {
    val (name, rec, e) = let
    val e0 = eval(e)
    val eres =
      if (rec.isRecursive) Scoped.recursive(name, e0)
      else e0

    // These are added lazily
    env.updated(name, Eval.defer(eres.inEnv(env)).memoize)
  }

  private def evaluate(pack: Package.Typed[T]): Env =
    envCache.getOrElseUpdate(pack.name, {
      val initEnv = importedEnv(pack) ++ constructorEnv(pack) ++ externalEnv(pack)
      // add all the external definitions
      pack.program.lets.foldLeft(initEnv)(addLet(_, _))
    })

  private def getValue(pack: Package.Typed[T], name: Identifier): Eval[Value] =
    evaluate(pack)
      .get(name)
      // error shouldn't happen due to typechecking
      .getOrElse(sys.error(s"unknown value: $name in ${pack.name}"))

  def evaluateLast(p: PackageName): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, _, tpe) <- pack.program.lets.lastOption
      value <- evaluate(pack).get(name)
    } yield (value, tpe.getType)

  /* TODO: this is useful for debugging, but we should probably test it and write a parser for the
   * list syntax

  def repr: String = {
    val packs = pm.toMap.map { case (_, pack) =>
      Doc.text(s"(package ${pack.name.asString}") +
        (Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace,
          pack.program.lets.map { case (b, _, te) =>
            Doc.text(s"(let ${b.asString}") +
              (Doc.lineOrSpace + Doc.text(te.repr) + Doc.text(")")).nested(2)
          })).nested(2) + Doc.char(')')
    }

    Doc.intercalate(Doc.lineOrSpace, packs).render(80)
  }

  */

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, tpe) =>
      def toAssert(a: Value): Test =
        a match {
          case ConsValue(True, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(true, message)
          case ConsValue(False, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(false, message)
          case other =>
            // $COVERAGE-OFF$
            sys.error(s"expected test value: $other")
            // $COVERAGE-ON$
        }
      def toSuite(a: Value): Test =
        a match {
          case ConsValue(Str(name), ConsValue(VList(tests), UnitValue)) =>
            Test.Suite(name, tests.map(toTest(_)))
          case other =>
            // $COVERAGE-OFF$
            sys.error(s"expected test value: $other")
            // $COVERAGE-ON$
        }
      def toTest(a: Value): Test =
        a match {
          case s: SumValue =>
            if (s.variant == 0) toAssert(s.value)
            else if (s.variant == 1) toSuite(s.value)
            else {
              // $COVERAGE-OFF$
              sys.error(s"unexpected variant in: $s")
              // $COVERAGE-ON$
            }
          case unexpected =>
            // $COVERAGE-OFF$
            sys.error(s"unreachable if compilation has worked: $unexpected")
            // $COVERAGE-ON$
        }

      tpe match {
        case Type.TyConst(Type.Const.Defined(PackageName.PredefName, tn)) =>
          tn.ident.asString match {
            case "Test" =>
              Some(toTest(ea.value))
            case _ =>
              None
          }
        case _ => None
      }
    }

  private type Ref = TypedExpr[T]

  private def evalBranch(
    branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])],
    recurse: Ref => Scoped): (Value, Env) => Eval[Value] = {
      def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
        pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2

      /*
       * Return true if this matches everything that would typecheck and does
       * not introduce new bindings
       */
      def isTotalNonBinding(p: Pattern[(PackageName, Constructor), Type]): Boolean =
        p match {
          case Pattern.WildCard => true
          case Pattern.Annotation(p, _) => isTotalNonBinding(p)
          case Pattern.PositionalStruct(pc, parts) =>
            definedForCons(pc).isStruct && parts.forall(isTotalNonBinding)
          case Pattern.AnyList => true
          case _ => false
        }

      val noop: (Value, Env) => Option[Env] = { (_, env) => Some(env) }
      val neverMatch: (Value, Env) => Option[Nothing] = { (_, _) => None }
      /*
       * This is used in a loop internally, so I am avoiding map and flatMap
       * in favor of pattern matching for performance
       */
      def maybeBind[E](pat: Pattern[(PackageName, Constructor), Type]): (Value, Env) => Option[Env] =
        pat match {
          case Pattern.WildCard => noop
          case Pattern.Literal(lit) =>
            val vlit = Value.fromLit(lit)

            { (v, env) => if (v == vlit) Some(env) else None }
          case Pattern.Var(n) =>

            { (v, env) => Some(env.updated(n, Eval.now(v))) }
          case Pattern.Named(n, p) =>
            val inner = maybeBind(p)

            { (v, env) =>
              inner(v, env) match {
                case None => None
                case Some(env1) => Some(env1.updated(n, Eval.now(v)))
              }
            }
          case Pattern.ListPat(items) =>
            items match {
              case Nil =>
                { (arg, acc) =>
                  arg match {
                    case VList.VNil => Some(acc)
                    case _ => None
                  }
                }
              case Pattern.ListPart.Item(ph) :: ptail =>
                // a right hand side pattern never matches the empty list
                val fnh = maybeBind(ph)
                val fnt = maybeBind(Pattern.ListPat(ptail))

                { (arg, acc) =>
                  arg match {
                    case VList.Cons(argHead, argTail) =>
                      fnh(argHead, acc) match {
                        case None => None
                        case Some(acc1) => fnt(argTail, acc1)
                      }
                    case _ => None
                  }
                }
              case Pattern.ListPart.NamedList(ident) :: Nil =>
                { (v, env) => Some(env.updated(ident, Eval.now(v))) }
              case Pattern.ListPart.WildList :: Nil =>
                noop
                // this is the common and easy case: a total match of the tail
                // we don't need to match on it being a list, because we have
                // already type checked
              case (splice: Pattern.ListPart.Glob) :: ptail =>
                // this is more costly, since we have to match a non infinite tail.
                // we reverse the tails, do the match, and take the rest into
                // the splice
                val revPat = Pattern.ListPat(ptail.reverse)
                val fnMatchTail = maybeBind(revPat)
                val ptailSize = ptail.size

                splice match {
                  case Pattern.ListPart.NamedList(nm) =>
                    { (arg, acc) =>
                      arg match {
                        case VList(asList) =>
                          // we only allow one splice, so we assume the rest of the patterns
                          val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                          fnMatchTail(VList(revArgTail), acc) match {
                            case None => None
                            case Some(acc1) => Some {
                              // now bind the rest into splice:
                              val rest = Eval.now(VList(spliceVals.reverse))
                              acc1.updated(nm, rest)
                            }
                          }
                        case notlist =>
                          // it has to be a list due to type checking
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match, expected list found: $notlist")
                          // $COVERAGE-ON$
                      }
                    }
                  case Pattern.ListPart.WildList =>
                    { (arg, acc) =>
                      arg match {
                        case VList(asList) =>
                          // we only allow one splice, so we assume the rest of the patterns
                          val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                          fnMatchTail(VList(revArgTail), acc)
                        case notlist =>
                          // it has to be a list due to type checking
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match, expected list found: $notlist")
                          // $COVERAGE-ON$
                      }
                    }
                }
            }
          case Pattern.Annotation(p, _) =>
            // we don't need types to match patterns once we have fully resolved constructors
            maybeBind(p)
          case u@Pattern.Union(_, _) =>
            // we can just loop expanding these out:
            def loop(ps: List[Pattern[(PackageName, Constructor), Type]]): (Value, Env) => Option[Env] =
              ps match {
                case Nil => neverMatch
                case head :: tail =>
                  val fnh = maybeBind(head)
                  val fnt = loop(tail)

                  { (arg, acc) =>
                    val rh = fnh(arg, acc)
                    if (rh.isDefined) rh
                    else fnt(arg, acc)
                  }
              }
            loop(Pattern.flatten(u).toList)
          case Pattern.PositionalStruct(pc@(_, ctor), items) =>
            /*
             * The type in question is not the outer dt, but the type associated
             * with this current constructor
             */
            val dt = definedForCons(pc)
            val itemFns = items.map(maybeBind(_))

            val itemsWild = items.forall(isTotalNonBinding)

            def processArgs(as: List[Value], acc: Env): Option[Env] = {
              // manually write out foldM hoping for performance improvements
              @annotation.tailrec
              def loop(vs: List[Value], fns: List[(Value, Env) => Option[Env]], env: Env): Option[Env] =
                vs match {
                  case Nil => Some(env)
                  case vh :: vt =>
                    fns match {
                      case fh :: ft =>
                        fh(vh, env) match {
                          case None => None
                          case Some(env1) => loop(vt, ft, env1)
                        }
                      case Nil => Some(env) // mismatch in size, shouldn't happen statically
                    }
                }
              loop(as, itemFns, acc)
            }

            if (dt.isNewType) {
              // we erase this entirely, and just match the underlying item
              itemFns.head
            }
            else if (dt.isStruct) {
              if (itemsWild) noop
              else
                // this is a struct, which means we expect it
                { (arg: Value, acc: Env) =>
                  arg match {
                    case p: ProductValue =>
                      // this is the pattern
                      // note passing in a List here we could have union patterns
                      // if all the pattern bindings were known to be of the same type
                      processArgs(p.toList, acc)

                    case other =>
                      // $COVERAGE-OFF$this should be unreachable
                      val itemStr = items.mkString("(", ", ", ")")
                      sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\nenv: $acc")
                      // $COVERAGE-ON$
                  }
                }
            }
            else {
              // compute the index of ctor, so we can compare integers later
              val idx = dt.constructors.indexWhere(_.name == ctor)

              dt.natLike match {
                case isz: DefinedType.NatLike.Is =>
                  // we represent Nats with java BigInteger
                  val isZero = isz.idxIsZero(idx)

                  if (isZero)
                    { (arg: Value, acc: Env) =>
                      arg match {
                        case ExternalValue(b: BigInteger) =>
                          if (b == BigInteger.ZERO) Some(acc)
                          else None
                        case other =>
                          // $COVERAGE-OFF$this should be unreachable
                          val itemStr = items.mkString("(", ", ", ")")
                          sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\nenv: $acc")
                          // $COVERAGE-ON$
                      }
                    }
                  else {
                    // We are expecting non-zero
                    val succMatch = itemFns.head

                    { (arg: Value, acc: Env) =>
                      arg match {
                        case ExternalValue(b: BigInteger) =>
                          if (b != BigInteger.ZERO) {
                            succMatch(ExternalValue(b.subtract(BigInteger.ONE)), acc)
                          }
                          else None
                        case other =>
                          // $COVERAGE-OFF$this should be unreachable
                          val itemStr = items.mkString("(", ", ", ")")
                          sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\nenv: $acc")
                          // $COVERAGE-ON$
                      }
                    }
                  }
                case DefinedType.NatLike.Not =>
                  if (itemsWild)
                    { (arg: Value, acc: Env) =>
                      arg match {
                        case s: SumValue =>
                          if (s.variant == idx) Some(acc)
                          else None
                        case other =>
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match: $other")
                          // $COVERAGE-ON$
                      }
                    }
                  else
                    { (arg: Value, acc: Env) =>
                      arg match {
                        case s: SumValue =>
                          if (s.variant == idx) processArgs(s.value.toList, acc)
                          else None
                        case other =>
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match: $other")
                          // $COVERAGE-ON$
                      }
                    }
              }
            }
        }
      def bindEnv[E](branches: List[(Pattern[(PackageName, Constructor), Type], E)]): (Value, Env) => (Env, E) =
        branches match {
          case Nil =>
            // This is ruled out by typechecking, we know all our matches are total
            // $COVERAGE-OFF$this should be unreachable
            { (arg, env) =>
              sys.error(s"non-total match: arg: $arg, branches: $branches")
            }
            // $COVERAGE-ON$
          case (p, e) :: tail =>
            val pfn = maybeBind(p)
            val fntail = bindEnv(tail)

            { (arg, env) =>
              pfn(arg, env) match {
                case None => fntail(arg, env)
                case Some(env) => (env, e)
              }
            }
        }

      // recurse on all the branches:
      val compiledBranches = branches.map { case (pattern, branch) =>
        (pattern, recurse(branch))
      }
      val bindFn = bindEnv(compiledBranches.toList)

      /*
       * Now we have fully compiled all the branches and eliminated a certain amount of runtime
       * cost of handling pattern matching
       */
      { (arg, env) =>
        val (localEnv, next) = bindFn(arg, env)
        next.inEnv(localEnv)
      }
    }

  /**
   * TODO, expr is a TypedExpr so we already know the type. returning it does not do any good that I
   * can see.
   */
  @annotation.tailrec
  private def evalTypedExpr(expr: TypedExpr[T], recurse: Ref => Scoped): Scoped = {

    import TypedExpr._

     expr match {
       case Generic(_, e, _) =>
         // TODO, we need to probably do something with this
         evalTypedExpr(e, recurse)
       case Annotation(e, _, _) => evalTypedExpr(e, recurse)
       case Var(None, ident, _, _) =>
         Scoped.fromEnv(ident)
       case Var(Some(p), ident, _, _) =>
         val pack = pm.toMap.get(p).getOrElse(sys.error(s"cannot find $p, shouldn't happen due to typechecking"))
         Scoped.const(getValue(pack, ident))
       case App(fn, arg, _, _) =>
         val efn = recurse(fn)
         val earg = recurse(arg)

         efn.applyArg(earg)
       case AnnotatedLambda(name, _, expr, _) =>
         recurse(expr).asLambda(name)
       case Let(arg, e, in, rec, _) =>
         val e0 = recurse(e)
         val eres =
           if (rec.isRecursive) Scoped.recursive(arg, e0)
           else e0
         val inres = recurse(in)

         eres.letNameIn(arg, inres)
       case Literal(lit, _, _) =>
         val res = Eval.now(Value.fromLit(lit))
         Scoped.const(res)
       case Match(arg, branches, _) =>
         val argR = recurse(arg)
         val branchR = evalBranch(branches, recurse)

         argR.branch(branchR(_, _))
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: Ref => Scoped =
    Memoize.function[Ref, Scoped] {
      (expr, recurse) => evalTypedExpr(expr, recurse)
    }

  private[this] val zeroNat: Eval[Value] = Eval.now(ExternalValue(BigInteger.ZERO))

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): Eval[Value] = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case (cf, idx) if cf.name == c => (idx, cf.args.size) }
      .get // the ctor must be in the list or we wouldn't typecheck

    // partially erase new-types
    // more advanced new-type erasure would
    // make Apply(Foo, x) into x, but this
    // does not yet do that
    if (dt.isNewType) cats.Now(FnValue.identity)
    else {
      val singleItemStruct = dt.isStruct

      dt.natLike match {
        case isz: DefinedType.NatLike.Is =>
          // we represent Nats with java BigInteger
          val isZero = isz.idxIsZero(enum)
          def inc(v: Value): Value = {
            val ex = v.asInstanceOf[ExternalValue]
            val bi = ex.toAny.asInstanceOf[BigInteger]
            ExternalValue(bi.add(BigInteger.ONE))
          }
          if (isZero) zeroNat
          else Eval.now(FnValue {
            case cats.Now(a) => cats.Now(inc(a))
            case ea => ea.map(inc).memoize
          })

        case DefinedType.NatLike.Not =>
          // TODO: this is a obviously terrible
          // the encoding is inefficient, the implementation is inefficient
          def loop(param: Int, args: List[Value]): Value =
            if (param == 0) {
              val prod = ProductValue.fromList(args.reverse)
              if (singleItemStruct) prod
              else SumValue(enum, prod)
            }
            else FnValue {
              case cats.Now(a) => cats.Now(loop(param - 1, a :: args))
              case ea => ea.map { a => loop(param - 1, a :: args) }.memoize
            }

          Eval.now(loop(arity, Nil))
      }
    }
  }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  def toJson(a: Value, tpe: Type): Option[Json] = {
    def canEncodeToNull(t: Type): Boolean =
      t match {
        case Type.UnitType | Type.OptionT(_) => true
        case Type.ForAll(_, inner) => canEncodeToNull(inner)
        case _ => false
      }
    tpe match {
      case Type.IntType =>
        val ExternalValue(v) = a
        Some(Json.JNumberStr(v.toString))
      case Type.StrType =>
        val ExternalValue(v) = a
        Some(Json.JString(v.toString))
      case Type.BoolType =>
        a match {
          case True => Some(Json.JBool(true))
          case False => Some(Json.JBool(false))
          case other =>
            // $COVERAGE-OFF$this should be unreachable
            sys.error(s"invalid cast to Boolean: $other")
            // $COVERAGE-ON$
        }
      case Type.UnitType =>
        // encode this as null
        Some(Json.JNull)
      case Type.OptionT(tpe) =>
        if (canEncodeToNull(tpe)) {
          // we can't encode Option[Option[T]] as null or not, so we encode
          // as list of 0 or 1 items
          a match {
            case VOption(None) => Some(Json.JArray(Vector.empty))
            case VOption(Some(a)) =>
              toJson(a, tpe).map { j => Json.JArray(Vector(j)) }
          }
        }
        else {
          // not a nested option
          a match {
            case VOption(None) => Some(Json.JNull)
            case VOption(Some(a)) => toJson(a, tpe)
          }
        }
      case Type.ListT(t) =>
        val VList(vs) = a
        vs.toVector
          .traverse { v => toJson(v, t) }
          .map(Json.JArray(_))
      case Type.DictT(Type.StrType, vt) =>
        val VDict(d) = a
        d.toList.traverse { case (k, v) =>
          val Str(kstr) = k
          toJson(v, vt).map((kstr, _))
        }
        .map(Json.JObject(_))
      case Type.Tuple(ts) =>
        val Tuple(as) = a
        as.zip(ts)
          .toVector
          .traverse { case (a, t) =>
            toJson(a, t)
          }
          .map(Json.JArray(_))
      case Type.ForAll(_, inner) =>
        // we assume the generic positions don't matter and to continue
        toJson(a, inner)
      case _ =>
        val vp =
          a match {
            case s: SumValue => Some((s.variant, s.value))
            case p: ProductValue => Some((0, p))
            case _ => None
          }

        vp match {
          case None =>
            a match {
              case ExternalValue(b: BigInteger) =>
                Some(Json.JNumberStr(b.toString))
              case _ => None
            }
          case Some((variant, prod)) =>
            Type.rootConst(tpe)
              .flatMap {
                case Type.TyConst(Type.Const.Defined(pn, n)) =>
                  defined(pn, n)
              }
              .flatMap { dt =>
                val cons = dt.constructors
                val (_, targs) = Type.applicationArgs(tpe)
                val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]
                cons.lift(variant).flatMap { cf =>
                  prod.toList.zip(cf.args).traverse { case (a1, (pn, t)) =>
                    toJson(a1, Type.substituteVar(t, replaceMap)).map((pn.asString, _))
                  }
                }
              }
              .map { ps => Json.JObject(ps) }
        }
    }
  }

  def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType[Any]] =
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
}
