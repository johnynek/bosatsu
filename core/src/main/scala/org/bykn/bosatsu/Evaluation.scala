package org.bykn.bosatsu

import cats.Eval
import cats.data.NonEmptyList
import org.bykn.bosatsu.graph.Memoize
import org.bykn.bosatsu.pattern.Matcher
import java.math.BigInteger
import org.bykn.bosatsu.pattern.{NamedSeqPattern, Splitter}
import org.bykn.bosatsu.rankn.{DefinedType, Type, DataRepr}
import scala.collection.mutable.{Map => MMap}

import cats.implicits._

import Identifier.{Bindable, Constructor}

object Evaluation {
  import Value._

  type Env = Map[Identifier, Eval[Value]]

  sealed abstract class Scoped {
    import Scoped.fromFn

    def inEnv(env: Env): Value

    def letNameIn(name: Bindable, in: Scoped): Scoped =
      Scoped.Let(name, this, in)

    def branch(fn: (Value, Env) => Value): Scoped =
      fromFn { env =>
        val v1 = inEnv(env)
        fn(v1, env)
      }

    def asLambda(name: Bindable): Scoped =
      fromFn { env =>
        FnValue { v => inEnv(env.updated(name, Eval.now(v))) }
      }

    def applyArg(arg: Scoped): Scoped =
      fromFn { env =>
        val fnE = inEnv(env)
        val argE = arg.inEnv(env)
        // safe because we typecheck
        fnE.asFn(argE)
      }
  }

  object Scoped {
    def const(e: => Value): Scoped = {
      lazy val computedE = e
      fromFn(_ => computedE)
    }

    def fromEnv(identifier: Bindable): Scoped =
      fromFn { env =>
        env.get(identifier) match {
          case Some(e) => e.value
          case None => sys.error(s"could not find $identifier in the environment with keys: ${env.keys.toList.sorted}")
        }
      }

    def recursive(name: Bindable, item: Scoped): Scoped =
      fromFn { env =>
        lazy val res: Eval[Value] =
          Eval.later(item.inEnv(env1))
        lazy val env1: Env =
          env.updated(name, res)

        res.value
      }


    private def continueScoped(names: List[Bindable], registers: MMap[Bindable, Value]): Value =
      FnValue.curry(names.size) { eargs =>
        var n = names
        var a = eargs
        while (n.nonEmpty) {
          registers(n.head) = a.head
          n = n.tail
          a = a.tail
        }

        null
      }

    def loop(arg: Bindable, params: List[Bindable], body: Scoped): Scoped = {

      def loopBody(args: List[Bindable])(bodyFn: MMap[Bindable, Value] => Scoped): Scoped =
        fromFn { env =>
          var res: Value = null
          var first = true
          val registers: MMap[Bindable, Value] =
            MMap(args.map { a => (a, env(a).value) } :_*)

          val body = bodyFn(registers)
          while(res eq null) {
            val e1 =
              if (!first) {
                registers.foldLeft(env) { case (e, (k, v)) => e.updated(k, Eval.now(v)) }
              }
              else env

            res = body.inEnv(e1)
            first = false
          }

          res
        }

       val bodyScoped = loopBody(params) { regs =>
         // in the body, we call the continueScoped
         // to update the mutable variables in the loop
         // and just return null, above we loop the while
         // loop with the result is null
         Scoped.const(Scoped.continueScoped(params, regs))
           .letNameIn(arg, body)
       }

       Scoped.fromFn { env =>
         FnValue.curry(params.size) { eargs =>
           var n = params
           var a = eargs
           var env1 = env
           while (n.nonEmpty) {
             env1 = env1.updated(n.head, Eval.now(a.head))
             n = n.tail
             a = a.tail
           }
           bodyScoped.inEnv(env1)
         }
       }
    }

    private case class Let(name: Bindable, arg: Scoped, in: Scoped) extends Scoped {
      def inEnv(env: Env) =
        in.inEnv(env.updated(name, Eval.later(arg.inEnv(env))))
    }

    private def fromFn(fn: Env => Value): Scoped =
      FromFn(fn)

    private case class FromFn(fn: Env => Value) extends Scoped {
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
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
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
          (cf.name, Eval.now(constructor(cf.name, dt)))
        }
      }
      .toMap

  private def addLet(env: Env, let: (Bindable, RecursionKind, TypedExpr[T])): Env = {
    val (name, rec, e) = let
    val eres = evalLetValue(name, e, rec)(eval)

    // These are added lazily
    env.updated(name, Eval.later(eres.inEnv(env)))
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

  def evaluateName(p: PackageName, name: Identifier): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (_, _, tpe) <- pack.program.lets.filter { case (n, _, _) => n == name }.lastOption
      value <- evaluate(pack).get(name)
    } yield (value, tpe.getType)

  /**
   * Return the last test, if any, in the package.
   * this is the test that is run when we test
   * the package
   */
  def lastTest(p: PackageName): Option[Eval[Value]] =
    for {
      pack <- pm.toMap.get(p)
      name <- pack.program.lets.collect { case (name, _, te) if te.getType == Type.TestType => name }.lastOption
      value <- evaluate(pack).get(name)
    } yield value

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

  def evalTest(ps: PackageName): Option[Eval[Test]] =
    lastTest(ps).map { ea =>
      def toAssert(a: ProductValue): Test =
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
      def toSuite(a: ProductValue): Test =
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

      ea.map(toTest(_))
    }

  private type Ref = TypedExpr[T]

  private type Pat = Pattern[(PackageName, Constructor), Type]

  private val emptyEnv: Option[Env] = Some(Map.empty)

  private val anyMatch: Any => Option[Env] =
    { a: Any => emptyEnv }

  private val neverMatch: Any => Option[Env] =
    { a: Any => None }

  private lazy val patListSplitter =
    Splitter.listSplitter(patternMatcher)(
      new cats.Monoid[Env] {
        val empty = Map.empty[Identifier, Eval[Value]]

        def combine(left: Env, right: Env) = left ++ right

        override def combineAll(vs: TraversableOnce[Env]) = {
          val eb = Map.newBuilder[Identifier, Eval[Value]]
          vs.foreach(eb ++= _)
          eb.result
        }
      })

  private lazy val patternMatcher: Matcher[Pat, Value, Env] =
    new Matcher[Pat, Value, Env] { self =>
      def apply(pat: Pat): Value => Option[Env] =
        pat match {
          case Pattern.WildCard => anyMatch
          case Pattern.Literal(lit) =>
            val vlit = Value.fromLit(lit)

            { v => if (v == vlit) emptyEnv else None }
          case Pattern.Var(n) =>

            { v => Some(Map.empty.updated(n, Eval.now(v))) }
          case Pattern.Named(n, p) =>
            val inner = self(p)

            { v =>
              inner(v) match {
                case None => None
                case Some(env1) => Some(env1.updated(n, Eval.now(v)))
              }
            }
          case pat@Pattern.StrPat(_) =>
            if (pat.names.isEmpty) {
              // we can just use the simple matcher
              { v =>
                // this casts should be safe if we have typechecked
                val str = v.asExternal.toAny.asInstanceOf[String]
                if (pat.matches(str)) emptyEnv
                else None
              }
            }
            else {
              val spat = pat.toNamedSeqPattern
              val nameMap = spat.names.map { n =>
                (n, Identifier.unsafe(n))
              }.toMap

              val matcher = NamedSeqPattern.matcher[Char, Char, String, String](Splitter.stringSplitter(_.toString))(spat)

              { v =>
                // this casts should be safe if we have typechecked
                val str = v.asExternal.toAny.asInstanceOf[String]
                matcher(str).map { _._2.map { case (k, v) => (nameMap(k), Eval.now(ExternalValue(v))) } }
              }
            }
          case lp@Pattern.ListPat(items) =>
            val lpat = lp.toNamedSeqPattern

            if (lp.names.isEmpty) {
              // this is a cheaper operation
              val matcher = NamedSeqPattern.matcher[Pat, Value, List[Value], Unit](
                Splitter.listSplitter(self.map(_ => ()))
              )
              val fn = matcher(lpat)

              {
                case VList(vs) => if (fn(vs).isDefined) emptyEnv else None
                case _ => None
              }
            }
            else {
              val nameMap = lpat.names.map { n =>
                (n, Identifier.unsafe(n))
              }.toMap

              val fn: List[Value] => Option[Env] =
                (NamedSeqPattern.matcher[Pattern[(PackageName, Constructor), Type], Value, List[Value], Env](patListSplitter)
                  .map { case (env0, captures) =>
                    captures.foldLeft(env0) { case (env0, (k, listV)) =>
                      env0.updated(Identifier.unsafe(k), Eval.now(VList(listV)))
                    }
                  })(lpat)

                {
                  case VList(vs) => fn(vs)
                  case other =>
                    //$COVERAGE-OFF$
                    sys.error(s"ill-typed, expected list: $other")
                    //$COVERAGE-ON$
                }
            }
          case Pattern.Annotation(p, _) =>
            // we don't need types to match patterns once we have fully resolved constructors
            self(p)
          case u@Pattern.Union(_, _) =>
            // we can just loop expanding these out:
            def loop(ps: List[Pattern[(PackageName, Constructor), Type]]): Value => Option[Env] =
              ps match {
                case Nil => neverMatch
                case head :: tail =>
                  val fnh = self(head)
                  val fnt = loop(tail)

                  { arg =>
                    val rh = fnh(arg)
                    if (rh.isDefined) rh
                    else fnt(arg)
                  }
              }
            loop(Pattern.flatten(u).toList)
          case Pattern.PositionalStruct(pc@(_, ctor), items) =>
            def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
              pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2

            val itemFns = items.map(self(_))

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
            val itemsWild = items.forall(isTotalNonBinding)

            def processArgs(as: List[Value]): Option[Env] = {
              // manually write out foldM hoping for performance improvements
              @annotation.tailrec
              def loop(vs: List[Value], fns: List[Value => Option[Env]], acc: Env): Option[Env] =
                vs match {
                  case Nil => Some(acc)
                  case vh :: vt =>
                    fns match {
                      case fh :: ft =>
                        fh(vh) match {
                          case None => None
                          case Some(env1) => loop(vt, ft, acc ++ env1)
                        }
                      case Nil =>
                        // $COVERAGE-OFF$
                        sys.error(s"mismatch in size, shouldn't happen, statically: $as, $fns")
                        // $COVERAGE-ON$
                    }
                }
              loop(as, itemFns, Map.empty)
            }

            /*
             * The type in question is not the outer dt, but the type associated
             * with this current constructor
             */
            val dt: DefinedType[Any] = definedForCons(pc)

            dt.dataRepr(ctor) match {
              case DataRepr.NewType =>
                // we erase this entirely, and just match the underlying item
                itemFns.head
              case DataRepr.Struct(_) =>
                if (itemsWild) anyMatch
                else
                  // this is a struct, which means we expect it
                  { (arg: Value) =>
                    arg match {
                      case p: ProductValue =>
                        // this is the pattern
                        // note passing in a List here we could have union patterns
                        // if all the pattern bindings were known to be of the same type
                        processArgs(p.toList)

                      case other =>
                        // $COVERAGE-OFF$this should be unreachable
                        val itemStr = items.mkString("(", ", ", ")")
                        sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\n")
                        // $COVERAGE-ON$
                    }
                  }
              case DataRepr.Enum(idx, _) =>
                if (itemsWild)
                  { (arg: Value) =>
                    arg match {
                      case s: SumValue =>
                        if (s.variant == idx) emptyEnv
                        else None
                      case other =>
                        // $COVERAGE-OFF$this should be unreachable
                        sys.error(s"ill typed in match: $other")
                        // $COVERAGE-ON$
                    }
                  }
                else
                  { (arg: Value) =>
                    arg match {
                      case s: SumValue =>
                        if (s.variant == idx) processArgs(s.value.toList)
                        else None
                      case other =>
                        // $COVERAGE-OFF$this should be unreachable
                        sys.error(s"ill typed in match: $other")
                        // $COVERAGE-ON$
                    }
                  }

              case DataRepr.ZeroNat =>
                { (arg: Value) =>
                  arg match {
                    case ExternalValue(b: BigInteger) =>
                      if (b == BigInteger.ZERO) emptyEnv
                      else None
                    case other =>
                      // $COVERAGE-OFF$this should be unreachable
                      val itemStr = items.mkString("(", ", ", ")")
                      sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\n")
                      // $COVERAGE-ON$
                  }
                }

              case DataRepr.SuccNat =>
                // We are expecting non-zero
                val succMatch = itemFns.head
                if (itemsWild) {
                  { (arg: Value) =>
                    arg match {
                      case ExternalValue(b: BigInteger) =>
                        if (b != BigInteger.ZERO) emptyEnv
                        else None
                      case other =>
                        // $COVERAGE-OFF$this should be unreachable
                        val itemStr = items.mkString("(", ", ", ")")
                        sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\n")
                        // $COVERAGE-ON$
                    }
                  }
                }
                else {
                  { (arg: Value) =>
                    arg match {
                      case ExternalValue(b: BigInteger) =>
                        if (b != BigInteger.ZERO) {
                          succMatch(ExternalValue(b.subtract(BigInteger.ONE)))
                        }
                        else None
                      case other =>
                        // $COVERAGE-OFF$this should be unreachable
                        val itemStr = items.mkString("(", ", ", ")")
                        sys.error(s"ill typed in match (${ctor.asString}$itemStr\n\n$other\n\n")
                        // $COVERAGE-ON$
                    }
                  }
                }
              }
            }
          }

  private def evalBranch(
    branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])],
    recurse: Ref => Scoped): (Value, Env) => Value = {

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
            val pfn = patternMatcher(p)
            val fntail = bindEnv(tail)

            { (arg, env) =>
              pfn(arg) match {
                case Some(env1) => (env ++ env1, e)
                case None => fntail(arg, env)
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

  private def evalLetValue(arg: Bindable, e: TypedExpr[T], rec: RecursionKind)(eval: Ref => Scoped): Scoped = {
    lazy val e0 = eval(e)
    if (rec.isRecursive) {
      // this could be tail recursive
      if (TypedExpr.selfCallKind(arg, e) == TypedExpr.SelfCallKind.TailCall) {
        val arity = Type.Fun.arity(e.getType)
        TypedExpr.toArgsBody(arity, e) match {
          case Some((params, body)) =>
            Scoped.loop(arg, params.map(_._1), eval(body))
          case None =>
            // TODO: I don't think this case should ever happen
            Scoped.recursive(arg, e0)
        }
      }
      else Scoped.recursive(arg, e0)
    }
    else e0
  }

  /**
   * TODO, expr is a TypedExpr so we already know the type. returning it does not do any good that I
   * can see.
   */
  private def evalTypedExpr(expr: TypedExpr[T], recurse: Ref => Scoped): Scoped = {

    import TypedExpr._

     expr match {
       case Generic(_, e, _) =>
         // types aren't needed to evaluate
         evalTypedExpr(e, recurse)
       case Annotation(e, _, _) =>
         // types aren't needed to evaluate
         evalTypedExpr(e, recurse)
       case Local(ident, _, _) =>
         Scoped.fromEnv(ident)
       case Global(p, ident, _, _) =>
         val pack = pm.toMap.get(p).getOrElse(sys.error(s"cannot find $p, shouldn't happen due to typechecking"))
         // const is lazy so this won't run until needed
         Scoped.const(getValue(pack, ident).value)
       case App(fn, arg, _, _) =>
         val efn = recurse(fn)
         val earg = recurse(arg)

         efn.applyArg(earg)
       case AnnotatedLambda(name, _, expr, _) =>
         recurse(expr).asLambda(name)
       case l@Let(arg, e, in, rec, _) =>
         val eres = evalLetValue(arg, e, rec)(recurse)
         val inres = recurse(in)

         eres.letNameIn(arg, inres)
       case Literal(lit, _, _) =>
         Scoped.const(Value.fromLit(lit))
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
    Memoize.memoizeDagHashedConcurrent[Ref, Scoped] {
      (expr, recurse) => evalTypedExpr(expr, recurse)
    }

  private[this] val zeroNat: Value = ExternalValue(BigInteger.ZERO)
  private[this] val succNat: Value = {
    def inc(v: Value): Value = {
      val bi = v.asExternal.toAny.asInstanceOf[BigInteger]
      ExternalValue(bi.add(BigInteger.ONE))
    }
    FnValue(inc(_))
  }

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): Value = {
    // partially erase new-types
    // more advanced new-type erasure would
    // make Apply(Foo, x) into x, but this
    // does not yet do that
    dt.dataRepr(c) match {
      case DataRepr.NewType => FnValue.identity
      case DataRepr.Struct(arity) =>
        FnValue.curry(arity)(ProductValue.fromList(_))
      case DataRepr.Enum(variant, arity) =>
        FnValue.curry(arity) { args =>
          val prod = ProductValue.fromList(args)
          SumValue(variant, prod)
        }
      case DataRepr.ZeroNat => zeroNat
      case DataRepr.SuccNat => succNat
    }
  }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  val valueToJson: ValueToJson = ValueToJson({
    case Type.Const.Defined(pn, t) =>
      for {
        pack <- pm.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
      } yield dt
  })

  /**
   * Convert a typechecked value to Doc
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  val valueToDoc: ValueToDoc = ValueToDoc({
    case Type.Const.Defined(pn, t) =>
      for {
        pack <- pm.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
      } yield dt
  })
}
