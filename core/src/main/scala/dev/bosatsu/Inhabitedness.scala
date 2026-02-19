package dev.bosatsu

import cats.{Eq}
import cats.data.{NonEmptyList, ValidatedNec}
import cats.syntax.all._
import dev.bosatsu.rankn.Type
import dev.bosatsu.rankn.TypeEnv
import Identifier.Constructor

object Inhabitedness {
  enum State derives CanEqual {
    case Inhabited
    case Uninhabited
    case Unknown
  }

  object State {
    given Eq[State] = Eq.fromUniversalEquals

    def orStates(states: List[State]): State =
      if (states.contains(State.Inhabited)) State.Inhabited
      else if (states.forall(_ == State.Uninhabited)) State.Uninhabited
      else State.Unknown

    def andStates(states: List[State]): State =
      if (states.contains(State.Uninhabited)) State.Uninhabited
      else if (states.forall(_ == State.Inhabited)) State.Inhabited
      else State.Unknown
  }

  enum Error derives CanEqual {
    case TypeNotValueKind(tpe: Type, kind: Kind)
    case UnknownTypeConstructor(tpe: Type.TyConst)
    case UnknownTypeVariable(v: Type.Var.Bound)
    case IllKindedTypeApply(apply: Type.TyApply)
    case KindSubsumptionError(
        apply: Type.TyApply,
        expected: Kind,
        found: Kind
    )
    case UnknownConstructor(cons: (PackageName, Constructor))
    case ConstructorArityMismatch(
        cons: (PackageName, Constructor),
        expected: Int,
        found: Int
    )
  }

  type Result[+A] = ValidatedNec[Error, A]

  private val EmptyListCons: (PackageName, Constructor) =
    (PackageName.PredefName, Constructor("EmptyList"))
  private val NonEmptyListCons: (PackageName, Constructor) =
    (PackageName.PredefName, Constructor("NonEmptyList"))

  // Keep quantifier expansion finite and fast in adversarial inputs.
  private val MaxQuantifierVars = 10

  def check(t: Type, env: TypeEnv[Kind.Arg]): Result[State] = {
    val an = Analyzer(env)
    an.validateType(t).andThen(_ => an.checkType(t, Map.empty, Set.empty).validNec)
  }

  def checkMatch(
      scrutinee: Type,
      pattern: Pattern[(PackageName, Constructor), Type],
      env: TypeEnv[Kind.Arg]
  ): Result[State] = {
    val an = Analyzer(env)
    (an.validateType(scrutinee), an.validatePattern(pattern)).mapN { (_, _) =>
      an.checkPattern(scrutinee, pattern, Map.empty, Set.empty)
    }
  }

  private final case class Analyzer(env: TypeEnv[Kind.Arg]) {
    import Error._
    import State._

    private val kinds: Map[Type.Const.Defined, Kind] =
      Type.builtInKinds ++ env.toKindMap

    private val alwaysInhabited: Set[Type.Const.Defined] =
      Set(
        Type.IntType.tpe.toDefined,
        Type.StrType.tpe.toDefined,
        Type.CharType.tpe.toDefined,
        Type.Float64Type.tpe.toDefined,
        Type.BoolType.tpe.toDefined,
        Type.UnitType.tpe.toDefined
      )

    def validateType(t: Type): Result[Unit] =
      kindOf(t).toValidatedNec.andThen {
        case Kind.Type => ().validNec
        case k         => TypeNotValueKind(t.normalize, k).invalidNec
      }

    private def kindOf(t: Type): Either[Error, Kind] =
      Type.kindOf[Error](
        unknownVar = UnknownTypeVariable(_),
        invalidApply = IllKindedTypeApply(_),
        kindSubsumeError = (apply, expected, found) =>
          KindSubsumptionError(apply, expected.arg.kind, found),
        cons = tc => kinds.get(tc.tpe.toDefined).toRight(UnknownTypeConstructor(tc))
      )(t)

    def validatePattern(
        p: Pattern[(PackageName, Constructor), Type]
    ): Result[Unit] =
      p match {
        case Pattern.WildCard | Pattern.Var(_) | Pattern.Literal(_) |
            Pattern.StrPat(_) =>
          ().validNec
        case Pattern.Named(_, pat)      =>
          validatePattern(pat)
        case Pattern.ListPat(items)     =>
          items
            .collect { case Pattern.ListPart.Item(item) => item }
            .traverse_(validatePattern)
        case Pattern.Annotation(pat, t) =>
          validatePattern(pat) *> validateType(t)
        case Pattern.PositionalStruct(cons, params) =>
          val consCheck =
            env.getConstructor(cons._1, cons._2) match {
              case None => UnknownConstructor(cons).invalidNec
              case Some((_, cfn)) =>
                val expected = cfn.args.size
                val found = params.size
                if (expected == found) ().validNec
                else ConstructorArityMismatch(cons, expected, found).invalidNec
            }

          consCheck *> params.traverse_(validatePattern)
        case Pattern.Union(head, rest) =>
          (head :: rest.toList).traverse_(validatePattern)
      }

    private def varAssignments(
        vars: List[Type.Var.Bound]
    ): Option[NonEmptyList[Map[Type.Var.Bound, State]]] =
      if (vars.lengthCompare(MaxQuantifierVars) > 0) None
      else {
        @annotation.tailrec
        def loop(
            rem: List[Type.Var.Bound],
            acc: NonEmptyList[Map[Type.Var.Bound, State]]
        ): NonEmptyList[Map[Type.Var.Bound, State]] =
          rem match {
            case Nil => acc
            case h :: t =>
              val next = acc.flatMap { m =>
                NonEmptyList.of(
                  m.updated(h, Inhabited),
                  m.updated(h, Uninhabited)
                )
              }
              loop(t, next)
          }

        Some(loop(vars, NonEmptyList.one(Map.empty)))
      }

    private def evaluateQuantifier(
        vars: List[(Type.Var.Bound, Kind)],
        bodyEval: Map[Type.Var.Bound, State] => State,
        isForAll: Boolean,
        outerState: Map[Type.Var.Bound, State]
    ): State = {
      val kindsOk = vars.forall { case (_, k) => k == Kind.Type }
      if (!kindsOk) Unknown
      else {
        varAssignments(vars.map(_._1)) match {
          case None => Unknown
          case Some(assignments) =>
            val states = assignments.map(assignment => bodyEval(outerState ++ assignment))
            if (isForAll) {
              if (states.contains_(Uninhabited)) Uninhabited
              else if (states.forall(_ == Inhabited)) Inhabited
              else Unknown
            } else {
              if (states.contains_(Inhabited)) Inhabited
              else if (states.forall(_ == Uninhabited)) Uninhabited
              else Unknown
            }
        }
      }
    }

    def checkType(
        t: Type,
        varState: Map[Type.Var.Bound, State],
        seen: Set[Type]
    ): State = {
      val norm = t.normalize

      norm match {
        case Type.ForAll(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkType(in, vs, seen),
            isForAll = true,
            varState
          )
        case Type.Exists(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkType(in, vs, seen),
            isForAll = false,
            varState
          )
        case Type.Fun(args, res) =>
          val resultState = checkType(res, varState, seen)
          val argStates = args.map(checkType(_, varState, seen))
          if ((resultState == Uninhabited) && argStates.forall(_ == Inhabited))
            Uninhabited
          else if ((resultState == Inhabited) || argStates.contains_(Uninhabited))
            Inhabited
          else Unknown
        case Type.TyVar(b: Type.Var.Bound) =>
          varState.getOrElse(b, Unknown)
        case Type.TyVar(_: Type.Var.Skolem) | Type.TyMeta(_) =>
          Unknown
        case _ =>
          val (on, args) = Type.unapplyAll(norm)
          on match {
            case tc @ Type.TyConst(_) =>
              val cons = tc.tpe.toDefined
              if (alwaysInhabited(cons)) Inhabited
              else {
                env.getType(tc) match {
                  case None => Unknown
                  case Some(dt) =>
                    // `norm` is already the fully-applied normalized type for this branch;
                    // use it as the cycle-detection key directly.
                    val root = norm
                    if (seen(root)) Uninhabited
                    else {
                      val params = dt.typeParams
                      if (params.lengthCompare(args.length) != 0) Unknown
                      else {
                        val paramSub: Map[Type.Var, Type] =
                          params.iterator
                            .zip(args.iterator)
                            .map { case (v, arg) => (v: Type.Var) -> arg }
                            .toMap

                        val seen1 = seen + root
                        val constructorStates =
                          dt.constructors.map { cfn =>
                            val argTypes =
                              cfn.args.map(arg => Type.substituteVar(arg.tpe, paramSub))
                            evaluateQuantifier(
                              cfn.exists.map { case (b, ka) => (b, ka.kind) },
                              vs => {
                                State.andStates(
                                  argTypes.map(tpe => checkType(tpe, vs, seen1))
                                )
                              },
                              isForAll = false,
                              varState
                            )
                          }

                        if (constructorStates.isEmpty) Unknown
                        else State.orStates(constructorStates)
                      }
                    }
                }
              }
            case _ => Unknown
          }
      }
    }

    private def definitelyDisjoint(left: Type, right: Type): Boolean = {
      val (lroot, _) = Type.unapplyAll(left.normalize)
      val (rroot, _) = Type.unapplyAll(right.normalize)
      (lroot, rroot) match {
        case (ltc @ Type.TyConst(_), rtc @ Type.TyConst(_)) =>
          !ltc.sameAs(rtc)
        case _ => false
      }
    }

    private def definitelySameHead(left: Type, right: Type): Boolean = {
      val (lroot, _) = Type.unapplyAll(left.normalize)
      val (rroot, _) = Type.unapplyAll(right.normalize)
      (lroot, rroot) match {
        case (ltc @ Type.TyConst(_), rtc @ Type.TyConst(_)) =>
          ltc.sameAs(rtc)
        case _ => false
      }
    }

    private def listElementType(t: Type): Option[Type] = {
      val (root, args) = Type.unapplyAll(t.normalize)
      root match {
        case tc @ Type.TyConst(_)
            if tc.sameAs(Type.ListType) && (args.lengthCompare(1) == 0) =>
          args.headOption
        case _ => None
      }
    }

    def checkPattern(
        scrutinee: Type,
        pattern: Pattern[(PackageName, Constructor), Type],
        varState: Map[Type.Var.Bound, State],
        seen: Set[Type]
    ): State =
      pattern match {
        case Pattern.WildCard | Pattern.Var(_) =>
          checkType(scrutinee, varState, seen)
        case Pattern.Named(_, pat) =>
          checkPattern(scrutinee, pat, varState, seen)
        case Pattern.Annotation(pat, tpe) =>
          if (definitelyDisjoint(scrutinee, tpe)) Uninhabited
          else {
            val scrState = checkType(scrutinee, varState, seen)
            lazy val annState = checkType(tpe, varState, seen)
            lazy val patState = checkPattern(tpe, pat, varState, seen)

            if (
              (scrState == Uninhabited) || (annState == Uninhabited) || (patState == Uninhabited)
            ) Uninhabited
            else if (
              (scrState == Inhabited) && (annState == Inhabited) &&
                (patState == Inhabited) && definitelySameHead(scrutinee, tpe)
            ) Inhabited
            else Unknown
          }
        case Pattern.Union(head, rest) =>
          State.orStates(
            (head :: rest.toList).map(checkPattern(scrutinee, _, varState, seen))
          )
        case Pattern.Literal(lit)      =>
          if (scrutinee.sameAs(Type.getTypeOf(lit))) Inhabited
          else Uninhabited
        case _: Pattern.StrPat =>
          if (scrutinee.sameAs(Type.StrType)) Inhabited
          else Uninhabited
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, EmptyListCons, NonEmptyListCons) match {
            case Right(asStruct) =>
              checkPattern(scrutinee, asStruct, varState, seen)
            case Left((_, suffix)) =>
              // Prefix-glob list patterns (for example: `*_, p1, p2`) do not
              // lower to a simple cons spine. We still know they require a list
              // scrutinee and each explicit item in the suffix to be matchable
              // against the list element type. We use that to prove definite
              // `Uninhabited` cases; otherwise we stay conservative with `Unknown`.
              checkType(scrutinee, varState, seen) match {
                case Uninhabited => Uninhabited
                case scrState =>
                  listElementType(scrutinee) match {
                    case None => Unknown
                    case Some(itemType) =>
                      val itemStates =
                        suffix.toList.collect { case Pattern.ListPart.Item(itemPat) =>
                          checkPattern(itemType, itemPat, varState, seen)
                        }
                      if (itemStates.contains(Uninhabited)) Uninhabited
                      else if ((scrState == Inhabited) && itemStates.forall(_ == Inhabited))
                        // We only conclude Inhabited when the scrutinee is known inhabited.
                        Inhabited
                      else Unknown
                  }
              }
          }
        case Pattern.PositionalStruct(cons, params) =>
          env.getConstructor(cons._1, cons._2) match {
            case None =>
              Unknown
            case Some((dt, cfn)) =>
              val (scrRoot, scrArgs) = Type.unapplyAll(scrutinee.normalize)
              scrRoot match {
                case tc @ Type.TyConst(_) if tc.sameAs(dt.toTypeTyConst) =>
                  val tparams = dt.typeParams
                  if (tparams.lengthCompare(scrArgs.length) != 0) Unknown
                  else if (cfn.args.size != params.size) Unknown
                  else {
                    val paramSub: Map[Type.Var, Type] =
                      tparams.iterator
                        .zip(scrArgs.iterator)
                        .map { case (v, arg) => (v: Type.Var) -> arg }
                        .toMap

                    val fieldTypes =
                      cfn.args.map(arg => Type.substituteVar(arg.tpe, paramSub))
                    evaluateQuantifier(
                      cfn.exists.map { case (b, ka) => (b, ka.kind) },
                      vs => {
                        State.andStates(
                          fieldTypes.iterator
                            .zip(params.iterator)
                            .map { case (fieldType, pat) =>
                              checkPattern(fieldType, pat, vs, seen)
                            }
                            .toList
                        )
                      },
                      isForAll = false,
                      varState
                    )
                  }
                case _: Type.TyConst =>
                  Uninhabited
                case _ =>
                  Unknown
              }
          }
      }
  }
}
