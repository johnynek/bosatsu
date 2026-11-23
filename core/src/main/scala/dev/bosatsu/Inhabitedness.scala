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

  /** Checks against one TypeEnv. Creating one computes the env's kind map,
    * which sorts every defined type, so reuse it for all the checks against
    * the same env (TotalityCheck makes one per package).
    */
  final class Checker(env: TypeEnv[Kind.Arg]) {
    private val an = Analyzer(env)

    def check(t: Type): Result[State] =
      an.validateType(t).andThen(_ => an.checkType(t, Map.empty, Set.empty).validNec)

    def checkMatch(
        scrutinee: Type,
        pattern: Pattern[(PackageName, Constructor), Type]
    ): Result[State] =
      an.validateType(scrutinee) *>
        an.validatePattern(pattern).as(
          an.checkPattern(scrutinee, pattern, Map.empty, Set.empty)
        )
  }

  def check(t: Type, env: TypeEnv[Kind.Arg]): Result[State] =
    new Checker(env).check(t)

  def checkMatch(
      scrutinee: Type,
      pattern: Pattern[(PackageName, Constructor), Type],
      env: TypeEnv[Kind.Arg]
  ): Result[State] =
    new Checker(env).checkMatch(scrutinee, pattern)

  private final case class Analyzer(env: TypeEnv[Kind.Arg]) {
    import Error._
    import State._

    private val kinds: Map[Type.Const.Defined, Kind] =
      env.toKindMap

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

    // The `seen` key of a data type replaces each free variable that has an
    // Inhabited (Uninhabited) state with the marker `$inhabited`
    // (`$uninhabited`). Invariant: two types with the same key are inhabited
    // or not together, under every assignment of types to variables that
    // agrees with the states. Variables are only ever read through their
    // state, so swapping one variable for another with the same state
    // changes nothing. The cycle rule (a type met again while expanding it
    // is Uninhabited) is therefore sound for any type with the same key.
    //
    // Keys never mention a variable that has a state by name, so a binder
    // opened later can't match an older key by reusing that name. That is
    // why the binders of quantifiers inside field types need no renaming:
    // `normalize` names them apart from their body's free variables, so the
    // outer variable they shadow in `varState` can't occur in the body, and
    // the only other place names matter is `seen`. Keying by state also
    // keeps the expansion finite: with `Step[a](t: T[a])`, renamed
    // existentials would otherwise give T[a], T[b], T[c], ... forever.
    //
    // A variable without a state keeps its name. Only constructor fields
    // with an undeclared variable produce one: `validateType` rejects free
    // variables in the checked type and in pattern annotations. Names parsed
    // from source or made by `alignBinders` can't start with `$`, so they
    // never match a marker.
    private val inhabitedMarker: Type =
      Type.TyVar(Type.Var.Bound("$inhabited"))
    private val uninhabitedMarker: Type =
      Type.TyVar(Type.Var.Bound("$uninhabited"))

    private def cycleKey(
        t: Type,
        varState: Map[Type.Var.Bound, State]
    ): Type = {
      val sub: Map[Type.Var, Type] =
        Type
          .freeBoundTyVars(t :: Nil)
          .iterator
          .flatMap { b =>
            varState.get(b) match {
              case Some(Inhabited)   => Some((b: Type.Var) -> inhabitedMarker)
              case Some(Uninhabited) => Some((b: Type.Var) -> uninhabitedMarker)
              case Some(Unknown) | None => None
            }
          }
          .toMap

      if (sub.isEmpty) t
      else Type.substituteVar(t, sub).normalize
    }

    /** The existentials and field types of `cfn` applied to the type
      * arguments in `paramSub`. Existentials whose names are in `avoid` are
      * renamed apart first, so evaluating them with `varState ++ assignment`
      * can't capture an outer variable of the same name that `paramSub`
      * substitutes into a field. Unlike a quantifier inside a type, an
      * existential is not named apart from the arguments by `normalize`.
      */
    private def openConstructor(
        cfn: rankn.ConstructorFn[Kind.Arg],
        paramSub: Map[Type.Var, Type],
        avoid: => Set[Type.Var.Bound]
    ): (List[(Type.Var.Bound, Kind)], List[Type]) = {
      val rename: Map[Type.Var.Bound, Type.Var.Bound] =
        if (cfn.exists.isEmpty) Map.empty
        else {
          val avoidSet = avoid
          val (clashing, kept) = cfn.exists.map(_._1).partition(avoidSet)
          if (clashing.isEmpty) Map.empty
          else {
            val keptSet = kept.toSet
            Type
              .alignBinders(clashing, b => avoidSet(b) || keptSet(b))
              .toMap
          }
        }

      val sub: Map[Type.Var, Type] =
        paramSub ++ rename.iterator.map { case (b, b1) =>
          (b: Type.Var) -> (Type.TyVar(b1): Type)
        }
      val exists = cfn.exists.map { case (b, ka) =>
        (rename.getOrElse(b, b), ka.kind)
      }
      (exists, cfn.args.map(arg => Type.substituteVar(arg.tpe, sub)))
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
                    val root = cycleKey(norm, varState)
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
                        lazy val avoid: Set[Type.Var.Bound] =
                          varState.keySet ++
                            Type.freeBoundTyVars(args ::: seen1.toList)
                        val constructorStates =
                          dt.constructors.map { cfn =>
                            val (exists, argTypes) =
                              openConstructor(cfn, paramSub, avoid)
                            evaluateQuantifier(
                              exists,
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
    ): State = {
      val scrutineeNorm = scrutinee.normalize

      scrutineeNorm match {
        case Type.ForAll(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkPattern(in, pattern, vs, seen),
            isForAll = true,
            varState
          )
        case Type.Exists(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkPattern(in, pattern, vs, seen),
            isForAll = false,
            varState
          )
        case _ =>
          pattern match {
            case Pattern.WildCard | Pattern.Var(_) =>
              checkType(scrutineeNorm, varState, seen)
            case Pattern.Named(_, pat) =>
              checkPattern(scrutineeNorm, pat, varState, seen)
            case Pattern.Annotation(pat, tpe) =>
              if (definitelyDisjoint(scrutineeNorm, tpe)) Uninhabited
              else {
                val scrState = checkType(scrutineeNorm, varState, seen)
                lazy val annState = checkType(tpe, varState, seen)
                lazy val patState = checkPattern(tpe, pat, varState, seen)

                if (
                  (scrState == Uninhabited) || (annState == Uninhabited) || (patState == Uninhabited)
                ) Uninhabited
                else if (
                  (scrState == Inhabited) && (annState == Inhabited) &&
                    (patState == Inhabited) && definitelySameHead(scrutineeNorm, tpe)
                ) Inhabited
                else Unknown
              }
            case Pattern.Union(head, rest) =>
              State.orStates(
                (head :: rest.toList).map(checkPattern(scrutineeNorm, _, varState, seen))
              )
            case Pattern.Literal(lit)      =>
              if (scrutineeNorm.sameAs(Type.getTypeOf(lit))) Inhabited
              else Uninhabited
            case _: Pattern.StrPat =>
              if (scrutineeNorm.sameAs(Type.StrType)) Inhabited
              else Uninhabited
            case lp @ Pattern.ListPat(_) =>
              Pattern.ListPat.toPositionalStruct(lp, EmptyListCons, NonEmptyListCons) match {
                case Right(asStruct) =>
                  checkPattern(scrutineeNorm, asStruct, varState, seen)
                case Left((_, suffix)) =>
                  // Prefix-glob list patterns (for example: `*_, p1, p2`) do not
                  // lower to a simple cons spine. We still know they require a list
                  // scrutinee and each explicit item in the suffix to be matchable
                  // against the list element type. We use that to prove definite
                  // `Uninhabited` cases; otherwise we stay conservative with `Unknown`.
                  checkType(scrutineeNorm, varState, seen) match {
                    case Uninhabited => Uninhabited
                    case scrState =>
                      listElementType(scrutineeNorm) match {
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
                  val (scrRoot, scrArgs) = Type.unapplyAll(scrutineeNorm)
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

                        // Annotation types in `params` need no avoiding:
                        // `validatePattern` rejects free variables in them,
                        // so they can't refer to an outer variable.
                        val (exists, fieldTypes) =
                          openConstructor(
                            cfn,
                            paramSub,
                            varState.keySet ++
                              Type.freeBoundTyVars(scrArgs ::: seen.toList)
                          )
                        evaluateQuantifier(
                          exists,
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
  }
}
