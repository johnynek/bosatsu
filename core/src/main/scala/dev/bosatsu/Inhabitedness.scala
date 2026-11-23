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

  // The cycle rule only prunes data types on the current path, so a type
  // whose expansion opens k kind-* variables at each step (constructor
  // existentials, or a quantifier in a field) can have 2^k `cycleKey`s and
  // up to (2^k)! paths through them: k = 4 would never finish. One check
  // visits at most this many data types, then answers Unknown for the rest.
  // That is always safe: no rule needs a sub-answer to be Unknown in order
  // to answer Inhabited or Uninhabited, so turning a sub-answer into Unknown
  // can only turn other answers into Unknown.
  private val MaxDataTypeVisits = 10000

  /** The cycle rule's assumptions while checking one type: the `cycleKey`s
    * of the data types being expanded on the current path. It also carries
    * the data-type visit budget of the whole check, shared by every path.
    */
  private final class Seen private (keys: Set[Type], budget: Seen.Budget) {
    def contains(key: Type): Boolean = keys(key)
    def push(key: Type): Seen = new Seen(keys + key, budget)
    def spendVisit(): Boolean = budget.spend()
  }

  private object Seen {
    // Mutable so that sibling paths share one budget; each check makes its own.
    final class Budget(private var remaining: Int) {
      def spend(): Boolean =
        if (remaining > 0) {
          remaining -= 1
          true
        } else false
    }

    def start(): Seen = new Seen(Set.empty, new Budget(MaxDataTypeVisits))
  }

  /** Checks against one TypeEnv. Creating one computes the env's kind map,
    * which sorts every defined type, so reuse it for all the checks against
    * the same env (TotalityCheck makes one per package).
    */
  final class Checker(env: TypeEnv[Kind.Arg]) {
    private val an = Analyzer(env)

    def check(t: Type): Result[State] =
      an.validateType(t).andThen(_ => an.checkType(t, Map.empty, Seen.start()).validNec)

    def checkMatch(
        scrutinee: Type,
        pattern: Pattern[(PackageName, Constructor), Type]
    ): Result[State] =
      an.validateType(scrutinee) *>
        an.validatePattern(pattern).as(
          an.checkPattern(scrutinee, pattern, Map.empty, Seen.start())
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
    // keeps the set of keys finite: with `Step[a](t: T[a])`, renamed
    // existentials would otherwise give T[a], T[b], T[c], ... forever.
    // Finite is not small, so `MaxDataTypeVisits` bounds the search.
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
    ): Type =
      if (varState.isEmpty) t
      else {
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

    /** The existentials and field types of `cfn` applied to `args`, the type
      * arguments that `paramSub` maps its type parameters to.
      *
      * Unlike a quantifier inside a type, an existential is not named apart
      * from the arguments by `normalize`. So existentials that share a name
      * with a free variable of `args` (or with a variable a field uses
      * without declaring it) are renamed first. Otherwise evaluating them
      * with `varState ++ assignment` would capture that outer variable. Other
      * names need no avoiding: `seen` keys never name a variable that has a
      * state, and an outer variable that occurs in no field can't be
      * captured.
      */
    private def openConstructor(
        cfn: rankn.ConstructorFn[Kind.Arg],
        paramSub: Map[Type.Var, Type],
        args: List[Type]
    ): (List[(Type.Var.Bound, Kind)], List[Type]) = {
      val fields = cfn.args.map(_.tpe)
      NonEmptyList.fromList(cfn.exists.map { case (b, ka) => (b, ka.kind) }) match {
        case None =>
          (Nil, fields.map(Type.substituteVar(_, paramSub)))
        case Some(exists) =>
          val existNames = exists.iterator.map(_._1).toSet
          val avoid =
            Type.freeBoundTyVars(args).toSet ++
              Type.freeBoundTyVars(fields).filterNot(existNames)
          val (rename, exists1) = Type.unshadow(exists, avoid)
          val sub = paramSub ++ rename
          (exists1.toList, fields.map(Type.substituteVar(_, sub)))
      }
    }

    def checkType(
        t: Type,
        varState: Map[Type.Var.Bound, State],
        seen: Seen
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
                    if (!seen.spendVisit()) Unknown
                    else if (seen.contains(root)) Uninhabited
                    else {
                      val params = dt.typeParams
                      if (params.lengthCompare(args.length) != 0) Unknown
                      else {
                        val paramSub: Map[Type.Var, Type] =
                          params.iterator
                            .zip(args.iterator)
                            .map { case (v, arg) => (v: Type.Var) -> arg }
                            .toMap

                        val seen1 = seen.push(root)
                        val constructorStates =
                          dt.constructors.map { cfn =>
                            val (exists, argTypes) =
                              openConstructor(cfn, paramSub, args)
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
        seen: Seen
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
                          openConstructor(cfn, paramSub, scrArgs)
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
