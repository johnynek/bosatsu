package dev.bosatsu

import cats.{Eq}
import cats.data.{NonEmptyList, ValidatedNec}
import cats.syntax.all._
import dev.bosatsu.rankn.{ConstructorFn, Type, TypeEnv}
import scala.collection.mutable
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

  // Regular data types reach finitely many `dataKey`s, but polymorphic
  // recursion such as `T[a] = C(T[List[a]])` reaches new ones forever, and
  // `T[a] = C(T[(a, a)])` doubles the key's size at each step. One check
  // solves at most `MaxDataTypeKeys` keys, each of at most
  // `MaxDataTypeKeySize` nodes counted as a tree, and the others are Unknown.
  // That is safe because a key past a limit drops out of the equations and
  // is read as Unknown, that is, as either answer. Every rule gives a lower
  // and an upper bound that hold whatever value such a read has (Unknown
  // under a function argument bounds the other way round), so a definite
  // answer computed from it is right.
  private val MaxDataTypeKeys = 1000
  private val MaxDataTypeKeySize = 256

  // A component whose keys read each other under function arguments is
  // solved by a depth-first search instead (see `Solver`). That search can
  // follow up to (keys)! paths, so it expands at most `MaxSearchVisits` keys
  // per component and follows paths of at most `MaxSearchDepth` keys,
  // answering Unknown past either. Within the search, an Unknown sub-answer
  // can only turn other answers into Unknown.
  private val MaxSearchVisits = 10000
  private val MaxSearchDepth = 32

  /** How `checkType` reads the answer for a data type's `dataKey`. Under a
    * function argument it reads through `contravariant`, so a reader can
    * tell which keys are read where a larger answer gives a smaller result.
    */
  private trait DataStates {
    def stateOf(key: Type): State
    def contravariant: DataStates
  }

  /** Checks against one TypeEnv. Creating one computes the env's kind map,
    * which sorts every defined type, so reuse it for all the checks against
    * the same env (TotalityCheck makes one per package).
    */
  final class Checker(env: TypeEnv[Kind.Arg]) {
    private val an = Analyzer(env)

    def check(t: Type): Result[State] =
      an.validateType(t).andThen { _ =>
        an.checkType(t, Map.empty, an.newSolver()).validNec
      }

    def checkMatch(
        scrutinee: Type,
        pattern: Pattern[(PackageName, Constructor), Type]
    ): Result[State] =
      an.validateType(scrutinee) *>
        an.validatePattern(pattern).as(
          an.checkPattern(scrutinee, pattern, Map.empty, an.newSolver())
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

    // Data types are least fixed points, so whether a data type has a value
    // is the least solution of a system with one equation per data type the
    // check reaches: `D[args]` is the `or` over D's constructors of the
    // `exists` over their existentials of the `and` of their fields.
    //
    // The unknowns of that system are `dataKey`s. A key replaces each free
    // variable that has an Inhabited (Uninhabited) state with the variable
    // `$inhabited` (`$uninhabited`), which always has that state. Invariant:
    // a key and the type it came from are inhabited or not together, under
    // every assignment of types to variables that agrees with the states.
    // Variables are only ever read through their state, so swapping one for
    // another with the same state changes nothing. So a key is a closed
    // equation: `expand` unfolds it knowing only the markers' states, and two
    // types with the same key share one unknown.
    //
    // Keys never mention a variable that has a state by name, so a binder
    // opened later can't be confused with an older one by reusing its name.
    // That is why the binders of quantifiers inside field types need no
    // renaming: `normalize` names them apart from their body's free
    // variables, so the outer variable they shadow in `varState` can't occur
    // in the body. Keying by state also keeps the keys of a regular data type
    // finite: with `Step[a](t: T[a])`, renamed existentials would otherwise
    // give T[a], T[b], T[c], ... forever.
    //
    // A variable without a state keeps its name and is Unknown. Only
    // constructor fields with an undeclared variable produce one:
    // `validateType` rejects free variables in the checked type and in
    // pattern annotations. Names parsed from source or made by
    // `alignBinders` can't start with `$`, so they never match a marker.
    private val inhabitedMarker: Type.Var.Bound = Type.Var.Bound("$inhabited")
    private val uninhabitedMarker: Type.Var.Bound =
      Type.Var.Bound("$uninhabited")
    private val markerStates: Map[Type.Var.Bound, State] =
      Map(inhabitedMarker -> Inhabited, uninhabitedMarker -> Uninhabited)

    private def dataKey(
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
                case Some(Inhabited) =>
                  Some((b: Type.Var) -> Type.TyVar(inhabitedMarker))
                case Some(Uninhabited) =>
                  Some((b: Type.Var) -> Type.TyVar(uninhabitedMarker))
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
      * names need no avoiding: keys never name a variable that has a state,
      * and an outer variable that occurs in no field can't be captured.
      */
    private def openConstructor(
        cfn: ConstructorFn[Kind.Arg],
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

    /** One unfolding of the data type `key`, a `dataKey`, reading the data
      * types in its fields through `dataState`.
      *
      * It asks `dataState` for the same keys, in the same positions, whatever
      * `dataState` answers: every rule evaluates all of its parts.
      * `Solver.solveFrom` relies on that.
      */
    private def expand(key: Type, dataState: DataStates): State = {
      val (on, args) = Type.unapplyAll(key)
      on match {
        case tc @ Type.TyConst(_) =>
          env.getType(tc) match {
            case Some(dt)
                if dt.typeParams.lengthCompare(args.length) == 0 &&
                  dt.constructors.nonEmpty =>
              val paramSub: Map[Type.Var, Type] =
                dt.typeParams.iterator
                  .zip(args.iterator)
                  .map { case (v, arg) => (v: Type.Var) -> arg }
                  .toMap

              State.orStates(dt.constructors.map { cfn =>
                val (exists, fields) = openConstructor(cfn, paramSub, args)
                evaluateQuantifier(
                  exists,
                  vs => State.andStates(fields.map(checkType(_, vs, dataState))),
                  isForAll = false,
                  markerStates
                )
              })
            case _ => Unknown
          }
        case _ => Unknown
      }
    }

    // Whether `t` has at most `limit` nodes counted as a tree: a shared
    // subterm counts once per occurrence, as it does for every walk over a
    // key. Stops counting at `limit`.
    private def sizeAtMost(t: Type, limit: Int): Boolean = {
      val todo = mutable.ArrayBuffer(t)
      var count = 0
      while (todo.nonEmpty && (count <= limit)) {
        count += 1
        todo.remove(todo.length - 1) match {
          case Type.TyApply(on, arg) => (todo += on += arg): Unit
          case Type.ForAll(_, in)    => (todo += in): Unit
          case Type.Exists(_, in)    => (todo += in): Unit
          case _                     => ()
        }
      }
      count <= limit
    }

    def newSolver(): Solver = new Solver

    /** Solves, on demand, the data-type equations that one check reaches.
      *
      * `stateOf(key)` finds the keys reachable from `key`, noting which reads
      * are contravariant (under an odd number of function arguments).
      * Tarjan's algorithm splits the keys into strongly connected components.
      * It is iterative, so a long chain of keys can't overflow the stack.
      * Components come out dependencies first, so each one is solved after
      * every key it reads from outside itself.
      *
      * Uninhabited < Unknown < Inhabited is the interval order on (surely
      * inhabited, possibly inhabited). Every rule is monotone in it, except
      * `Fun`, which is antitone in its arguments. So a component that reads
      * its own keys only covariantly has equations monotone in its own keys.
      * Kleene iteration from Uninhabited (no values yet) then reaches the
      * least fixed point, and each key changes at most twice.
      *
      * That least fixed point, with the keys outside the component fixed,
      * is what the data types mean. Each key stands for every type that
      * agrees with it on variable states (see `dataKey`), and data types are
      * least fixed points of their unfoldings, which is what `expand`
      * computes. For a monotone system, solving all its equations at once
      * gives the same least fixed point as solving them one at a time
      * (Bekic's lemma).
      *
      * A component that reads one of its own keys contravariantly can't be
      * iterated from the bottom: that can settle on a fixed point that is not
      * the least one. Bosatsu accepts such components. With
      * `enum E[x, y]: C1(v: y), C2(f: x -> Never)` and
      * `struct D(w: E[E[D, Y], D])`, D occurs covariantly in its own
      * definition (under two contravariant parameters). Yet D,
      * `E[E[D, Y], D]` and `E[D, Y]` read each other contravariantly.
      *
      * When every cycle in such a component passes an even number of
      * function arguments (`parityConsistent`), each key occurs covariantly
      * in its own unfolding. That is the condition (`Model.cov`) under which
      * `lean_proofs/LeanProofs/Inhabitedness.lean` proves the older
      * depth-first search sound, so the component is solved by that search
      * (`searchComponent`).
      *
      * `KindFormula` requires every occurrence of a type in its own
      * definition to be covariant (`Constraint.RecursiveView`), so every
      * cycle of a checked environment should be even. An odd cycle, as in
      * `struct Liar(f: Liar -> Never)`, which `KindFormula` rejects, means the
      * environment skipped that check (a hand-built `TypeEnv`). The solver
      * answers Unknown for it instead of relying on `KindFormula`.
      *
      * Keys reached after `MaxDataTypeKeys` are Unknown.
      *
      * Keys are looked up by `Type`'s structural equality and hashing.
      * `Order[Type]` gives the same equality but compares large keys far
      * more slowly.
      */
    final class Solver extends DataStates {
      private val solved = mutable.HashMap.empty[Type, State]
      private var keysLeft = MaxDataTypeKeys

      def stateOf(key: Type): State =
        solved.get(key) match {
          case Some(s) => s
          case None =>
            solveFrom(key)
            solved.getOrElse(key, Unknown)
        }

      // Solved answers are final, so where a key is read doesn't matter.
      def contravariant: DataStates = this

      // One key being solved by `solveFrom`. `succ` are the keys `expand` reads,
      // `co` the ones it reads covariantly and `contra` the ones it reads
      // contravariantly (a key can be in both). `discovered` is what `expand`
      // answered while finding them, which is exact when `succ` is empty.
      private final class Node(
          val key: Type,
          val index: Int,
          val stackPos: Int,
          val succ: List[Type],
          val co: Set[Type],
          val contra: Set[Type],
          val discovered: State
      ) {
        var lowLink: Int = index
        var onStack: Boolean = true
        var inComponent: Boolean = false
        // While its component is solved, the current approximation; after,
        // the answer.
        var state: State = Uninhabited
        var readers: List[Node] = Nil
        var queued: Boolean = false
        // 0 or 1 once `parityConsistent` has labelled it, else -1.
        var parity: Int = -1
      }

      private def solveFrom(root: Type): Unit = {
        // One table holds everything about a key, so each step looks a key
        // up once: a lookup that hits compares the keys structurally.
        val nodes = mutable.HashMap.empty[Type, Node]
        val stack = mutable.ArrayBuffer.empty[Node]
        // The nodes being visited, innermost last, each with the successors it
        // has not visited yet.
        val frames = mutable.ArrayBuffer.empty[(Node, List[Type])]

        def enter(key: Type): Unit =
          if (keysLeft <= 0) solved(key) = Unknown
          else {
            keysLeft -= 1
            val found = mutable.HashSet.empty[Type]
            val succ = List.newBuilder[Type]
            val co = mutable.HashSet.empty[Type]
            val contra = mutable.HashSet.empty[Type]
            final class Reads(covariant: Boolean) extends DataStates {
              def stateOf(k: Type): State = {
                if (found.add(k)) (succ += k): Unit
                if (covariant) (co += k): Unit
                else (contra += k): Unit
                Uninhabited
              }
              lazy val contravariant: DataStates = new Reads(!covariant)
            }
            // `expand` reads the same keys in the same positions whatever it
            // is told, so these are the keys it depends on.
            val discovered = expand(key, new Reads(covariant = true))
            val node = new Node(
              key,
              nodes.size,
              stack.length,
              succ.result(),
              co.toSet,
              contra.toSet,
              discovered
            )
            nodes(key) = node
            stack += node
            frames += ((node, node.succ))
          }

        enter(root)
        while (frames.nonEmpty) {
          val (node, rest) = frames.last
          rest match {
            case next :: tail =>
              frames(frames.length - 1) = (node, tail)
              nodes.get(next) match {
                case Some(n) =>
                  // A visited key that is off the stack is in a finished
                  // component.
                  if (n.onStack) node.lowLink = node.lowLink.min(n.index)
                case None =>
                  if (!solved.contains(next)) enter(next)
              }
            case Nil =>
              frames.remove(frames.length - 1): Unit
              frames.lastOption.foreach { case (parent, _) =>
                parent.lowLink = parent.lowLink.min(node.lowLink)
              }
              if (node.lowLink == node.index) {
                // `node` is the first node of its component that was visited,
                // and the component is `node` and everything above it.
                val component = NonEmptyList(
                  node,
                  stack.iterator.drop(node.stackPos + 1).toList
                )
                stack.dropRightInPlace(stack.length - node.stackPos): Unit
                component.toList.foreach(_.onStack = false)
                solveComponent(component, nodes)
              }
          }
        }
      }

      private def solveComponent(
          component: NonEmptyList[Node],
          nodes: mutable.HashMap[Type, Node]
      ): Unit = {
        component.toList.foreach(_.inComponent = true)
        def member(k: Type): Option[Node] =
          nodes.get(k).filter(_.inComponent)

        if (component.tail.isEmpty && component.head.succ.isEmpty)
          component.head.state = component.head.discovered
        else if (component.exists(_.contra.exists(member(_).nonEmpty))) {
          if (parityConsistent(component, member))
            searchComponent(component, member, nodes)
          else component.toList.foreach(_.state = Unknown)
        } else {
          component.toList.foreach { n =>
            n.succ.foreach { k =>
              member(k).foreach(m => m.readers = n :: m.readers)
            }
          }
          // Nodes outside the component are finished, so their state is
          // their answer. Tarjan's order finished every key the component
          // reads from outside it.
          val read: DataStates = new DataStates {
            def stateOf(k: Type): State =
              nodes.get(k) match {
                case Some(n) => n.state
                case None    => solved.getOrElse(k, Unknown)
              }
            def contravariant: DataStates = this
          }

          val queue = mutable.ArrayDeque.from(component.toList)
          component.toList.foreach(_.queued = true)
          while (queue.nonEmpty) {
            val n = queue.removeHead()
            n.queued = false
            val next = expand(n.key, read)
            // Values only rise, because the component is monotone; taking
            // rises alone also bounds the loop by construction.
            val rises =
              (next != n.state) &&
                ((n.state == Uninhabited) || (next == Inhabited))
            if (rises) {
              n.state = next
              n.readers.foreach { r =>
                // The equations only rise, so an Inhabited key is done.
                if (!r.queued && (r.state != Inhabited)) {
                  r.queued = true
                  (queue += r): Unit
                }
              }
            }
          }
        }

        component.toList.foreach { n =>
          n.inComponent = false
          solved(n.key) = n.state
        }
      }

      // Whether the component's keys can be labelled 0 or 1 so that a
      // covariant read keeps the label and a contravariant read flips it,
      // that is, whether every cycle in it passes an even number of function
      // arguments. Expected to hold whenever `KindFormula` checked the
      // environment (see `Solver`), but checked here rather than assumed.
      private def parityConsistent(
          component: NonEmptyList[Node],
          member: Type => Option[Node]
      ): Boolean = {
        // The component is strongly connected, so this reaches all of it.
        val start = component.head
        start.parity = 0
        val todo = mutable.ArrayBuffer(start)
        var consistent = true
        while (consistent && todo.nonEmpty) {
          val n = todo.remove(todo.length - 1)
          def label(k: Type, flip: Int): Unit =
            member(k).foreach { m =>
              val want = n.parity ^ flip
              if (m.parity < 0) {
                m.parity = want
                (todo += m): Unit
              } else if (m.parity != want) consistent = false
            }
          n.co.foreach(label(_, 0))
          n.contra.foreach(label(_, 1))
        }
        consistent
      }

      // The search that `lean_proofs/LeanProofs/Inhabitedness.lean` models as
      // `check` and proves sound: expand a key with the keys on the current
      // path assumed empty, so a key met again on the path is Uninhabited.
      // Each key of the component is its own root. Keys outside the
      // component are finished and don't depend on it, so their answers are
      // the same under any assumption about its keys.
      private def searchComponent(
          component: NonEmptyList[Node],
          member: Type => Option[Node],
          nodes: mutable.HashMap[Type, Node]
      ): Unit = {
        var visitsLeft = MaxSearchVisits

        def search(key: Type, path: Set[Type]): State =
          if (path(key)) Uninhabited
          else if ((visitsLeft <= 0) || (path.size >= MaxSearchDepth)) Unknown
          else {
            visitsLeft -= 1
            val path1 = path + key
            val read: DataStates = new DataStates {
              def stateOf(k: Type): State =
                member(k) match {
                  case Some(_) => search(k, path1)
                  case None =>
                    nodes.get(k) match {
                      case Some(n) => n.state
                      case None    => solved.getOrElse(k, Unknown)
                    }
                }
              def contravariant: DataStates = this
            }
            expand(key, read)
          }

        component.toList.foreach(n => n.state = search(n.key, Set.empty))
      }
    }

    def checkType(
        t: Type,
        varState: Map[Type.Var.Bound, State],
        dataState: DataStates
    ): State = {
      val norm = t.normalize

      norm match {
        case Type.ForAll(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkType(in, vs, dataState),
            isForAll = true,
            varState
          )
        case Type.Exists(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkType(in, vs, dataState),
            isForAll = false,
            varState
          )
        case Type.Fun(args, res) =>
          val resultState = checkType(res, varState, dataState)
          val argStates =
            args.map(checkType(_, varState, dataState.contravariant))
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
          Type.unapplyAll(norm)._1 match {
            case tc @ Type.TyConst(_) =>
              if (alwaysInhabited(tc.tpe.toDefined)) Inhabited
              else if (env.getType(tc).isEmpty) Unknown
              else if (!sizeAtMost(norm, MaxDataTypeKeySize)) Unknown
              else dataState.stateOf(dataKey(norm, varState))
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
        dataState: DataStates
    ): State = {
      val scrutineeNorm = scrutinee.normalize

      scrutineeNorm match {
        case Type.ForAll(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkPattern(in, pattern, vs, dataState),
            isForAll = true,
            varState
          )
        case Type.Exists(vars, in) =>
          evaluateQuantifier(
            vars.toList,
            vs => checkPattern(in, pattern, vs, dataState),
            isForAll = false,
            varState
          )
        case _ =>
          pattern match {
            case Pattern.WildCard | Pattern.Var(_) =>
              checkType(scrutineeNorm, varState, dataState)
            case Pattern.Named(_, pat) =>
              checkPattern(scrutineeNorm, pat, varState, dataState)
            case Pattern.Annotation(pat, tpe) =>
              if (definitelyDisjoint(scrutineeNorm, tpe)) Uninhabited
              else {
                val scrState = checkType(scrutineeNorm, varState, dataState)
                lazy val annState = checkType(tpe, varState, dataState)
                lazy val patState = checkPattern(tpe, pat, varState, dataState)

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
                (head :: rest.toList).map(checkPattern(scrutineeNorm, _, varState, dataState))
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
                  checkPattern(scrutineeNorm, asStruct, varState, dataState)
                case Left((_, suffix)) =>
                  // Prefix-glob list patterns (for example: `*_, p1, p2`) do not
                  // lower to a simple cons spine. We still know they require a list
                  // scrutinee and each explicit item in the suffix to be matchable
                  // against the list element type. We use that to prove definite
                  // `Uninhabited` cases; otherwise we stay conservative with `Unknown`.
                  checkType(scrutineeNorm, varState, dataState) match {
                    case Uninhabited => Uninhabited
                    case scrState =>
                      listElementType(scrutineeNorm) match {
                        case None => Unknown
                        case Some(itemType) =>
                          val itemStates =
                            suffix.toList.collect { case Pattern.ListPart.Item(itemPat) =>
                              checkPattern(itemType, itemPat, varState, dataState)
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
                                  checkPattern(fieldType, pat, vs, dataState)
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
