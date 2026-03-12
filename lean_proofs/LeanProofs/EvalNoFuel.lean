namespace EvalNoFuel

universe u

/-
A bounded continuation.

`BFn α β wOut wFn` means:
- running the continuation on any input produces an `Eval β wOut`
- `wOut < wFn`

The strict inequality is what lets the `RunStack -> RunEval` transition decrease rank.
-/
mutual
  inductive Eval : Type u → Nat → Type (u + 1) where
    | pure : α → Eval α 0
    | flatMap : Eval β wPrev → BFn β α wOut wFn → Eval α (wPrev + wFn + 1)

  inductive BFn : Type u → Type u → Nat → Nat → Type (u + 1) where
    | mk :
        (run : α → Eval β wOut) →
        (lt_bound : wOut < wFn) →
        BFn α β wOut wFn
end

inductive Stack : Type u → Type u → Nat → Type (u + 1) where
  | done : (α → β) → Stack α β 0
  | more : BFn α β wOut wFn → Stack β γ wTail → Stack α γ (wFn + wTail)

inductive Loop : Type u → Type u → Type (u + 1) where
  | runEval : Eval α w → Stack α β c → Loop α β
  | runStack : α → Stack α β c → Loop α β

@[simp] def push (fn : BFn α β wOut wFn) (tail : Stack β γ wTail) : Stack α γ (wFn + wTail) :=
  Stack.more fn tail

@[simp] def weight : Loop α β → Nat
  | .runEval (w := w) (_ : Eval α w) (c := c) (_ : Stack α β c) => w + c
  | .runStack (c := c) _ (_ : Stack α β c) => c

@[simp] def phaseOf : Loop α β → Nat
  | .runEval _ _ => 1
  | .runStack _ _ => 0

@[simp] def rank (st : Loop α β) : Nat :=
  2 * weight st + phaseOf st

theorem odd_lt_even_of_lt (a b : Nat) (h : a < b) : 2 * a + 1 < 2 * b := by
  omega

/--
Fuel-free evaluator.

Lean accepts this recursive definition because `rank` strictly decreases at each
recursive step (proved in `decreasing_by`).
-/
def runLoop : Loop α β → β
  | .runStack a stack =>
      match stack with
      | .done k => k a
      | .more (.mk run _ltBound) tail =>
          runLoop (.runEval (run a) tail)
  | .runEval e stack =>
      match e with
      | .pure a => runLoop (.runStack a stack)
      | .flatMap prev fn => runLoop (.runEval prev (push fn stack))
termination_by st => rank st
decreasing_by
  all_goals simp [rank, weight, phaseOf, Nat.add_assoc]
  all_goals try omega
  exact odd_lt_even_of_lt _ _ (Nat.add_lt_add_right _ltBound _)

@[simp] def pure (a : α) : Eval α 0 :=
  Eval.pure a

@[simp] def flatMap (e : Eval α wA) (fn : BFn α β wOut wFn) : Eval β (wA + wFn + 1) :=
  Eval.flatMap e fn

@[simp] def done : Stack α α 0 :=
  Stack.done id

@[simp] def runEval (e : Eval α w) : α :=
  runLoop (.runEval e done)

/-- A continuation that maps `n` to `n + 1`. -/
def plusOneFn : BFn Nat Nat 0 1 :=
  .mk (fun n => pure (n + 1)) (by omega)

/-- `pure 41 >>= plusOneFn` evaluates to `42`. -/
def sampleEval : Eval Nat 2 :=
  flatMap (pure 41) plusOneFn

example : runEval sampleEval = 42 := by
  native_decide

end EvalNoFuel
