namespace EvalFuelExistence

universe u

/--
Naive `Eval` encoding: no static size indices, no bounded continuation witness.
-/
inductive Eval : Type u → Type (u + 1) where
  | pure : α → Eval α
  | flatMap : Eval α → (α → Eval β) → Eval β

/--
Fuel-based evaluator for the naive `Eval`.
-/
@[simp] def runFuel : Nat → Eval α → Option α
  | 0, _ => none
  | _ + 1, .pure a => some a
  | n + 1, .flatMap prev fn =>
      match runFuel n prev with
      | none => none
      | some a => runFuel n (fn a)

/--
Big-step semantics for `Eval`.
-/
inductive Evaluates : Eval α → α → Prop where
  | pure : Evaluates (.pure a) a
  | flatMap : Evaluates prev a → Evaluates (fn a) b → Evaluates (.flatMap prev fn) b

def Terminates (e : Eval α) : Prop :=
  ∃ v, Evaluates e v

/--
All values of this `Eval` shape terminate semantically.

This is the key `hTotal` theorem: it is derivable by structural induction on
`Eval`, because the `flatMap` induction case provides an IH for all `fn` outputs.
-/
theorem terminates_all : ∀ {α : Type u}, (e : Eval α) → Terminates e := by
  intro α e
  induction e with
  | pure a =>
      exact ⟨a, Evaluates.pure⟩
  | flatMap prev fn ihPrev ihFn =>
      rcases ihPrev with ⟨a, hPrev⟩
      rcases ihFn a with ⟨b, hFn⟩
      exact ⟨b, Evaluates.flatMap hPrev hFn⟩

/--
If `runFuel n e = some v`, then one extra unit of fuel still yields `v`.
-/
theorem runFuel_succ_mono {α : Type u} :
    ∀ n (e : Eval α) (v : α), runFuel n e = some v → runFuel (n + 1) e = some v
  | 0, e, v, h => by
      simp [runFuel] at h
  | n + 1, .pure a, v, h => by
      simpa [runFuel] using h
  | n + 1, .flatMap prev fn, v, h => by
      simp [runFuel] at h ⊢
      cases hPrev : runFuel n prev with
      | none =>
          simp [hPrev] at h
      | some a =>
          have hPrev' : runFuel (n + 1) prev = some a :=
            runFuel_succ_mono n prev a hPrev
          have hBody : runFuel n (fn a) = some v := by
            simpa [hPrev] using h
          have hBody' : runFuel (n + 1) (fn a) = some v :=
            runFuel_succ_mono n (fn a) v hBody
          simp [hPrev', hBody']

/--
Fuel success is monotone in the fuel amount.
-/
theorem runFuel_mono {α : Type u} {n m : Nat} {e : Eval α} {v : α}
    (hLe : n ≤ m) (h : runFuel n e = some v) :
    runFuel m e = some v := by
  induction hLe with
  | refl =>
      simpa using h
  | @step m hLe ih =>
      exact runFuel_succ_mono m e v ih

/--
Completeness of `runFuel` w.r.t. big-step semantics:
if `e ⇓ v` then some finite fuel computes `v`.
-/
theorem runFuel_complete {α : Type u} {e : Eval α} {v : α} (hEval : Evaluates e v) :
    ∃ n, runFuel n e = some v := by
  exact
    match hEval with
    | .pure =>
        ⟨1, by simp [runFuel]⟩
    | @Evaluates.flatMap _ _ prev a fn b hPrev hNext =>
        let ⟨nPrev, hPrevFuel⟩ := runFuel_complete hPrev
        let ⟨nNext, hNextFuel⟩ := runFuel_complete hNext
        let n := Nat.max nPrev nNext
        have hPrevAtN : runFuel n prev = some a :=
          runFuel_mono (Nat.le_max_left nPrev nNext) hPrevFuel
        have hNextAtN : runFuel n (fn a) = some b :=
          runFuel_mono (Nat.le_max_right nPrev nNext) hNextFuel
        ⟨n + 1, by
          simpa [runFuel, hPrevAtN] using hNextAtN
        ⟩

/--
Key existential theorem:
if an `Eval` terminates semantically, there exists some finite fuel amount
that makes `runFuel` return `some`.
-/
theorem exists_fuel_of_terminates {α : Type u} {e : Eval α} (hTerm : Terminates e) :
    ∃ n v, runFuel n e = some v := by
  rcases hTerm with ⟨v, hEval⟩
  rcases runFuel_complete hEval with ⟨n, hFuel⟩
  exact ⟨n, v, hFuel⟩

/--
Bridge shape for Bosatsu use:
if you can show every Bosatsu-generated `Eval` terminates semantically,
you get finite-fuel existence for all such values.
-/
theorem exists_fuel_if_total
    (hTotal : ∀ {α : Type u}, (e : Eval α) → Terminates e) :
    ∀ {α : Type u}, (e : Eval α) → ∃ n v, runFuel n e = some v := by
  intro α e
  exact exists_fuel_of_terminates (hTotal e)

/--
Direct corollary for this `Eval` definition:
for every `e`, there exists some finite fuel that succeeds.
-/
theorem exists_fuel_for_all :
    ∀ {α : Type u}, (e : Eval α) → ∃ n v, runFuel n e = some v := by
  intro α e
  exact exists_fuel_of_terminates (terminates_all e)

/-- Small sanity example. -/
def plusOne (n : Nat) : Eval Nat :=
  .pure (n + 1)

def sample : Eval Nat :=
  .flatMap (.pure 41) plusOne

example : runFuel 2 sample = some 42 := by
  rfl

example : ∃ n v, runFuel n sample = some v := by
  exact ⟨2, 42, rfl⟩

namespace BosatsuLoop

/-!
Bosatsu-style representation (close to `test_workspace/Bosatsu/Eval.bosatsu`):
- `Leaf`
- `Eval`
- `Stack`
- `Loop` with `RunEval` / `RunStack`

We connect this representation to the naive `Eval` above and reuse
`exists_fuel_for_all` to define a fuel-free `eval_loop`.
-/

inductive Leaf : Type u → Type (u + 1) where
  | done : α → Leaf α
  | lazyLeaf : (Unit → α) → Leaf α
  | always : (Unit → α) → Leaf α

@[simp] def evalLeaf : Leaf α → α
  | .done a => a
  | .lazyLeaf fn => fn ()
  | .always fn => fn ()

inductive Eval : Type u → Type (u + 1) where
  | pure : Leaf α → Eval α
  | flatMap : Eval β → (β → Eval α) → Eval α

@[simp] def flatMapEval : Eval α → (α → Eval β) → Eval β
  | eval, fn => .flatMap eval fn

inductive Stack : Type u → Type u → Type (u + 1) where
  | last : (α → Eval β) → Stack α β
  | more : (α → Eval γ) → Stack γ β → Stack α β

inductive Loop : Type u → Type (u + 1) where
  | runStack : α → Stack α β → Loop β
  | runEval : Eval α → Stack α β → Loop β

/-- Erase Bosatsu-style Eval into the naive Eval used above. -/
@[simp] def toNaiveEval : Eval α → EvalFuelExistence.Eval α
  | .pure leaf => .pure (evalLeaf leaf)
  | .flatMap prev fn =>
      EvalFuelExistence.Eval.flatMap
        (toNaiveEval prev)
        (fun a => toNaiveEval (fn a))

@[simp] def toNaiveStack : Stack α β → α → EvalFuelExistence.Eval β
  | .last fn => fun a => toNaiveEval (fn a)
  | .more first rest => fun a =>
      EvalFuelExistence.Eval.flatMap
        (toNaiveEval (first a))
        (toNaiveStack rest)

@[simp] def toNaiveLoop : Loop β → EvalFuelExistence.Eval β
  | .runEval e stack =>
      EvalFuelExistence.Eval.flatMap
        (toNaiveEval e)
        (toNaiveStack stack)
  | .runStack a stack =>
      toNaiveStack stack a

theorem exists_fuel_for_loop :
    ∀ {β : Type u}, (st : Loop β) → ∃ n v, EvalFuelExistence.runFuel n (toNaiveLoop st) = some v := by
  intro β st
  exact EvalFuelExistence.exists_fuel_for_all (toNaiveLoop st)

theorem exists_value_for_loop {β : Type u} (st : Loop β) :
    ∃ v, ∃ n, EvalFuelExistence.runFuel n (toNaiveLoop st) = some v := by
  rcases exists_fuel_for_loop st with ⟨n, v, h⟩
  exact ⟨v, n, h⟩

/--
Fuel-free loop runner for the Bosatsu-style machine.

This is noncomputable because it chooses a witness from
`exists_fuel_for_loop`, but it matches the desired no-fuel API shape:
`eval_loop : Loop α -> α`.
-/
noncomputable def eval_loop (st : Loop β) : β :=
  Classical.choose (exists_value_for_loop st)

theorem eval_loop_sound (st : Loop β) :
    ∃ n, EvalFuelExistence.runFuel n (toNaiveLoop st) = some (eval_loop st) := by
  exact Classical.choose_spec (exists_value_for_loop st)

@[simp] def doneStack : Stack α α :=
  .last (fun a => .pure (.done a))

noncomputable def runEval (e : Eval α) : α :=
  eval_loop (.runEval e doneStack)

def plusOne (n : Nat) : Eval Nat :=
  .pure (.done (n + 1))

def sampleLoop : Loop Nat :=
  .runEval (.flatMap (.pure (.done 41)) plusOne) doneStack

example : ∃ n, EvalFuelExistence.runFuel n (toNaiveLoop sampleLoop) = some 42 := by
  exact ⟨3, rfl⟩

end BosatsuLoop

end EvalFuelExistence
