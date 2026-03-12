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

/-- Small sanity example. -/
def plusOne (n : Nat) : Eval Nat :=
  .pure (n + 1)

def sample : Eval Nat :=
  .flatMap (.pure 41) plusOne

example : runFuel 2 sample = some 42 := by
  rfl

example : ∃ n v, runFuel n sample = some v := by
  exact ⟨2, 42, rfl⟩

end EvalFuelExistence
