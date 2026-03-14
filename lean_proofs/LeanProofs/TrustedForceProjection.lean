namespace TrustedForceProjection

/-!
This file models the trusted projection rules currently used by the recursion
checker:

- structural field projection
- thunk force
- lazy force

The important point is that trusted delayed values are interpreted with a hidden
rank witness. A thunk/lazy wrapper of rank `n + 1` can only force to a value of
rank `n`. Under that interpretation, adding trusted force edges preserves
well-foundedness.
-/

mutual
  inductive Value : Nat → Type where
    | atom : Value 0
    | pair : Value a → Value b → Value (Nat.max a b + 1)
    | thunk : Thunk n → Value (n + 1)
    | lazyVal : Thunk n → Value (n + 1)

  inductive Thunk : Nat → Type where
    | mk : (Unit → Value n) → Thunk n
end

@[simp] def force : Thunk n → Value n
  | .mk fn => fn ()

abbrev AnyValue := Sigma Value

@[simp] def height : AnyValue → Nat
  | ⟨n, _⟩ => n

inductive Smaller : AnyValue → AnyValue → Prop where
  | pairLeft (left : Value a) (right : Value b) :
      Smaller ⟨a, left⟩ ⟨Nat.max a b + 1, Value.pair left right⟩
  | pairRight (left : Value a) (right : Value b) :
      Smaller ⟨b, right⟩ ⟨Nat.max a b + 1, Value.pair left right⟩
  | thunkForce (th : Thunk n) :
      Smaller ⟨n, force th⟩ ⟨n + 1, Value.thunk th⟩
  | lazyForce (th : Thunk n) :
      Smaller ⟨n, force th⟩ ⟨n + 1, Value.lazyVal th⟩

@[simp] theorem smaller_height_lt {x y : AnyValue} (h : Smaller x y) :
    height x < height y := by
  cases h with
  | pairLeft left right =>
      simp [height]
      exact Nat.lt_succ_of_le (Nat.le_max_left _ _)
  | pairRight left right =>
      simp [height]
      exact Nat.lt_succ_of_le (Nat.le_max_right _ _)
  | thunkForce th =>
      simp [height]
  | lazyForce th =>
      simp [height]

/--
The trusted step relation is well-founded: every structural child and every
trusted thunk/lazy force step strictly decreases the hidden height index.
-/
theorem smallerSubMeasure : Subrelation Smaller (measure height).rel := by
  intro a b h
  exact smaller_height_lt h

theorem wellFoundedSmaller : WellFounded Smaller :=
  Subrelation.wf
    smallerSubMeasure
    (measure height).wf

/--
The same remains true after composing several trusted steps, which is the shape
used by checker reasoning through lets and nested matches.
-/
theorem wellFoundedSmallerTransGen :
    WellFounded (Relation.TransGen Smaller) :=
  wellFoundedSmaller.transGen

end TrustedForceProjection
