import LeanProofs.TrustedForceProjection
import LeanProofs.EvalFuelExistence

namespace TrustedEvalProjection

open EvalFuelExistence.BosatsuLoop

abbrev BaseValue := TrustedForceProjection.AnyValue

/--
Package an `Eval` program over trusted-force values together with its chosen
total result.
-/
structure EvalBox where
  prog : Eval BaseValue
  result : BaseValue
  sound : runEval prog = result

noncomputable def box (prog : Eval BaseValue) : EvalBox :=
  ⟨prog, runEval prog, rfl⟩

inductive Node : Type 2 where
  | plain : BaseValue → Node
  | evalNode : EvalBox → Node

@[simp] def height : Node → Nat
  | .plain v => TrustedForceProjection.height v
  | .evalNode b => TrustedForceProjection.height b.result + 1

inductive Smaller : Node → Node → Prop where
  | base {x y : BaseValue} :
      TrustedForceProjection.Smaller x y →
      Smaller (.plain x) (.plain y)
  | evalForce (b : EvalBox) :
      Smaller (.plain b.result) (.evalNode b)

@[simp] theorem smaller_height_lt {x y : Node} (h : Smaller x y) :
    height x < height y := by
  cases h with
  | base hBase =>
      simpa [height] using TrustedForceProjection.smaller_height_lt hBase
  | evalForce b =>
      simp [height]

theorem smallerSubMeasure : Subrelation Smaller (measure height).rel := by
  intro a b h
  exact smaller_height_lt h

/--
Structural descent plus trusted thunk/lazy/eval forcing remains well-founded.
-/
theorem wellFoundedSmaller : WellFounded Smaller :=
  Subrelation.wf
    smallerSubMeasure
    (measure height).wf

theorem wellFoundedSmallerTransGen :
    WellFounded (Relation.TransGen Smaller) :=
  wellFoundedSmaller.transGen

/--
The trusted `Eval::eval` rule: evaluating `prog` yields a smaller node than the
delayed `evalNode` wrapper containing `prog`.
-/
theorem evalRuleSound (prog : Eval BaseValue) :
    Smaller (.plain (runEval prog)) (.evalNode (box prog)) := by
  simpa [box] using Smaller.evalForce (box prog)

/--
Model of the callback binder in `flat_map(prog, fn)`: the bound value is
exactly the result of evaluating `prog`. The continuation itself does not affect
this projection fact.
-/
noncomputable def flatMapBinder
    (prog : Eval BaseValue)
    (_fn : BaseValue → Eval BaseValue) : BaseValue :=
  runEval prog

@[simp] theorem flatMapBinder_eq_eval
    (prog : Eval BaseValue)
    (_fn : BaseValue → Eval BaseValue) :
    flatMapBinder prog _fn = runEval prog :=
  rfl

theorem flatMapBinderRuleSound
    (prog : Eval BaseValue)
    (_fn : BaseValue → Eval BaseValue) :
    Smaller (.plain (flatMapBinder prog _fn)) (.evalNode (box prog)) := by
  simpa [flatMapBinder] using evalRuleSound prog

end TrustedEvalProjection
