namespace Inhabitedness

/-!
# A model of `Inhabitedness.Analyzer.checkType`

This file models `core/src/main/scala/dev/bosatsu/Inhabitedness.scala`
(`Analyzer.checkType`) and proves that an `Uninhabited` answer is sound:
the type has no value.

`TotalityCheck` lets a match leave out the branches the checker calls
Uninhabited, so a wrong Uninhabited answer would admit a match that fails at
runtime. Inhabited and Unknown answers never let a match omit a branch.

Only `checkType` is modelled. `TotalityCheck` reaches it through
`checkPattern`, whose own Uninhabited rules (`definitelyDisjoint` annotations,
literal and string patterns against other types, a constructor of a different
type, prefix-glob list patterns) are not covered here. A wildcard or variable
pattern is exactly `checkType`, and a constructor pattern whose sub-patterns
are wildcards is the constructor's existential quantifier over the `andStates`
of `checkType` on its fields.

## Syntax

Types are locally nameless:

* `bvar i` is bound by an enclosing quantifier or constructor binder
  (a de Bruijn index);
* `fvar l` is a quantified variable that the checker has already opened. It is
  named by its *level*, the position of its state in the checker's `varState`.

The Scala code names variables. Levels are the capture-free version of those
names: every opened variable gets a fresh level, so the `seen` set and
`varState` never confuse two different variables. See "Assumptions" at the end
of the file for what this means for the Scala code.

Everything the checker answers Unknown for without looking inside (skolems,
metas, applications whose head is not a constant, quantifiers over a
higher-kinded variable) is an `opaque` leaf. Built-in types that the checker
always answers Inhabited for (Int, String, Char, Float64, Bool, Unit) are
`prim` leaves. Multi-argument function types are curried; the checker's
answer for `(a1, ..., an) -> r` is the same as for `a1 -> ... -> an -> r`.
-/

inductive Ty where
  | bvar : Nat → Ty
  | fvar : Nat → Ty
  | prim : Nat → Ty
  | opaque : Nat → Ty
  | arr : Ty → Ty → Ty
  | all : Nat → Ty → Ty
  | ex : Nat → Ty → Ty
  | con : Nat → Ty
  | app : Ty → Ty → Ty
  deriving DecidableEq, Repr

/-- `Type.unapplyAll` for a constant head: `D[a1, ..., an]` is
`app (... (app (con D) a1) ...) an`. -/
def spine : Ty → List Ty → Option (Nat × List Ty)
  | .con d, acc => some (d, acc)
  | .app f a, acc => spine f (a :: acc)
  | _, _ => none

def asData (t : Ty) : Option (Nat × List Ty) := spine t []

/-- `D[args]` -/
def mkData (d : Nat) (args : List Ty) : Ty := args.foldl .app (.con d)

/--
`inst k vs t` replaces the dangling indices `k, k+1, ...` of `t` by `vs`.
The `vs` are locally closed, so they need no shifting under binders.
-/
def inst (k : Nat) (vs : List Ty) : Ty → Ty
    | .bvar j =>
        if j < k then .bvar j
        else
          match vs[j - k]? with
          | some v => v
          | none => .bvar (j - vs.length)
    | .fvar l => .fvar l
    | .prim p => .prim p
    | .opaque o => .opaque o
    | .arr a b => .arr (inst k vs a) (inst k vs b)
    | .all n b => .all n (inst (k + n) vs b)
    | .ex n b => .ex n (inst (k + n) vs b)
    | .con d => .con d
    | .app f a => .app (inst k vs f) (inst k vs a)

/-- `n` fresh variables starting at level `base`. -/
def levels (base n : Nat) : List Ty :=
  (List.range n).map (fun i => .fvar (base + i))

/-- Open the `n` variables of a quantifier body at levels `lvl, ..., lvl+n-1`. -/
def openAt (lvl n : Nat) (t : Ty) : Ty :=
  inst 0 (levels lvl n) t

/-- A size that ignores type applications; opening a binder preserves it. -/
def size : Ty → Nat
  | .arr a b => size a + size b + 1
  | .all _ b => size b + 1
  | .ex _ b => size b + 1
  | _ => 1

theorem size_inst (k : Nat) (vs : List Ty) (h : ∀ v ∈ vs, size v = 1) :
    ∀ t, size (inst k vs t) = size t
  | .bvar j => by
      unfold inst
      split
      · rfl
      · split
        · rename_i v hv
          exact h v (List.mem_of_getElem? hv)
        · rfl
  | .fvar _ => by simp [inst, size]
  | .prim _ => by simp [inst, size]
  | .opaque _ => by simp [inst, size]
  | .arr a b => by
      simp [inst, size, size_inst k vs h a, size_inst k vs h b]
  | .all n b => by simp [inst, size, size_inst (k + n) vs h b]
  | .ex n b => by simp [inst, size, size_inst (k + n) vs h b]
  | .con _ => by simp [inst, size]
  | .app _ _ => by simp [inst, size]

theorem size_openAt (lvl n : Nat) (t : Ty) : size (openAt lvl n t) = size t := by
  apply size_inst
  intro v hv
  simp only [levels, List.mem_map] at hv
  obtain ⟨_, _, rfl⟩ := hv
  rfl

/-! ## Data definitions -/

/--
A constructor. Its field types are under a binder of
`nex` existential variables (indices `0 .. nex-1`) followed by the type
parameters (indices `nex .. nex+nparams-1`). A constructor with an existential
of a non-`*` kind, which `evaluateQuantifier` answers Unknown for without
looking at its fields, is modelled as a constructor with no existentials and
the single field `opaque`, which gives the same answer.
-/
structure Ctor where
  nex : Nat
  fields : List Ty
  deriving Repr

structure DataDef where
  nparams : Nat
  ctors : List Ctor
  deriving Repr

/-- A type environment: data type ids to definitions. `none` is a type the
checker cannot see into (it answers Unknown). -/
abbrev Env := Nat → Option DataDef

/-! ## The checker -/

inductive St where
  | inh
  | un
  | unk
  deriving DecidableEq, Repr

/-- `State.orStates` -/
def orS (l : List St) : St :=
  if l.contains .inh then .inh
  else if l.all (· == .un) then .un
  else .unk

/-- `State.andStates`, and also the `forall` case of `evaluateQuantifier`. -/
def andS (l : List St) : St :=
  if l.contains .un then .un
  else if l.all (· == .inh) then .inh
  else .unk

/-- The `Type.Fun` case of `checkType`, for one argument. -/
def funS (arg res : St) : St :=
  if res = .un ∧ arg = .inh then .un
  else if res = .inh ∨ arg = .un then .inh
  else .unk

/-- Every assignment of Inhabited/Uninhabited to `n` variables
(`varAssignments`). -/
def assigns : Nat → List (List St)
  | 0 => [[]]
  | n + 1 => (assigns n).flatMap (fun a => [.inh :: a, .un :: a])

/-- `MaxQuantifierVars` -/
def maxQuantifierVars : Nat := 10

/--
The data-type case of `checkType` for `D[args]`, given the recursive checker
`rec` (called with the extended `seen` set and `varState`). Each constructor is
an `exists` over its existential variables (`evaluateQuantifier` with
`isForAll = false`) of the `andStates` of its fields; the type is the
`orStates` of its constructors.
-/
def dataState (E : Env) (cap : Nat) (rec : List (Nat × List Ty) → List St → Ty → St)
    (seen : List (Nat × List Ty)) (σ : List St) (d : Nat) (args : List Ty) : St :=
  match E d with
  | none => .unk
  | some dt =>
      -- the cycle rule: a type met again while checking it is Uninhabited
      if (d, args) ∈ seen then .un
      else if dt.nparams ≠ args.length then .unk
      else if dt.ctors.isEmpty then .unk
      else orS (dt.ctors.map fun c =>
        if cap < c.nex then .unk
        else orS ((assigns c.nex).map fun as =>
          andS (c.fields.map fun f =>
            rec ((d, args) :: seen) (σ ++ as)
              (inst 0 (levels σ.length c.nex ++ args) f))))

/--
`check E cap fuel seen σ t` models `checkType(t, varState, seen)`:

* `σ` is `varState`, indexed by level;
* `seen` holds the data types currently being expanded, as `(id, args)`;
* `cap` is `MaxQuantifierVars`: a quantifier (or constructor) with more
  variables than `cap` is Unknown;
* `fuel` bounds the recursion so the model is a total function. Running out is
  Unknown, which is always a safe answer. The Scala code has no fuel; when it
  terminates, enough fuel gives the same answer.
-/
def check (E : Env) (cap : Nat) : Nat → List (Nat × List Ty) → List St → Ty → St
  | 0, _, _, _ => .unk
  | fuel + 1, seen, σ, t =>
    match t with
    | .bvar _ => .unk
    | .fvar l => σ.getD l .unk
    | .prim _ => .inh
    | .opaque _ => .unk
    | .arr a b => funS (check E cap fuel seen σ a) (check E cap fuel seen σ b)
    | .all n b =>
        if cap < n then .unk
        else andS ((assigns n).map fun as =>
          check E cap fuel seen (σ ++ as) (openAt σ.length n b))
    | .ex n b =>
        if cap < n then .unk
        else orS ((assigns n).map fun as =>
          check E cap fuel seen (σ ++ as) (openAt σ.length n b))
    | .con _ | .app _ _ =>
        match asData t with
        | none => .unk
        | some (d, args) =>
            dataState E cap (fun seen' σ' t' => check E cap fuel seen' σ' t') seen σ d args

/-- `Analyzer.checkType(t, Map.empty, Set.empty)` with the Scala cap. -/
def checkTop (E : Env) (fuel : Nat) (t : Ty) : St :=
  check E maxQuantifierVars fuel [] [] t

/-! ## Values

A type denotes a proposition: "the type has a value". This erases values to
their existence, which is all inhabitedness asks about.

* A function type `a -> b` has a value iff every value of `a` can be sent to a
  value of `b` (a total function).
* `forall` has a value iff the body has one at every instantiation. Every
  Bosatsu type is either inhabited or not, so instantiating at a type is
  instantiating at a proposition; Lean's `Prop` is impredicative, so the
  quantifier can range over all of them.
* `exists` has a value iff the body has one for some packed type.
* A data type `D[args]` has a value iff some constructor of `D` has values for
  all its fields, for some packed existentials. Values are finite constructor
  trees: the interpretation of data types is a *least* fixed point. That is
  stated by `Model` below.

Data types are interpreted at *syntactic* points `(D, args, ρ)`: the type
`D[args]` in the environment `ρ` that gives each level a proposition. The
checker's cycle rule compares types syntactically, and its assumptions ("this
type has no value") are about syntactic types, so the semantics keeps the
syntax around.
-/

abbrev Interp := Nat → List Ty → List Prop → Prop

/-- The meaning of a type application `D[args]` under the data interpretation `J`. -/
def semData (J : Interp) (ρ : List Prop) (t : Ty) : Prop :=
  match asData t with
  | some (d, args) => J d args ρ
  | none => False

/--
`sem Op J ρ t`: the type `t` has a value, where data types are interpreted
by `J`, opened variables by `ρ` (indexed by level), and opaque leaves by `Op`.
-/
def sem (Op : Nat → List Prop → Prop) (J : Interp) (ρ : List Prop) : Ty → Prop
  | .bvar _ => False
  | .fvar l => ρ.getD l False
  | .prim _ => True
  | .opaque o => Op o ρ
  | .arr a b => sem Op J ρ a → sem Op J ρ b
  | .all n b => ∀ xs : List Prop, xs.length = n → sem Op J (ρ ++ xs) (openAt ρ.length n b)
  | .ex n b => ∃ xs : List Prop, xs.length = n ∧ sem Op J (ρ ++ xs) (openAt ρ.length n b)
  | .con d => semData J ρ (.con d)
  | .app f a => semData J ρ (.app f a)
termination_by t => size t
decreasing_by
  all_goals simp only [size_openAt, size]
  all_goals omega

/--
One unfolding of a data type: some constructor has values for all its fields,
for some propositions `ws` packed as its existentials. A type the checker
cannot see into (unknown, external, or applied to the wrong number of
arguments) has an arbitrary meaning `Ext`.
-/
def Body (E : Env) (Ext : Interp) (Op : Nat → List Prop → Prop) (J : Interp)
    (d : Nat) (args : List Ty) (ρ : List Prop) : Prop :=
  match E d with
  | none => Ext d args ρ
  | some dt =>
      if dt.nparams = args.length ∧ dt.ctors ≠ [] then
        ∃ c ∈ dt.ctors, ∃ ws : List Prop, ws.length = c.nex ∧
          ∀ f ∈ c.fields, sem Op J (ρ ++ ws) (inst 0 (levels ρ.length c.nex ++ args) f)
      else Ext d args ρ

abbrev Point := Nat × List Ty × List Prop

/-! ### Scoping

`WF k j t`: `t` mentions only levels below `k`, and only indices below `j`
when it sits under `j` binders. The semantics of a data type only makes sense
for arguments that are well scoped in its environment: opening more variables
must not change what the arguments mean.
-/

def WF (k : Nat) : Nat → Ty → Prop
  | j, .bvar i => i < j
  | _, .fvar l => l < k
  | _, .prim _ => True
  | _, .opaque _ => True
  | j, .arr a b => WF k j a ∧ WF k j b
  | j, .all n b => WF k (j + n) b
  | j, .ex n b => WF k (j + n) b
  | _, .con _ => True
  | j, .app f a => WF k j f ∧ WF k j a

/-- The arguments of `D[args]` are well scoped in `ρ`. -/
def Scoped (ρ : List Prop) (args : List Ty) : Prop :=
  ∀ a ∈ args, WF ρ.length 0 a

/-- Field types mention only their constructor's existentials and the type
parameters, as in a Bosatsu `DefinedType`. -/
def EnvWF (E : Env) : Prop :=
  ∀ d dt, E d = some dt → ∀ c ∈ dt.ctors, ∀ f ∈ c.fields, WF 0 (c.nex + dt.nparams) f

/-- `D[args]` at `ρ` is assumed to have no value by `X` if `X` holds it at a
prefix of `ρ` (an assumption made before more variables were opened). -/
def Forced (X : List Point) (d : Nat) (args : List Ty) (ρ : List Prop) : Prop :=
  ∃ ρ0 xs, (d, args, ρ0) ∈ X ∧ ρ = ρ0 ++ xs

/--
A semantics for the data types of `E`: for each list `X` of data types assumed
to have no values, `W X` says which data types have values.

`W []` is the semantics proper. The other worlds exist because the cycle rule
reasons under assumptions: "if `D[args]` has no value, then neither do these
fields". Each hypothesis is only about well-scoped `D[args]`.

* `fix`: values of `D[args]` are constructor applications, except that the
  types in `X` have none.
* `discharge`: if `D[args]` cannot be built when it is itself assumed empty,
  then it is empty. This is induction on values, which holds because values
  are *finite* constructor trees (a least fixed point); it fails for
  non-terminating values. Bosatsu's recursion check, which rejects general
  recursion, is what makes values finite.
* `cov`: assuming fewer types empty never makes `D[args]` harder to build.
  This holds when `D` occurs only covariantly in its own definition, which
  `KindFormula` enforces (`Constraint.RecursiveView`).
-/
structure Model (E : Env) (Ext : Interp) (Op : Nat → List Prop → Prop) where
  W : List Point → Interp
  fix : ∀ X d args ρ, Scoped ρ args →
    (W X d args ρ ↔ (¬Forced X d args ρ ∧ Body E Ext Op (W X) d args ρ))
  discharge : ∀ X d args ρ, Scoped ρ args → ¬Forced X d args ρ →
    ¬Body E Ext Op (W ((d, args, ρ) :: X)) d args ρ → ¬W X d args ρ
  cov : ∀ X d args ρ, Scoped ρ args → ¬Forced X d args ρ →
    Body E Ext Op (W ((d, args, ρ) :: X)) d args ρ → Body E Ext Op (W X) d args ρ

/-- The closed type `t` has a value. -/
def Model.HasValue {E Ext Op} (M : Model E Ext Op) (t : Ty) : Prop :=
  sem Op (M.W []) [] t

/-! ### Scoping lemmas -/

theorem WF.mono {k k' : Nat} (hk : k ≤ k') :
    ∀ {j j' : Nat} (t : Ty), j ≤ j' → WF k j t → WF k' j' t
  | _, _, .bvar _, hj, h => by simp only [WF] at h ⊢; omega
  | _, _, .fvar _, _, h => by simp only [WF] at h ⊢; omega
  | _, _, .prim _, _, _ => trivial
  | _, _, .opaque _, _, _ => trivial
  | _, _, .arr a b, hj, h => ⟨WF.mono hk a hj h.1, WF.mono hk b hj h.2⟩
  | _, _, .all n b, hj, h => WF.mono hk b (by omega) h
  | _, _, .ex n b, hj, h => WF.mono hk b (by omega) h
  | _, _, .con _, _, _ => trivial
  | _, _, .app f a, hj, h => ⟨WF.mono hk f hj h.1, WF.mono hk a hj h.2⟩

theorem WF.inst {k k' : Nat} {vs : List Ty} (hk : k ≤ k') (hvs : ∀ v ∈ vs, WF k' 0 v) :
    ∀ (j : Nat) (t : Ty), WF k (j + vs.length) t → WF k' j (Inhabitedness.inst j vs t)
  | j, .bvar i, h => by
      simp only [WF] at h
      simp only [Inhabitedness.inst]
      split
      · simp only [WF]; assumption
      · rename_i hij
        have hlt : i - j < vs.length := by omega
        rw [List.getElem?_eq_getElem hlt]
        exact WF.mono (Nat.le_refl _) _ (Nat.zero_le _) (hvs _ (List.getElem_mem hlt))
  | _, .fvar _, h => by simp only [WF, Inhabitedness.inst] at h ⊢; omega
  | _, .prim _, _ => trivial
  | _, .opaque _, _ => trivial
  | j, .arr a b, h => ⟨WF.inst hk hvs j a h.1, WF.inst hk hvs j b h.2⟩
  | j, .all n b, h => by
      simp only [WF, Inhabitedness.inst] at h ⊢
      exact WF.inst hk hvs (j + n) b (by rwa [Nat.add_right_comm])
  | j, .ex n b, h => by
      simp only [WF, Inhabitedness.inst] at h ⊢
      exact WF.inst hk hvs (j + n) b (by rwa [Nat.add_right_comm])
  | _, .con _, _ => trivial
  | j, .app f a, h => ⟨WF.inst hk hvs j f h.1, WF.inst hk hvs j a h.2⟩

theorem WF.openAt {k n : Nat} {b : Ty} (h : WF k n b) : WF (k + n) 0 (openAt k n b) := by
  apply WF.inst (Nat.le_add_right k n)
  · intro v hv
    simp only [levels, List.mem_map, List.mem_range] at hv
    obtain ⟨i, hi, rfl⟩ := hv
    simp only [WF]
    omega
  · simpa [levels] using h

theorem WF.spine {k : Nat} :
    ∀ (t : Ty) (acc : List Ty) {d : Nat} {args : List Ty},
      spine t acc = some (d, args) → WF k 0 t → (∀ a ∈ acc, WF k 0 a) →
      ∀ a ∈ args, WF k 0 a
  | .con _, acc, _, _, h, _, hacc => by
      cases h
      exact hacc
  | .app f a, acc, _, _, h, ht, hacc => by
      refine WF.spine f (a :: acc) h ht.1 ?_
      intro x hx
      simp only [List.mem_cons] at hx
      rcases hx with rfl | hx
      · exact ht.2
      · exact hacc x hx
  | .bvar _, _, _, _, h, _, _ => by cases h
  | .fvar _, _, _, _, h, _, _ => by cases h
  | .prim _, _, _, _, h, _, _ => by cases h
  | .opaque _, _, _, _, h, _, _ => by cases h
  | .arr _ _, _, _, _, h, _, _ => by cases h
  | .all _ _, _, _, _, h, _, _ => by cases h
  | .ex _ _, _, _, _, h, _, _ => by cases h

theorem WF.asData {ρ : List Prop} {t : Ty} {d : Nat} {args : List Ty}
    (h : asData t = some (d, args)) (ht : WF ρ.length 0 t) : Scoped ρ args :=
  WF.spine t [] h ht (by simp)

theorem WF.field {E : Env} (hE : EnvWF E) {d : Nat} {dt : DataDef} (hd : E d = some dt)
    {c : Ctor} (hc : c ∈ dt.ctors) {f : Ty} (hf : f ∈ c.fields) {ρ : List Prop}
    {args : List Ty} (hlen : dt.nparams = args.length) (hargs : Scoped ρ args)
    (ws : List Prop) (hws : ws.length = c.nex) :
    WF (ρ ++ ws).length 0 (Inhabitedness.inst 0 (levels ρ.length c.nex ++ args) f) := by
  apply WF.inst (Nat.zero_le _)
  · intro v hv
    simp only [List.mem_append, levels, List.mem_map, List.mem_range] at hv
    rcases hv with ⟨i, hi, rfl⟩ | hv
    · simp only [WF, List.length_append]
      omega
    · exact WF.mono (by simp) _ (Nat.le_refl _) (hargs v hv)
  · have := hE d dt hd c hc f hf
    simpa [levels, hlen] using this

/-! ## Lemmas about the checker's pieces -/

theorem sem_data {Op J ρ t} (h : (∃ d, t = .con d) ∨ ∃ f a, t = .app f a) :
    sem Op J ρ t = semData J ρ t := by
  rcases h with ⟨d, rfl⟩ | ⟨f, a, rfl⟩ <;> simp [sem]

theorem orS_inh {l : List St} (h : orS l = .inh) : ∃ s ∈ l, s = .inh := by
  unfold orS at h
  split at h
  · rename_i hc
    exact ⟨.inh, by simpa using hc, rfl⟩
  · split at h <;> cases h

theorem orS_un {l : List St} (h : orS l = .un) : ∀ s ∈ l, s = .un := by
  unfold orS at h
  split at h
  · cases h
  · split at h
    · rename_i _ ha
      intro s hs
      simpa using (List.all_eq_true.1 ha) s hs
    · cases h

theorem andS_un {l : List St} (h : andS l = .un) : ∃ s ∈ l, s = .un := by
  unfold andS at h
  split at h
  · rename_i hc
    exact ⟨.un, by simpa using hc, rfl⟩
  · split at h <;> cases h

theorem andS_inh {l : List St} (h : andS l = .inh) : ∀ s ∈ l, s = .inh := by
  unfold andS at h
  split at h
  · cases h
  · split at h
    · rename_i _ ha
      intro s hs
      simpa using (List.all_eq_true.1 ha) s hs
    · cases h

theorem funS_un {a r : St} (h : funS a r = .un) : r = .un ∧ a = .inh := by
  unfold funS at h
  split at h
  · assumption
  · split at h <;> cases h

theorem funS_inh {a r : St} (h : funS a r = .inh) : r = .inh ∨ a = .un := by
  unfold funS at h
  split at h
  · cases h
  · split at h
    · assumption
    · cases h

/-- The checker's variable states agree with the propositions in `ρ`. -/
inductive Compat : List St → List Prop → Prop
  | nil : Compat [] []
  | cons {s : St} {P : Prop} {σ : List St} {ρ : List Prop} :
      (s = .inh → P) → (s = .un → ¬P) → Compat σ ρ → Compat (s :: σ) (P :: ρ)

theorem Compat.length {σ ρ} (h : Compat σ ρ) : σ.length = ρ.length := by
  induction h with
  | nil => rfl
  | cons _ _ _ ih => simp [ih]

theorem Compat.append {σ ρ σ' ρ'} (h : Compat σ ρ) (h' : Compat σ' ρ') :
    Compat (σ ++ σ') (ρ ++ ρ') := by
  induction h with
  | nil => simpa using h'
  | cons h1 h2 _ ih => exact .cons h1 h2 ih

theorem Compat.getD {σ ρ} (h : Compat σ ρ) : ∀ l,
    (σ.getD l .unk = .inh → ρ.getD l False) ∧ (σ.getD l .unk = .un → ¬ρ.getD l False) := by
  induction h with
  | nil => intro l; simp
  | cons h1 h2 _ ih =>
      intro l
      cases l with
      | zero => simpa using And.intro h1 h2
      | succ l => simpa using ih l

def stProp : St → Prop
  | .inh => True
  | .un => False
  | .unk => False

theorem assigns_compat : ∀ {n a}, a ∈ assigns n → a.length = n ∧ Compat a (a.map stProp)
  | 0, a, h => by
      simp [assigns] at h
      subst h
      exact ⟨rfl, .nil⟩
  | n + 1, a, h => by
      simp only [assigns, List.mem_flatMap, List.mem_cons, List.mem_nil_iff, or_false] at h
      obtain ⟨b, hb, rfl | rfl⟩ := h
      · obtain ⟨hl, hc⟩ := assigns_compat hb
        exact ⟨by simp [hl], .cons (fun _ => trivial) (fun h => by cases h) hc⟩
      · obtain ⟨hl, hc⟩ := assigns_compat hb
        exact ⟨by simp [hl], .cons (fun h => by cases h) (fun _ h => h) hc⟩

theorem exists_assign : ∀ xs : List Prop, ∃ a ∈ assigns xs.length, Compat a xs
  | [] => ⟨[], by simp [assigns], .nil⟩
  | P :: xs => by
      obtain ⟨a, ha, hc⟩ := exists_assign xs
      have hmem : ∀ s : St, s = .inh ∨ s = .un → s :: a ∈ assigns (P :: xs).length := by
        intro s hs
        simp only [List.length_cons, assigns, List.mem_flatMap, List.mem_cons,
          List.mem_nil_iff, or_false]
        exact ⟨a, ha, by rcases hs with rfl | rfl <;> simp⟩
      by_cases hP : P
      · exact ⟨.inh :: a, hmem _ (.inl rfl), .cons (fun _ => hP) (fun h => by cases h) hc⟩
      · exact ⟨.un :: a, hmem _ (.inr rfl), .cons (fun h => by cases h) (fun _ => hP) hc⟩

/-- `seen` and the assumptions `X` describe the same types. -/
def SeenInv (seen : List (Nat × List Ty)) (X : List Point) (ρ : List Prop) : Prop :=
  (∀ d args, (d, args) ∈ seen → Forced X d args ρ) ∧
  (∀ d args ρ0, (d, args, ρ0) ∈ X → (d, args) ∈ seen)

theorem SeenInv.extend {seen X ρ} (h : SeenInv seen X ρ) (xs : List Prop) :
    SeenInv seen X (ρ ++ xs) := by
  refine ⟨fun d args hm => ?_, h.2⟩
  obtain ⟨ρ0, ys, hx, rfl⟩ := h.1 d args hm
  exact ⟨ρ0, ys ++ xs, hx, by simp⟩

theorem SeenInv.push {seen X ρ} (h : SeenInv seen X ρ) (d : Nat) (args : List Ty)
    (ws : List Prop) :
    SeenInv ((d, args) :: seen) ((d, args, ρ) :: X) (ρ ++ ws) := by
  refine ⟨fun d' args' hm => ?_, fun d' args' ρ0 hm => ?_⟩
  · simp only [List.mem_cons] at hm
    rcases hm with heq | hm
    · cases heq
      exact ⟨ρ, ws, List.mem_cons_self, rfl⟩
    · obtain ⟨ρ0, ys, hx, rfl⟩ := h.1 d' args' hm
      exact ⟨ρ0, ys ++ ws, List.mem_cons_of_mem _ hx, by simp⟩
  · simp only [List.mem_cons] at hm
    rcases hm with heq | hm
    · cases heq
      exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (h.2 d' args' ρ0 hm)

theorem SeenInv.not_forced {seen X ρ d args} (h : SeenInv seen X ρ)
    (hs : (d, args) ∉ seen) : ¬Forced X d args ρ := by
  rintro ⟨ρ0, _, hx, _⟩
  exact hs (h.2 d args ρ0 hx)

/-! ## Soundness

The checker's answers hold in the world `W X` where the types in `seen` are
assumed empty: Uninhabited means no value there, Inhabited means a value
there. Both directions are needed together: an Uninhabited function type
needs an Inhabited argument.
-/

/-- What the soundness proof needs of a recursive checker. -/
def Sound {E Ext Op} (M : Model E Ext Op)
    (rec : List (Nat × List Ty) → List St → Ty → St) : Prop :=
  ∀ seen σ t X ρ, Compat σ ρ → SeenInv seen X ρ → WF ρ.length 0 t →
    (rec seen σ t = .un → ¬sem Op (M.W X) ρ t) ∧
    (rec seen σ t = .inh → sem Op (M.W X) ρ t)

/-- The data-type case, including the cycle rule. -/
theorem sound_dataState {E Ext Op} (M : Model E Ext Op) (hEnv : EnvWF E) (cap : Nat)
    {rec : List (Nat × List Ty) → List St → Ty → St} (IH : Sound M rec)
    {seen σ X ρ} (d : Nat) (args : List Ty) (hc : Compat σ ρ) (hs : SeenInv seen X ρ)
    (hargs : Scoped ρ args) :
    (dataState E cap rec seen σ d args = .un → ¬M.W X d args ρ) ∧
    (dataState E cap rec seen σ d args = .inh → M.W X d args ρ) := by
  unfold dataState
  cases hE : E d with
  | none => simp
  | some dt =>
    simp only
    by_cases hseen : (d, args) ∈ seen
    · -- cycle rule: `D[args]` is assumed empty, and `fix` says so
      simp only [hseen, ite_true]
      refine ⟨fun _ hW => ((M.fix X d args ρ hargs).1 hW).1 (hs.1 d args hseen), fun h => by cases h⟩
    · simp only [hseen, ite_false]
      by_cases hp : dt.nparams ≠ args.length
      · simp [hp]
      · by_cases hct : dt.ctors.isEmpty
        · simp [hp, hct]
        · simp only [hp, hct, ite_false, Bool.false_eq_true]
          have hnf := hs.not_forced hseen
          have hne : dt.ctors ≠ [] := by simpa using hct
          have hBody : ∀ J, Body E Ext Op J d args ρ ↔
              ∃ c ∈ dt.ctors, ∃ ws : List Prop, ws.length = c.nex ∧
                ∀ f ∈ c.fields, sem Op J (ρ ++ ws) (inst 0 (levels ρ.length c.nex ++ args) f) := by
            intro J
            simp only [Body, hE]
            split
            · rfl
            · rename_i h
              exact absurd ⟨Classical.not_not.1 hp, hne⟩ h
          have hlen : σ.length = ρ.length := hc.length
          have hnp : dt.nparams = args.length := Classical.not_not.1 hp
          constructor
          · -- every constructor is Uninhabited assuming `D[args]` is empty
            intro hun hW
            have hall := orS_un hun
            have hnb : ¬Body E Ext Op (M.W ((d, args, ρ) :: X)) d args ρ := by
              rw [hBody]
              rintro ⟨c, hcmem, ws, hws, hf⟩
              have hcs := hall _ (List.mem_map_of_mem hcmem)
              by_cases hcap : cap < c.nex
              · simp [hcap] at hcs
              · simp only [hcap, ite_false] at hcs
                obtain ⟨as, has, hcomp⟩ := exists_assign ws
                rw [hws] at has
                obtain ⟨s, hsmem, hsun⟩ := andS_un (orS_un hcs _ (List.mem_map_of_mem has))
                obtain ⟨f, hfmem, rfl⟩ := List.mem_map.1 hsmem
                rw [hlen] at hsun
                exact (IH _ _ _ ((d, args, ρ) :: X) (ρ ++ ws) (hc.append hcomp)
                  (hs.push d args ws) (WF.field hEnv hE hcmem hfmem hnp hargs ws hws)).1 hsun
                  (hf f hfmem)
            -- so it cannot be built at all (induction on finite values)
            exact M.discharge X d args ρ hargs hnf hnb hW
          · -- some constructor is Inhabited even assuming `D[args]` is empty
            intro hinh
            obtain ⟨s, hsmem, hsinh⟩ := orS_inh hinh
            obtain ⟨c, hcmem, rfl⟩ := List.mem_map.1 hsmem
            by_cases hcap : cap < c.nex
            · simp [hcap] at hsinh
            · simp only [hcap, ite_false] at hsinh
              obtain ⟨s2, hs2, hs2inh⟩ := orS_inh hsinh
              obtain ⟨as, has, rfl⟩ := List.mem_map.1 hs2
              have hall := andS_inh hs2inh
              obtain ⟨haslen, hcomp⟩ := assigns_compat has
              have hbody : Body E Ext Op (M.W ((d, args, ρ) :: X)) d args ρ := by
                rw [hBody]
                refine ⟨c, hcmem, as.map stProp, by simp [haslen], fun f hf => ?_⟩
                have hfi := hall _ (List.mem_map_of_mem hf)
                rw [hlen] at hfi
                exact (IH _ _ _ ((d, args, ρ) :: X) (ρ ++ as.map stProp) (hc.append hcomp)
                  (hs.push d args _)
                  (WF.field hEnv hE hcmem hf hnp hargs _ (by simp [haslen]))).2 hfi
              exact (M.fix X d args ρ hargs).2 ⟨hnf, M.cov X d args ρ hargs hnf hbody⟩

/-- Soundness of the checker at every fuel, `seen` set and `varState`. -/
theorem sound {E Ext Op} (M : Model E Ext Op) (hEnv : EnvWF E) (cap : Nat) :
    ∀ fuel, Sound M (check E cap fuel)
  | 0 => by intro seen σ t X ρ _ _ _; simp [check]
  | fuel + 1 => by
    intro seen σ t X ρ hc hs ht
    have IH := sound M hEnv cap fuel
    have hlen : σ.length = ρ.length := hc.length
    cases t with
    | bvar _ => simp [check]
    | fvar l =>
        simp only [check, sem]
        exact ⟨(hc.getD l).2, (hc.getD l).1⟩
    | prim _ => simp [check, sem]
    | «opaque» _ => simp [check]
    | arr a b =>
        have ha := IH seen σ a X ρ hc hs ht.1
        have hb := IH seen σ b X ρ hc hs ht.2
        simp only [check, sem]
        constructor
        · intro h hf
          obtain ⟨hbu, hai⟩ := funS_un h
          exact hb.1 hbu (hf (ha.2 hai))
        · intro h hav
          rcases funS_inh h with hbi | hau
          · exact hb.2 hbi
          · exact absurd hav (ha.1 hau)
    | all n b =>
        simp only [check, sem]
        by_cases hcap : cap < n
        · simp [hcap]
        · simp only [hcap, ite_false]
          constructor
          · intro h hall
            obtain ⟨s, hsmem, hsun⟩ := andS_un h
            obtain ⟨as, has, rfl⟩ := List.mem_map.1 hsmem
            obtain ⟨haslen, hcomp⟩ := assigns_compat has
            have h' := (IH seen _ _ X _ (hc.append hcomp) (hs.extend _)
              (by rw [List.length_append, List.length_map, haslen, ← hlen]
                  rw [hlen]; exact WF.openAt (by simpa [WF] using ht))).1 hsun
            rw [hlen] at h'
            exact h' (hall _ (by simp [haslen]))
          · intro h xs hxs
            obtain ⟨as, has, hcomp⟩ := exists_assign xs
            rw [hxs] at has
            have h' := (IH seen _ _ X _ (hc.append hcomp) (hs.extend _)
              (by rw [List.length_append, hxs, hlen]; exact WF.openAt (by simpa [WF] using ht))).2
              (andS_inh h _ (List.mem_map_of_mem has))
            rwa [hlen] at h'
    | ex n b =>
        simp only [check, sem]
        by_cases hcap : cap < n
        · simp [hcap]
        · simp only [hcap, ite_false]
          constructor
          · rintro h ⟨xs, hxs, hv⟩
            obtain ⟨as, has, hcomp⟩ := exists_assign xs
            rw [hxs] at has
            have h' := (IH seen _ _ X _ (hc.append hcomp) (hs.extend _)
              (by rw [List.length_append, hxs, hlen]; exact WF.openAt (by simpa [WF] using ht))).1
              (orS_un h _ (List.mem_map_of_mem has))
            rw [hlen] at h'
            exact h' hv
          · intro h
            obtain ⟨s, hsmem, hsinh⟩ := orS_inh h
            obtain ⟨as, has, rfl⟩ := List.mem_map.1 hsmem
            obtain ⟨haslen, hcomp⟩ := assigns_compat has
            have h' := (IH seen _ _ X _ (hc.append hcomp) (hs.extend _)
              (by rw [List.length_append, List.length_map, haslen, hlen]
                  exact WF.openAt (by simpa [WF] using ht))).2 hsinh
            rw [hlen] at h'
            exact ⟨_, by simp [haslen], h'⟩
    | con d =>
        rw [sem_data (.inl ⟨d, rfl⟩)]
        simp only [check, asData, spine, semData]
        exact sound_dataState M hEnv cap IH d [] hc hs (by simp [Scoped])
    | app f a =>
        rw [sem_data (.inr ⟨f, a, rfl⟩)]
        cases hd : asData (.app f a) with
        | none => simp [check, semData, hd]
        | some p =>
            obtain ⟨d, args⟩ := p
            simp only [check, semData, hd]
            exact sound_dataState M hEnv cap IH d args hc hs (WF.asData hd ht)

/--
**Soundness.** If the checker says a closed type is Uninhabited, the type has
no value, in every semantics `M` of a well-formed environment, for every cap
and fuel. (`TotalityCheck` closes a scrutinee's free type variables with
`exists` before checking it, so its types are closed.)
-/
theorem uninhabited_sound {E Ext Op} (M : Model E Ext Op) (hEnv : EnvWF E)
    (cap fuel : Nat) (t : Ty) (ht : WF 0 0 t)
    (h : check E cap fuel [] [] t = .un) : ¬M.HasValue t :=
  (sound M hEnv cap fuel [] [] t [] [] .nil ⟨by simp, by simp⟩ ht).1 h

/-- Inhabited answers are also sound for this semantics. (TotalityCheck does
not rely on them; see "Assumptions".) -/
theorem inhabited_sound {E Ext Op} (M : Model E Ext Op) (hEnv : EnvWF E)
    (cap fuel : Nat) (t : Ty) (ht : WF 0 0 t)
    (h : check E cap fuel [] [] t = .inh) : M.HasValue t :=
  (sound M hEnv cap fuel [] [] t [] [] .nil ⟨by simp, by simp⟩ ht).2 h

/-! ## Example: `P` is uninhabited through the cycle rule and function arguments

```
enum Never: Never(n: Never)
struct P(f: (P -> Never) -> Never)
enum Q: HasP(p: P), HasInt(i: Int)
```

`P` compiles: it occurs covariantly in its own definition (to the left of two
arrows). The checker calls `P` Uninhabited, so a match on `Q` may omit `HasP`
(`checkPattern` of `HasP(_)` is `checkType(P)`). The derivation:

* `Never`: its field `Never` is met again, so Uninhabited (cycle rule).
* `P -> Never`: the argument `P` is met again, so Uninhabited (cycle rule), and
  a function from an uninhabited type is Inhabited.
* `(P -> Never) -> Never`: an Inhabited argument and an Uninhabited result, so
  Uninhabited; `P`'s only constructor is Uninhabited.

The ids are `Never = 0`, `P = 1`, `Q = 2`.
-/

def exampleEnv : Env
  | 0 => some ⟨0, [⟨0, [.con 0]⟩]⟩
  | 1 => some ⟨0, [⟨0, [.arr (.arr (.con 1) (.con 0)) (.con 0)]⟩]⟩
  | 2 => some ⟨0, [⟨0, [.con 1]⟩, ⟨0, [.prim 0]⟩]⟩
  -- `enum Opt[a]: None, Some(v: a)`, to have a type with a parameter
  | 3 => some ⟨1, [⟨0, []⟩, ⟨0, [.bvar 0]⟩]⟩
  | _ => none

theorem exampleEnv_wf : EnvWF exampleEnv := by
  intro d dt hd c hc f hf
  match d, hd with
  | 0, hd | 1, hd | 2, hd | 3, hd =>
      simp only [exampleEnv, Option.some.injEq] at hd
      subst hd
      simp only [List.mem_cons, List.mem_nil_iff, or_false] at hc
      rcases hc with rfl | rfl <;>
        simp only [List.mem_cons, List.mem_nil_iff, or_false] at hf <;>
        subst hf <;> simp [WF]
  | n + 4, hd => simp [exampleEnv] at hd

theorem example_check_P : checkTop exampleEnv 10 (.con 1) = .un := by decide

theorem example_check_Never : checkTop exampleEnv 10 (.con 0) = .un := by decide

theorem example_check_Q : checkTop exampleEnv 10 (.con 2) = .inh := by decide

/-- `forall a. Opt[a]` is Inhabited (by `None`). -/
theorem example_check_Opt :
    checkTop exampleEnv 10 (.all 1 (.app (.con 3) (.bvar 0))) = .inh := by decide

/-- In every semantics of the environment, `P` (so `HasP`) has no value. -/
theorem example_P_has_no_value {Ext Op} (M : Model exampleEnv Ext Op) :
    ¬M.HasValue (.con 1) :=
  uninhabited_sound M exampleEnv_wf maxQuantifierVars 10 (.con 1) trivial example_check_P

/-! A semantics of this environment exists, so the hypotheses of `Model` are
not vacuous here: `Never` and `P` have no values, `Q` and every `Opt[a]` have
values, and the checker cannot see into anything else (taken as inhabited). -/

def exampleV (d : Nat) (args : List Ty) : Prop :=
  ¬((d = 0 ∨ d = 1) ∧ args = [])

def exampleW (X : List Point) (d : Nat) (args : List Ty) (ρ : List Prop) : Prop :=
  ¬Forced X d args ρ ∧ exampleV d args

theorem example_body (Op : Nat → List Prop → Prop) (Y : List Point) (d : Nat)
    (args : List Ty) (ρ : List Prop) :
    Body exampleEnv (fun _ _ _ => True) Op (exampleW Y) d args ρ ↔ exampleV d args := by
  match d, args with
  | 0, [] =>
      simp [Body, exampleEnv, exampleV, exampleW, sem, semData, asData, spine, inst, levels]
  | 1, [] =>
      simp [Body, exampleEnv, exampleV, exampleW, sem, semData, asData, spine, inst, levels]
  | 2, [] =>
      simp [Body, exampleEnv, exampleV, exampleW, sem, semData, asData, spine, inst, levels]
  | 3, [_] => simp [Body, exampleEnv, exampleV]
  | 0, _ :: _ => simp [Body, exampleEnv, exampleV]
  | 1, _ :: _ => simp [Body, exampleEnv, exampleV]
  | 2, _ :: _ => simp [Body, exampleEnv, exampleV]
  | 3, [] => simp [Body, exampleEnv, exampleV]
  | 3, _ :: _ :: _ => simp [Body, exampleEnv, exampleV]
  | n + 4, _ => simp [Body, exampleEnv, exampleV]

def exampleModel (Op : Nat → List Prop → Prop) :
    Model exampleEnv (fun _ _ _ => True) Op where
  W := exampleW
  fix := by
    intro X d args ρ _
    rw [example_body]
    rfl
  discharge := by
    intro X d args ρ _ _ hnb hW
    rw [example_body] at hnb
    exact hnb hW.2
  cov := by
    intro X d args ρ _ _ h
    rw [example_body] at h ⊢
    exact h

theorem example_Q_has_value (Op : Nat → List Prop → Prop) :
    (exampleModel Op).HasValue (.con 2) := by
  simp [Model.HasValue, exampleModel, exampleW, exampleV, Forced, sem, semData, asData, spine]

/-! ## Example: the model is capture-free

```
enum T[x]: Base(v: x), Step[a](t: T[a])
```

`forall b. T[b]` has the value `Step(Base(1))`. The Scala checker currently
answers Uninhabited for it: `Type.normalize` renames `b` to `a`, and the
existential `a` of `Step` is then confused with it, both in `varState` and in
the `seen` set (see "Assumptions"). The model gives each variable its own
level and answers Inhabited.
-/

def captureEnv : Env
  | 0 => some ⟨1, [⟨0, [.bvar 0]⟩, ⟨1, [.app (.con 0) (.bvar 0)]⟩]⟩
  | _ => none

theorem capture_example :
    checkTop captureEnv 10 (.all 1 (.app (.con 0) (.bvar 0))) = .inh := by decide

/-!
The same happens through `normalize` renaming a quantifier inside a field:

```
enum D[x]: A(v: x), B(w: exists y. D[y])
```

`forall b. D[b]` has the value `B(A(1))`. Scala renames `b` to `a`, then the
field's `exists y. D[y]` to `exists a. D[a]`, which matches the `seen` entry
`D[a]` for the outer `a`, so Scala answers Uninhabited.
-/

def captureNestedEnv : Env
  | 0 => some ⟨1, [⟨0, [.bvar 0]⟩, ⟨0, [.ex 1 (.app (.con 0) (.bvar 0))]⟩]⟩
  | _ => none

theorem capture_nested_example :
    checkTop captureNestedEnv 10 (.all 1 (.app (.con 0) (.bvar 0))) = .inh := by decide

/-!
## Assumptions

`uninhabited_sound` is proved from the hypotheses of `Model`. Relating it to
Bosatsu programs takes the following assumptions about the rest of Bosatsu.

1. **The Scala checker computes `check`.** `Analyzer.checkType` on a
   well-kinded type gives the same answer as `check` on its translation. The
   translation is taken after `Type.normalize` (which drops unused binders and
   merges adjacent ones): constants with a definition are `con`, built-in
   always-inhabited types are `prim`, quantified variables of kind `*` are
   `fvar`/`bvar`, and everything the checker answers Unknown for without
   looking inside is `opaque`. Known gaps:
   * *Name capture.* Scala names variables; the model gives every opened
     variable a fresh level. The Scala code does not keep names apart:
     `normalize` renames every quantifier binder to the first free names
     (`a`, `b`, ...), avoiding only the body's own free variables, and a
     constructor's existentials keep their declared names
     (`varState ++ assignment`). Either can reuse the name of a variable that
     is already in `varState` or in a `seen` entry, and then the cycle rule
     fires for a different type. Two reachable cases, both accepted by
     TotalityCheck as matches that omit a constructor which has values:
     `capture_example` (an existential named like the normalized outer
     variable) and `capture_nested_example` (a field's `exists y` renamed to
     the outer variable's name). The proof covers the Scala code only once every
     variable it opens (quantifier binders, including those inside field
     types after substitution, and constructor existentials) gets a name
     distinct from every name in `varState` and in `seen`.
   * *Renormalization.* Scala normalizes again after substituting type
     arguments into a field; the model does not. This can regroup or drop
     quantifiers, which changes which ones exceed the cap, and can make two
     types syntactically equal for the `seen` check in Scala and not in the
     model (or the reverse).
   * *Termination.* The model has fuel and answers Unknown when it runs out.
     Scala has none: if a type whose expansion never repeats (polymorphic
     recursion such as `T[a] = C(T[List[a]])`) is accepted, `checkType`
     would not terminate on it. Where Scala is capture-free and terminates,
     its answer is the model's at enough fuel, and soundness holds at every
     fuel. (Capture can also make Scala terminate where the model never
     does: with `B(w: forall y. D[y])` in `capture_nested_example`, Scala
     answers Uninhabited, which is right there, and the model answers
     Unknown at every fuel.)
2. **Values are finite constructor trees** (`Model.discharge`, together with
   `Model.fix`). Bosatsu's recursion check rejects general recursion, so a
   closed value of a data type is built by finitely many constructor
   applications; `def p -> P: p()` style definitions that would inhabit `P`
   are rejected. This assumes the recursion check itself is sound. Its
   callback rule was not: `struct Cb(f: forall a. (a, a -> Int) -> Int)` with
   `def bad(c: Cb) -> Int: recur c: case Cb(f): f(c, x -> bad(x))` was
   accepted and loops. The proof assumes the restricted rule of
   johnynek/bosatsu#2517 (`reachableCallbackRuleApplies`: callback binders
   count as smaller only when no argument type at the call can carry a value
   of the target type at an input position).
3. **Recursive types are covariant** (`Model.cov`). `KindFormula` requires
   every occurrence of a type inside its own definition to be in a covariant
   position (`Constraint.RecursiveView`), and Bosatsu has no mutually
   recursive types. Without this the cycle rule means nothing: for
   `struct E(f: E -> Never)` the checker calls `E` Inhabited because
   `E -> Never` is Inhabited when `E` is assumed empty, yet `E` would have a
   value exactly when it has none.
4. **Typed values denote proofs of `sem`.** A closed, well-typed, total
   Bosatsu value of type `t` gives a proof of `sem Op (M.W []) [] t` for the
   semantics `M` of its environment: functions are total, `forall` values can
   be instantiated at any type (every type is inhabited or not), and `exists`
   values pack a type. External (FFI) values must have the types they claim;
   external and unknown types have the arbitrary meaning `Ext`, and opaque
   leaves `Op`, and the theorem holds for every choice of both.

The semantics is classical: it over-approximates the values Bosatsu can
define. That is the right direction for Uninhabited answers (no value in the
semantics, so no value in Bosatsu). It is not for Inhabited answers:
`forall a. Either[a, a -> Never]` is Inhabited here, but no parametric Bosatsu
value has that type. `inhabited_sound` is only about this semantics, and
TotalityCheck does not use Inhabited answers to accept a match.

Division of labour: the induction principle behind the cycle rule is
`Model.discharge` and `Model.cov`, which are assumed. That the checker's use
of `seen` respects them (assumptions made under binders, inside constructor
fields, and across nested expansions), and everything about functions,
quantifiers, existentials and the cap, is proved. A `Model` is constructed
only for `exampleEnv`; constructing one for every well-formed covariant
environment is not done here.
-/

end Inhabitedness
