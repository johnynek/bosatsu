# Well-Typed Statement Generator Design

Status: proposed  
Date: 2026-02-16

## Goal
Design `wellTypedProgramGen: Gen[WellTypedProgram]` that produces source `Statement`s plus typing witnesses (`expected`, `typeEnv`) so we can property-test the compiler pipeline with generated programs, not just parser-shaped random syntax.

## Motivation
Current test generators have a gap:

1. `Generators.genStatements` generates syntactically valid statements, but not type-directed ones.
2. `Generators.genTypedExpr` explicitly notes it is random and not well typed.

That means we can stress parsing/printing/proto paths, but we cannot reliably generate source programs to test:

1. `SourceConverter` + inference success paths,
2. normalization and compilation (`MatchlessFromTypedExpr`),
3. backend codegen against broader random inputs.

## Non-goals (v1)
1. Full language coverage on day one (especially recursion, comprehensions, and complex pattern guards).
2. Generating every valid program shape.
3. Proving generator soundness independent of the typechecker.

v1 should prioritize high signal and low flake risk.

## Proposed API

```scala
object WellTypedGenerators {
  final case class Config(
    maxStatements: Int,
    maxExprDepth: Int,
    maxTypeDepth: Int,
    allowDefs: Boolean,
    allowTypeDefs: Boolean
  )

  final case class WellTypedProgram(
    statements: List[Statement],
    expected: Map[Identifier.Bindable, rankn.Type],
    typeEnv: rankn.TypeEnv[Kind.Arg]
  )

  def wellTypedProgramGen(cfg: Config): Gen[WellTypedProgram]
}
```

`wellTypedProgramGen` is the primary output because statements alone are not enough to assert typing behavior; tests also need value-type witnesses (`expected`) and the type-constant interpretation context (`typeEnv`).

## Core Idea
Use type-directed synthesis:

1. Generate a target type `t`.
2. Generate an expression `e` such that `e : t` under the current environment.
3. Emit a statement binding `name = e`.
4. Extend the environment with `name : t`.
5. Repeat.

This is a standard introduction/elimination strategy:

1. Introduction rules build values of a requested type (`lambda`, literals, constructors).
2. Elimination rules consume existing values/functions from scope (`var`, application).

## Generation Context

```scala
final case class ValSig(name: Identifier.Bindable, sigma: rankn.Type)
final case class CtorSig(
  pack: PackageName,
  cons: Identifier.Constructor,
  sigma: rankn.Type,
  resultTypeConst: rankn.Type.Const.Defined
)

final case class Ctx(
  packageName: PackageName,
  vals: Vector[ValSig],
  ctors: Vector[CtorSig],
  typeEnv: rankn.TypeEnv[Kind.Arg],
  usedNames: Set[Identifier.Bindable]
)
```

Notes:

1. `vals` holds in-scope names generated so far.
2. `usedNames` is the global freshness set, and must include all value binders:
   `vals.forall(v => usedNames(v.name))`.

3. `ctors` is a sampling cache; the source of truth is `typeEnv`, and every cached constructor must exist there:
   `ctors.forall(c => typeEnv.getConstructor(c.pack, c.cons).isDefined)`.

4. `CtorSig.resultTypeConst` records the constructor's final return type constant (the defined type it constructs), so constructor selection can quickly filter by demanded result type root before full instantiation.
5. `ctors` and `typeEnv` come from imported interfaces plus generated type definitions.
6. v1 uses monomorphic goals by default, but `sigma` storage keeps polymorphism support open.

## TypeEnv witness
`WellTypedProgram.typeEnv` is required so `expected: Map[Bindable, Type]` has an explicit interpretation context for `TyConst` references (especially generated local `struct`/`enum` types).

Planned construction:

1. Incrementally update `ctx.typeEnv` while generating statements (for v1 this is mostly imported/predef; later phases add local type defs).
2. On finalize, derive the compiler-view env from generated statements via the same path used by compilation (`SourceConverter.toProgram` -> parsed type env -> `TypeEnv.fromParsed`) and merge with imported env.
3. Return that finalized env as `WellTypedProgram.typeEnv`.

This keeps the witness aligned with how the compiler interprets type constants.

## Type Generation
Generate only inhabitable goal types to keep `genExpr(goal)` failure low.

### `genType(ctx, depth): Gen[Type]`
Weighted choices:

1. Ground base types: `Int`, `String`, `Char`, `Float64`, `Bool`, `Unit`.
2. ADT applications with available constructors: `Option[a]`, `List[a]`, tuples, and local generated ADTs.
3. Function types with explicit arity (small arity), i.e. `a -> r`, `(a1, a2) -> r`, `(a1, a2, a3) -> r`, ... (internally `Fn1`..`Fn32`, not curried chains).

### Inhabited-type filter
Use a memoized predicate `canInhabit(ctx, t)`:

1. `True` for base literal types.
2. `True` for function types (we can always lambda-introduce at that exact arity if result type is inhabitable under an extended scope).
3. `True` for ADT types when some constructor can be instantiated and all argument types are inhabitable.
4. `True` if there is already an in-scope value assignable to `t`.

If no candidate is found at some depth, fall back to base types.

## Expression Generation

```scala
def genExpr(ctx: Ctx, goal: Type, depth: Int): OptionT[Gen, Declaration.NonBinding]
```

`OptionT` is intentional: some goals may be temporarily unreachable under current context.

### Introduction path (`genIntro`)
Construct directly by type shape:

1. If goal is `Type.Fun(args, res)`: generate a lambda with exactly `args.length` parameters, extend `ctx`, generate body for `res`.
2. If goal is a literal type: emit literal (`Int`, `String`, `Char`, `Float64`).
3. If goal is an ADT: choose constructor where `resultTypeConst` matches goal root `TyConst` (when present), then instantiate `sigma` to the full goal and recursively build args.
4. If goal is tuple/list/option and syntax sugar is easier, emit tuple/list forms directly.

### Elimination path (`genElim`)
Use available names:

1. Choose a value/function `v : sigma` from `ctx.vals` or constructor from `ctx.ctors`.
2. For constructors, optionally prefilter by `resultTypeConst` vs goal root `TyConst`; then instantiate `sigma` toward `goal` (using `Type.instantiate`-style matching on result type).
3. If instantiation yields needed argument types `a1..an`, recursively generate args and build one application node with arity `n` (not nested curried applications).
4. If no args needed and instantiation matches goal, emit variable directly.

### Control flow expressions (phase-gated)
After v1 is stable:

1. `if` / ternary by generating `Bool` condition and same-typed branches.
2. `match` with typed pattern generation and exhaustiveness by construction.

## Complete Declaration Surface (Exhaustive)
This section lists every `Declaration` syntax constructor and how to generate it.

### Declaration (binding forms)
1. `Declaration.Binding(BindingStatement(pattern, value, in))`  
   Syntax shape: `<pattern> = <nonbinding>` then continuation declaration.  
   Generation: choose `pattern`, generate `value` that matches pattern type, extend env with pattern bindings for `in`.

2. `Declaration.Comment(CommentStatement[Padding[Declaration]])`  
   Syntax shape: `# ...` preceding declaration.  
   Generation: wrapper over an already generated declaration; semantics-preserving noise.

3. `Declaration.DefFn(DefStatement(...))`  
   Syntax shape: local `def` with args/body plus continuation.  
   Generation: generate function type, args/patterns/body, optional return annotation, then generate continuation in extended env.

4. `Declaration.LeftApply(arg, fn, result)`  
   Syntax shape: `<pattern> <- <fn(exprs...)>` then body.  
   Generation: choose `fn` returning function-on-last-arg; generate lambda body for appended arg. Equivalent to desugared `Apply` with final lambda argument.

### Declaration.NonBinding (expression forms)
1. `Declaration.Annotation(fn, tpe)`  
   Syntax: `<expr>: <TypeRef>`.  
   Generation: generate expression of subtype/instantiation-compatible type, then annotate/widen to demanded type.

2. `Declaration.Apply(fn, args, kind = Parens)`  
   Syntax: `f(a, b, ...)`.  
   Generation: pick callable with arity `n`, generate exactly `n` typed args, build one application.

3. `Declaration.Apply(fn, args, kind = Dot)`  
   Syntax: `recv.f(a, b, ...)` (parser desugars shape constraints).  
   Generation: same as apply, but render in dot form when legal for readability/syntax coverage.

4. `Declaration.ApplyOp(left, op, right)`  
   Syntax: `left <op> right`.  
   Generation: choose operator identifier with known function type and generate typed operands.

5. `Declaration.CommentNB(CommentStatement[Padding[NonBinding]])`  
   Syntax shape: comment wrapping NB expression.  
   Generation: wrapper/noise over generated nonbinding.

6. `Declaration.IfElse(ifCases, elseCase)`  
   Syntax: `if cond: ... elif cond: ... else: ...`.  
   Generation: generate `Bool` conditions and branch bodies of demanded type.

7. `Declaration.Ternary(trueCase, cond, falseCase)`  
   Syntax: `trueCase if cond else falseCase`.  
   Generation: same typing rule as `IfElse`, compact syntax variant.

8. `Declaration.Lambda(args, body)`  
   Syntax: `(p1, p2, ...) -> body` / `p -> body`.  
   Generation: introduction for `Type.Fun(args, res)` with exact arity and typed arg patterns.

9. `Declaration.Literal(lit)`  
   Syntax: numeric/string/char/float literal.  
   Generation: direct for literal-compatible goal types.

10. `Declaration.Match(kind, arg, cases)`  
    Syntax: `match`/`recur` with `case` branches.  
    Generation: choose scrutinee type, build total pattern set, generate each branch in branch-extended env.

11. `Declaration.Matches(arg, pattern)`  
    Syntax: `expr matches pattern` (returns `Bool`).  
    Generation: generate `arg` and pattern at same type; use when demanded type is `Bool`.

12. `Declaration.Parens(of)`  
    Syntax: `(decl)`.  
    Generation: wrapper for precedence/roundtrip diversity.

13. `Declaration.TupleCons(items)`  
    Syntax: `()`, `(a,)`, `(a, b, ...)`.  
    Generation: produce tuple values for tuple goal types.

14. `Declaration.Var(name)`  
    Syntax: identifier reference.  
    Generation: elimination path from env value/constructor.

15. `Declaration.RecordConstructor(cons, arg)`  
    Syntax: `Cons { field }`, `Cons { field: value, ... }`.  
    Generation: choose struct/constructor with record style fields; generate each provided field expression.

16. `Declaration.StringDecl(items)` with `StringDecl.Part` variants:  
    1. `StringDecl.Literal(region, str)`  
    2. `StringDecl.StrExpr(nonBinding)`  
    3. `StringDecl.CharExpr(nonBinding)`  
    Syntax: interpolated string segments `'foo${s}$.{c}'`.  
    Generation: mixed literal and embedded expressions (`String` or `Char`) to produce `String`.

17. `Declaration.ListDecl(list)` with `ListLang` variants:  
    1. `ListLang.Cons(items)` where items are `SpliceOrItem.Item(expr)` or `SpliceOrItem.Splice(expr)` (`*expr`)  
    2. `ListLang.Comprehension(expr, binding, in, filter)`  
    Syntax: `[a, *rest]`, `[expr for p in src if cond]`.  
    Generation: constructor/list-intro path plus optional comprehension path.

18. `Declaration.DictDecl(list)` with `ListLang` variants:  
    1. `ListLang.Cons(items)` where item is `KVPair(key, value)`  
    2. `ListLang.Comprehension(KVPair(key, value), binding, in, filter)`  
    Syntax: `{k: v, ...}`, `{k: v for p in src if cond}`.  
    Generation: only when demanded type is `Dict[k, v]`; generate typed keys/values and source/filter.

### Internal declaration helper syntax that must also be generated
1. `Declaration.MatchBranch(pattern, guard, body)` for `match` cases.
2. `Declaration.RecordArg.Simple(field)` and `Declaration.RecordArg.Pair(field, arg)` for record constructors.
3. `Declaration.ApplyKind.Parens` and `Declaration.ApplyKind.Dot` for application rendering diversity.

## Pattern Generation
`match` generation requires typed pattern synthesis plus branch-local binding environments.

### Pattern surface (exhaustive)
Generate all `Pattern` forms:

1. `Pattern.WildCard` syntax `_`.
2. `Pattern.Literal(lit)` syntax literal pattern.
3. `Pattern.Var(name)` syntax bindable name.
4. `Pattern.Named(name, pat)` syntax `pat as name`.
5. `Pattern.Annotation(pat, tpe)` syntax `pat: Type`.
6. `Pattern.PositionalStruct(name, params)` syntax:
   1. constructor tuple-like: `Cons(p1, p2, ...)`,
   2. constructor record-like: `Cons { f1: p1, f2, ... }`,
   3. tuple patterns: `()`, `(p,)`, `(p1, p2, ...)`,
   4. partial constructor patterns: `Cons(...)` and `Cons(p1, p2, ...)` (`NamedPartial`).
7. `Pattern.ListPat(parts)` syntax `[p1, p2, *rest]`, with parts:
   1. `ListPart.Item(pat)`,
   2. `ListPart.NamedList(name)` (`*name`),
   3. `ListPart.WildList` (`*_`).
8. `Pattern.StrPat(parts)` syntax interpolated string patterns with parts:
   1. `StrPart.LitStr(s)`,
   2. `StrPart.NamedStr(name)` (`${name}`),
   3. `StrPart.WildStr` (`${_}`),
   4. `StrPart.NamedChar(name)` (`$.{name}`),
   5. `StrPart.WildChar` (`$.{_}`).
9. `Pattern.Union(head, rest)` syntax `p1 | p2 | ...`.

### Typed branch plan
Use an internal typed plan during generation:

```scala
final case class BranchPattern(
  parsed: Pattern.Parsed,
  typed: Pattern[(PackageName, Identifier.Constructor), rankn.Type],
  scrutineeType: rankn.Type,
  bindings: Map[Identifier.Bindable, rankn.Type]
)
```

`bindings` is computed while building the pattern, not recovered later by guessing.

### Total pattern-set generation
Define:

```scala
def totalPatterns(ctx: Ctx, tpe: rankn.Type, depth: Int): NonEmptyList[BranchPattern]
```

Rules (must always return a total set):

1. If `tpe` root is a known defined type in `ctx.typeEnv` (including predef ADTs) and has constructors, generate one branch per constructor.
2. Instantiate constructor argument types by substituting the type parameters of the `DefinedType` with the concrete type arguments from `tpe`.
3. For each constructor argument type, generate subpatterns with a depth budget:
   1. prefer binders (`Var`) when branch expressions should use the value,
   2. prefer `_` when binding is unnecessary,
   3. optionally recurse into structural subpatterns for additional coverage.
4. If no structural decomposition is available (functions, primitive numerics, opaque defined/external types, unsupported shape), return single wildcard branch `_`.
5. Keep branches unguarded when proving totality by construction; guarded branches can be added only with an unguarded fallback branch.

### DefinedType-specific totality
Given `dt: DefinedType[Kind.Arg]` and demanded type `T`:

1. Let `constructors = dt.constructors`.
2. Build branch pattern for each constructor `cf` in source order.
3. Each branch root is `Pattern.PositionalStruct((dt.packageName, cf.name), argsPats)`.
4. If `constructors` is empty, emit wildcard-only fallback (`_`) for that type.
5. Otherwise, this constructor-complete set is total for that `DefinedType` (modulo valid arity), so it is the default strategy for ADTs/enums.

### Branch environment extension
For each generated branch:

1. Compute `branchCtx = ctx` extended with `bindings` from that branch pattern.
2. Generate guard (if any) and branch body under `branchCtx`.
3. Because each branch has different `bindings`, branch generation is independent per branch.

### Validation pass for pattern sets
Before finalizing a `match`:

1. Convert planned patterns to typed/internal form.
2. Run `TotalityCheck` validation (`validatePattern`, missing/unreachable checks) in test mode.
3. If invalid/non-total, regenerate pattern set with reduced complexity or wildcard fallback.

## Statement Generation
Use a stateful loop over `Ctx`.

### v1 statement set
1. `Statement.Bind` with variable pattern only.
2. Optional comments/padding can be added after semantic generation (do not affect typing).

This already enables nontrivial typed programs through lambdas/apps/constructors.

### v2 statement set
1. `Statement.Def` for named function declarations.
2. Local `struct`/`enum` generation, then value generation that uses those constructors.

### v3 statement set
1. Recursive defs constrained to known-safe templates.
2. Rich patterns and guards in generated `match`.

## Name and Scope Hygiene
1. Generate fresh bindable names not in `ctx.usedNames`.
2. For lambdas/matches, avoid shadowing collisions by allocating fresh locals and updating context explicitly.
3. Keep top-level names unique in v1 (simplifies witness checking and shrinking).

## Shrinking Strategy
Structural shrinking alone tends to break typing. Use a typing-aware shrinker:

1. Remove whole statements while preserving at least one exported/main candidate.
2. Shrink expression subtrees with typed rules (replace with in-scope variable of same type, simplify constructor args, simplify literals).
3. Recheck candidates with the fast typecheck harness; keep only well-typed shrinks.

This balances reliability and implementation cost.

## How This Checks the Compiler
The generator enables new properties beyond parser roundtrips.

## 1. Typecheck success property

For all generated programs, full package typechecking succeeds:

1. Build a parsed package from generated statements.
2. Run `PackageMap.typeCheckParsed` (so predef import behavior matches real compilation).
3. Assert no type errors.

This validates both generator and `SourceConverter` + infer success-path robustness.

## 2. Witness agreement property

For `wellTypedProgramGen`, compare inferred top-level types with generator expectations:

1. For each generated bind `x : t_expected`,
2. infer type `t_inferred`,
3. validate `t_expected` is closed/resolved in `wellTypedProgram.typeEnv` (all referenced `TyConst` are known in that env),
4. assert `t_inferred` subsumes/equates `t_expected` (modulo normalization/quantification), using the same env context.

This catches generator bookkeeping bugs and inference regressions.

## 3. Normalization equivalence property

For a generated package with a ground `main` value:

1. infer unoptimized body (`inferBodyUnopt`),
2. normalize (`TypedExprNormalization.normalizeProgram`),
3. evaluate both via `Evaluation`,
4. assert equal resulting value.

This directly tests optimization soundness on random typed source programs.

## 4. Matchless compilation totality property

For inferred packages:

1. run `MatchlessFromTypedExpr.compile`,
2. assert no exceptions and all lets compile.

This is a strong stability property for the middle-end.

## 5. Backend smoke/semantic checks (nightly)

For a restricted generated subset (ground result types, limited externals):

1. compile with backend(s) (C/Python as configured),
2. evaluate `main`,
3. compare with interpreter result.

Run at lower test counts initially due runtime cost.

## Incremental Rollout Plan
1. Phase 1: `Bind`-only generator, base/ADT/function expressions, typecheck + matchless properties.
2. Phase 2: add `Def`, add generated type definitions (`struct`/`enum`), add witness agreement checks.
3. Phase 3: add `match` generation with typed patterns and guards, add normalization equivalence property.
4. Phase 4: backend cross-checks for a restricted profile.

## Risks and Mitigations
1. High rejection rate from `OptionT` failures.
   Mitigation: inhabited-type filtering + fallback to base goals + bounded retries.

2. Inference blowups on deep types/expressions.
   Mitigation: strict depth/arity limits and weighted small-size bias.

3. Shrinker flakiness/perf cost.
   Mitigation: typed rewrite shrinker first, then typecheck-filtered fallback.

4. Overfitting to current inference behavior.
   Mitigation: keep generator rules type-theoretic (intro/elim) and avoid solver internals where possible.

## Why This Design Matches Bosatsu
1. It composes with existing ASTs (`Statement`, `Declaration`) and existing test infra (`ScalaCheck`, `PackageMap.typeCheckParsed`).
2. It follows the same environment-driven typing model used by inference.
3. It gives a clear path from “can typecheck generated source” to “can validate compiler transformations and codegen behavior” using the same generated inputs.
