# Bosatsu Pre-PR Review Guidance

This reviewer guidance supplements `coding_style.md`. Report approval-blocking findings only.

Reviewer expectations for this repo:

1. Correctness and robustness come first. Performance comes next. Idiomatic repo style matters
   after that. Accept non-idiomatic structure only when it has a clear semantic or performance win.
2. Treat compiler, optimizer, parser, and codegen changes as language-semantics work, not just
   refactors. Reject changes that rely on hand-wavy arguments; require the exact invariant or
   counterexample to be stated and tested.
3. Do not allow unsafe casts or ad hoc runtime type dispatch. Avoid `asInstanceOf`, `isInstanceOf`,
   `AnyRef` identity tricks, wildcard `?` escape hatches, and type-driven branching except for
   normal idiomatic pattern matching on well-typed ADTs.
4. Keep methods parametric when they can be parametric. If a method preserves a type parameter,
   carry that parameter through the signature instead of weakening the code to existential or
   dynamically typed shapes.
5. Prefer type classes and companion givens such as `Hash`, `Order`, `Eq`, and `Show` over JVM
   defaults like `hashCode`, `compareTo`, `equals`, and `toString`. If ordering or hashing is
   missing, define the right instance in the right place rather than adding bespoke comparisons.
6. Prefer existing repo helpers and idioms over duplicated scaffolding or special cases. Reuse
   existing parser combinators, `Let` constructors, shared traversals, `Argument` parsers, and
   companion-object utilities instead of copying similar logic into a new local implementation.
7. In hot paths, keep a high performance bar. Be skeptical of redundant traversals, avoidable
   closure allocation, capturing parsers, unnecessary bindings, and data structures with worse
   asymptotics than the operation requires.
8. Require stack safety for deep recursive traversals over `Expr`, `BoolExpr`, Matchless IR, and
   similar compiler structures; prefer tail-recursive or iterative implementations.
9. For optimizer and codegen changes, require focused regressions that pin the intended semantic
   behavior and, when relevant, the generated-shape properties that motivated the change.
10. Prefer portable tooling and reproducible harnesses. Reject hardcoded machine-specific paths,
    temp directories, or environment assumptions in checked-in code.
11. Avoid default arguments in touched Scala APIs and call sites; prefer explicit arguments so
    refactors fail loudly instead of silently changing behavior.
12. Require comments when a transformation is subtle, when a fallback looks surprising, or when
    a choice is only correct because of a non-obvious compiler invariant.
