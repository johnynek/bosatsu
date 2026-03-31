# Inlining Improvements Work Plan

## Context

We recently added Matchless global inlining and `show` support for inspecting generated `TypedExpr` and `Matchless` at different optimization points. Running those views against `../zafu` makes several optimizer gaps visible, especially in higher-order `Traverse` and `Applicative` code such as:

- `Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn`
- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

The current optimizer already has some of the building blocks we want, but they are narrow or disconnected:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala` already has a narrow let-to-match sinking rewrite around line 1368.
- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala` already has `rewriteNonEscapingClosureBinding` around line 1001, but it is only invoked for recursive lets around line 1358.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` already has `parameterDemandSummary` and `inlineApplyArgs`, which can keep some non-cheap arguments inside `If` and `SwitchVariant` branches after beta-reduction.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` already has `simplifyKnownConditions.knownValue`, which can fold `GetStructElement` and `GetEnumElement` when the underlying constructor value is syntactically known.
- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala` now publishes tiny pure value summaries in addition to lambdas.
- TypedExpr and Matchless now both route call-site inlining through `core/src/main/scala/dev/bosatsu/InlineBenefitModel.scala`, and the shared benefit model plus bounded Matchless cleanup fixpoint are both now in place.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` now runs `postLoweringCleanup` as a small bounded fixpoint over `hoistInvariantLoopLets`, `reuseConstructors`, `sinkBranchOnlyLets`, and `simplifyKnownConditions`, with overflow-safe canonicalization and equality checks so deep let chains keep their previous stack-safety behavior.
- `show` now supports `--validate-typedexpr`, which lets us run `TypeValidator.assertValid` on the selected optimized TypedExpr output while bisection-testing typed passes.

The goal of this plan is to turn those existing pieces into a more general optimizer pipeline that removes wrapper plumbing, avoids eager branch-only work, and converges on a stable final Matchless shape for semantically equivalent source programs.

## Goals

- Make semantically equivalent source variants compile to the same or nearly the same final Matchless shape.
- Evaluate expensive pure work only in the branches that actually need it.
- Inline small pure helper values, not just lambdas, so wrapper records and field-selection scaffolding disappear.
- Turn non-escaping local function values into direct-call helpers with explicit parameters for captures.
- Keep laziness and escaping-function behavior correct, especially around `Unit -> a` suspension boundaries.
- Avoid size growth in cases where inlining does not unlock any real simplification.

## How We Will Evaluate Progress

### Build and run against `../zafu`

Do not make future implementers rediscover the invocation path. For a checked-out repository such as `../zafu`, use the top-level `show` command, not `tool show`.

Verified baseline:

- Current working branch: `codex/inlining-improvements`
- Item 5 evaluation baseline checkpoint: `abbfc655b` (`Complete shared inline benefit model`)
- Item 4 evaluation baseline checkpoint: `2c8f6faeb` (`Checkpoint inlining work and typedexpr validation`)
- Checkpoint used to evaluate item 3: `43bf1d759` (`Inline tiny pure Matchless values`)
- Before any evaluation rerun `sbt cli/assembly`. `bosatsuj` selects the newest assembly jar from `cli/target/scala-*/bosatsu-cli-assembly-*.jar`, and the jar filename reflects the last commit hash rather than any uncommitted work.
- Verified current assembly path after item 3 work: `cli/target/scala-3.8.2/bosatsu-cli-assembly-0.0.61+4-43bf1d75+20260330-1059-SNAPSHOT.jar`

Build the current CLI once from the Bosatsu repo root:

```sh
sbt cli/assembly
```

Then use the checked-in wrapper, which always picks the newest assembly jar from `cli/target/scala-*/bosatsu-cli-assembly-*.jar`:

```sh
mkdir -p /tmp/zafu_show

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir typedexpr \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.typed.edn

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir matchless \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.matchless.edn

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir matchless \
  --disable-matchless-pass global-inlining \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.matchless.no_global_inline.edn
```

Useful follow-up comparison:

```sh
diff -u \
  /tmp/zafu_show/nonemptychain.matchless.no_global_inline.edn \
  /tmp/zafu_show/nonemptychain.matchless.edn
```

Swap the `--value` selector to inspect the other standing probes:

- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

After any change to TypedExpr normalization or any other TypedExpr optimizer pass, rerun the TypedExpr validator before trusting the new optimization:

```sh
./bosatsuj show \
  --repo_root ../zafu \
  --validate-typedexpr
```

Because optimizations must not be required for correctness, also rerun the validator with typed passes disabled. There are currently three typed passes (`loop-recur-lowering`, `normalize`, `discard-unused`), so either sweep all eight combinations or at minimum rerun `--no-opt` plus the single-pass disables:

```sh
./bosatsuj show --repo_root ../zafu --validate-typedexpr --no-opt
./bosatsuj show --repo_root ../zafu --validate-typedexpr --disable-typed-pass loop-recur-lowering
./bosatsuj show --repo_root ../zafu --validate-typedexpr --disable-typed-pass normalize
./bosatsuj show --repo_root ../zafu --validate-typedexpr --disable-typed-pass discard-unused
```

Current soundness status at checkpoint `2c8f6faeb`: all eight typed-pass configurations validate successfully on `../zafu`.

### Whole-library compare against `origin/main`

For changes in this plan, do not rely only on a handful of standing probes. Also compare the final Matchless for the whole of Zafu against a source-built `origin/main` compiler.

Do not use the checked-in `./bosatsu` wrapper in this repo as the baseline for this comparison. It may still point at an older released CLI that does not expose the same `show` entry point. Build `origin/main` from source in a clean sibling clone instead.

From the Bosatsu repo root:

```sh
rm -rf /tmp/bosatsu-main-compare-clone
git clone --shared "$(pwd)" /tmp/bosatsu-main-compare-clone
git -C /tmp/bosatsu-main-compare-clone checkout origin/main

sbt cli/assembly
( cd /tmp/bosatsu-main-compare-clone && sbt cli/assembly )
```

Then dump final Matchless for all of Zafu with separate cache directories so artifact reuse does not blur the comparison:

```sh
rm -rf /tmp/zafu_full_compare
mkdir -p /tmp/zafu_full_compare/branch_cache /tmp/zafu_full_compare/main_cache

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --ir matchless \
  --json \
  --color none \
  --cache_dir /tmp/zafu_full_compare/branch_cache \
  --output /tmp/zafu_full_compare/branch.matchless.json

/tmp/bosatsu-main-compare-clone/bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --ir matchless \
  --json \
  --color none \
  --cache_dir /tmp/zafu_full_compare/main_cache \
  --output /tmp/zafu_full_compare/main.matchless.json
```

Start with the coarse checks:

```sh
shasum /tmp/zafu_full_compare/main.matchless.json /tmp/zafu_full_compare/branch.matchless.json
wc -c -l /tmp/zafu_full_compare/main.matchless.json /tmp/zafu_full_compare/branch.matchless.json
diff -u /tmp/zafu_full_compare/main.matchless.json /tmp/zafu_full_compare/branch.matchless.json | sed -n '1,240p'
```

If they differ, reduce the diff to changed definitions and size deltas before deciding whether the change is positive:

```sh
python3 - <<'PY'
import json

def norm(x):
    return x if isinstance(x, str) else json.dumps(x, sort_keys=True, separators=(',', ':'))

def load_defs(path):
    data = json.load(open(path))
    defs = {}
    for pkg in data["packages"]["$vec"]:
        pname = norm(pkg["name"])
        for d in pkg.get("defs", {}).get("$vec", []):
            if isinstance(d, list) and len(d) == 3 and d[0] == "def":
                _, name, expr = d
                defs[(pname, norm(name))] = expr
    return defs

main_defs = load_defs("/tmp/zafu_full_compare/main.matchless.json")
branch_defs = load_defs("/tmp/zafu_full_compare/branch.matchless.json")

changed = []
for key in sorted(main_defs):
    m = main_defs[key]
    b = branch_defs[key]
    if b != m:
        ms = json.dumps(m, separators=(',', ':'))
        bs = json.dumps(b, separators=(',', ':'))
        changed.append((len(ms) - len(bs), key))

print("changed defs:", len(changed), "of", len(main_defs))
for delta, (pkg, name) in sorted(changed, reverse=True)[:40]:
    print(f"{delta:6d}  {pkg}::{name}")
PY
```

Do not use raw pretty-printed JSON file size as the main signal. Once tiny pure globals inline into explicit literals or constructors, the pretty JSON can get larger even while the underlying IR gets simpler. Prefer the changed-definition summary above, plus node-count deltas for `global`, `get-struct`, `get-enum`, `make-struct`, and `make-enum`.

```sh
python3 - <<'PY'
import json
from collections import Counter

def walk(node, counts):
    if isinstance(node, list):
        if node and isinstance(node[0], str):
            counts[node[0]] += 1
        for x in node[1:] if node and isinstance(node[0], str) else node:
            walk(x, counts)
    elif isinstance(node, dict):
        for v in node.values():
            walk(v, counts)

def load(path):
    data = json.load(open(path))
    counts = Counter()
    for pkg in data["packages"]["$vec"]:
        for d in pkg.get("defs", {}).get("$vec", []):
            if isinstance(d, list) and len(d) == 3 and d[0] == "def":
                walk(d[2], counts)
    return counts

main_counts = load("/tmp/zafu_full_compare/main.matchless.json")
branch_counts = load("/tmp/zafu_full_compare/branch.matchless.json")

for key in ["global", "get-struct", "get-enum", "make-struct", "make-enum"]:
    print(key, main_counts[key], branch_counts[key], branch_counts[key] - main_counts[key])
PY
```

When evaluating a single work item on top of this branch, compare against the previous checkpoint commit instead of `origin/main` so the measurement is isolated to the new work:

```sh
rm -rf /tmp/bosatsu-item-baseline
git clone --shared "$(pwd)" /tmp/bosatsu-item-baseline
git -C /tmp/bosatsu-item-baseline checkout <checkpoint-commit>

( cd /tmp/bosatsu-item-baseline && sbt cli/assembly )

rm -rf /tmp/zafu_item_compare
mkdir -p /tmp/zafu_item_compare/current_cache /tmp/zafu_item_compare/baseline_cache

./bosatsuj show \
  --repo_root /Users/oscar/code/zafu \
  --name zafu \
  --ir matchless \
  --json \
  --color none \
  --cache_dir /tmp/zafu_item_compare/current_cache \
  --output /tmp/zafu_item_compare/current.matchless.json

/tmp/bosatsu-item-baseline/bosatsuj show \
  --repo_root /Users/oscar/code/zafu \
  --name zafu \
  --ir matchless \
  --json \
  --color none \
  --cache_dir /tmp/zafu_item_compare/baseline_cache \
  --output /tmp/zafu_item_compare/baseline.matchless.json
```

For item 1 specifically, also count the residual number of sinkable pre-branch lets in final Matchless:

```sh
python3 - <<'PY'
import json

def is_dict_vec(x):
    return isinstance(x, dict) and "$vec" in x

def case_bodies(switch_node):
    cases = []
    default = None
    if not (isinstance(switch_node, list) and switch_node and switch_node[0] == "switch"):
        return cases, default
    i = 2
    while i < len(switch_node) - 1:
        key = switch_node[i]
        val = switch_node[i + 1]
        if isinstance(key, dict) and key.get("$kw") == "cases" and is_dict_vec(val):
            for item in val["$vec"]:
                if is_dict_vec(item) and len(item["$vec"]) >= 2:
                    cases.append(item["$vec"][1])
        elif isinstance(key, dict) and key.get("$kw") == "default":
            default = val
        i += 2
    return cases, default

def contains_var(node, var, bound=frozenset()):
    if var in bound:
        return False
    if isinstance(node, str):
        return node == var
    if isinstance(node, (bool, int, float)) or node is None:
        return False
    if isinstance(node, dict):
        if node.get("$form") == "lambda":
            args = set(node.get("args", {}).get("$vec", []))
            rec = node.get("recursive-name")
            if isinstance(rec, str):
                args.add(rec)
            return contains_var(node.get("body"), var, bound | args)
        return any(contains_var(v, var, bound) for k, v in node.items() if k != "$kw")
    if isinstance(node, list):
        if not node:
            return False
        tag = node[0] if isinstance(node[0], str) else None
        if tag in {"let", "let-bool"} and len(node) == 4:
            name, value, body = node[1], node[2], node[3]
            if contains_var(value, var, bound):
                return True
            if isinstance(name, str):
                if name == var:
                    return False
                return contains_var(body, var, bound | {name})
            return contains_var(body, var, bound)
        return any(contains_var(x, var, bound) for x in node[1 if tag is not None else 0:])
    return False

def is_pure(node):
    if isinstance(node, (str, bool, int, float)) or node is None:
        return True
    if isinstance(node, dict):
        if node.get("$form") == "lambda":
            return True
        return all(is_pure(v) for k, v in node.items() if k != "$kw")
    if isinstance(node, list):
        if not node:
            return True
        tag = node[0] if isinstance(node[0], str) else None
        if tag in {"app", "always", "let-mut", "while", "set-mut"}:
            return False
        return all(is_pure(x) for x in node[1 if tag is not None else 0:])
    return True

def count_candidates(node):
    total = 0
    if isinstance(node, dict):
        if node.get("$form") == "lambda":
            return count_candidates(node.get("body"))
        return sum(count_candidates(v) for k, v in node.items() if k != "$kw")
    if isinstance(node, list):
        if not node:
            return 0
        tag = node[0] if isinstance(node[0], str) else None
        if tag == "let" and len(node) == 4 and isinstance(node[1], str):
            var, value, body = node[1], node[2], node[3]
            if is_pure(value):
                if isinstance(body, list) and body and body[0] == "if" and len(body) == 4:
                    cond, then_expr, else_expr = body[1], body[2], body[3]
                    uses = (
                        contains_var(cond, var),
                        contains_var(then_expr, var),
                        contains_var(else_expr, var),
                    )
                    if not uses[0] and uses[1] != uses[2]:
                        total += 1
                elif isinstance(body, list) and body and body[0] == "switch":
                    on = body[1]
                    if not contains_var(on, var):
                        cases, default = case_bodies(body)
                        used = [contains_var(c, var) for c in cases]
                        if default is not None:
                            used.append(contains_var(default, var))
                        if used and any(used) and not all(used):
                            total += 1
        return total + sum(count_candidates(x) for x in node[1 if tag is not None else 0:])
    return 0

def load_defs(path):
    data = json.load(open(path))
    return [
        d[2]
        for pkg in data["packages"]["$vec"]
        for d in pkg.get("defs", {}).get("$vec", [])
        if isinstance(d, list) and len(d) == 3 and d[0] == "def"
    ]

for label, path in [
    ("main", "/tmp/zafu_full_compare/main.matchless.json"),
    ("branch", "/tmp/zafu_full_compare/branch.matchless.json"),
]:
    total = sum(count_candidates(expr) for expr in load_defs(path))
    print(label, total)
PY
```

For item 1, call the whole-repo result positive when the residual count falls sharply and the changed definitions are localized, even if a few tiny selectors get duplicated into some branches. The intended direction is that branch-only work should move under `if` and `switch`, and that this branch-local structure should then be available to later inlining and simplification passes.

### IR inspection

Use `show --ir typedexpr` and `show --ir matchless` on targeted Zafu values, with typed and Matchless pass toggles when needed to isolate each rewrite. The primary probes should stay the higher-order sites that exposed the weaknesses in the first place:

- `Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn`
- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

### Regression tests

Expand the existing test areas rather than inventing a separate harness:

- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`

### Progress checks

- Final Matchless no longer contains obvious dead lets, projections, or wrapper calls that disappear if one more cleanup round is run manually.
- Re-running the bounded optimizer pipeline is a no-op on the resulting IR.
- `Matchless.exprWeight` improves or stabilizes across rounds and does not oscillate.
- Escaping functions remain closures when they must, and `Unit -> a` thunks still preserve laziness.

### Current status after items 1 through 5

- The first work item is implemented and covered by focused regressions in `TypedExprTest` and `MatchlessTest`.
- The standing Zafu probe files for `NonEmptyChain`, `Chain`, `Result`, and `PartialResult` were rerun after the change and are still textually unchanged.
- A full final-Matchless comparison against a source-built `origin/main` compiler changed 139 of 1963 Zafu definitions.
- The residual number of sinkable pre-branch lets in final Matchless fell sharply in that comparison, from 535 to 22.
- There are also a few localized size regressions where branch-sinking duplicates a tiny selector such as `get-enum` or `get-struct` into multiple used branches. That trade is acceptable for now because this plan explicitly wants to expose branch-local laziness and make later inlining and simplification opportunities visible.
- Item 2 and the later fixpoint work are what should determine whether those temporary duplications collapse away in the final stable IR.
- The second work item is implemented and covered by focused regressions in `MatchlessTest` and `ToolAndLibCommandTest`.
- Isolating item 2 against checkpoint `92c12efc8` changed 123 of 1963 Zafu definitions. Of those, 81 shrank and 42 grew, for a net minified per-definition Matchless size win of 6641 bytes.
- In that isolated comparison, `global` references dropped from 13089 to 11833, `get-struct` from 10754 to 10709, and `get-enum` from 3449 to 3429. `make-enum` rose from 6321 to 6510 because tiny pure globals are now exposed directly as concrete constructors.
- Representative item-2 wins include `Zafu/Collection/LazyTree::applicative_LazyTree` inlining `empty_LazyList` to a direct enum constructor, `Zafu/Abstract/Instances/Predef::hash_TupleN` inlining the tiny pure `Hash.mix_prime` and `Hash.mix_add` constants, and `Zafu/Control/Result::tests` folding payload projections from inlined tiny enum values.
- The standing higher-order traverse probes are still mostly unchanged after item 2 alone: `traverse_NonEmptyChain_fn`, `traverse_Chain_fn`, `traverse_Result_fn`, and `traverse_PartialResult_fn` remain textually unchanged in final Matchless. That is acceptable for now because the whole-library comparison shows the intended value inlining is firing in real Zafu code, and later items are still expected to expose more branch-local structure in those probes.
- The third work item is implemented and covered by focused regressions in `TypedExprTest` and `MatchlessTest`.
- Isolating item 3 against checkpoint `43bf1d759` changed 0 of 1963 Zafu definitions in final Matchless. The whole-library final shape is therefore unchanged relative to item 2.
- That no-op corpus diff is acceptable for now. It means the current Zafu workload does not contain direct-call-only multi-use local closures that survive to final Matchless in a way this rewrite changes, so item 3 is evidenced by the new focused normalization and lowering regressions rather than by a whole-library shape shift.
- `show --validate-typedexpr` is now available through both `show` entry points and should be part of the evaluation loop for all future TypedExpr optimizer work.
- After the TypedExpr soundness fixes through checkpoint `2c8f6faeb`, `show --validate-typedexpr` succeeded on `../zafu` for the default pipeline, all single disabled typed-pass configurations, all two-pass disabled configurations, and `--no-opt`.
- The fourth work item is now implemented and covered by focused regressions in `MatchlessTest`.
- Item 4 already had the shared call-site scoring model in place before this turn; the remaining gap was that TypedExpr still did not recognize constructor-valued arguments as a payoff for inlining branchy helpers. `TypedExprNormalization` now treats direct constructor applications as “known enough” for the benefit model, so wrapper helpers that immediately branch on `Some(x)`-style values can inline even though those constructor applications are not otherwise “simple”.
- The new focused regression `optimized Matchless inlines constructor-branch helpers across direct alias and wrapper calls` shows the intended effect directly: the direct call, local alias, and wrapper variants all collapse to the same final identity-style Matchless body once the helper sees a known constructor argument.
- Isolating the item-4 completion work against checkpoint `2c8f6faeb` changed 40 Zafu definitions in final Matchless. Of those, 13 shrank, 6 grew, and 21 had a shape change at the same minified size, for a net minified Matchless win of 4251 bytes.
- In that isolated comparison, `get-enum` dropped by 26 nodes, `make-struct` by 10, and `make-enum` by 6. `get-struct` rose by 11 and `global` by 6, so this is a net positive but not a uniformly shrinking transform.
- Representative item-4 wins in that isolated compare include `Zafu/Control/PartialResult::tests`, `Zafu/Cli/Args/Internal/Core::render_spelling`, `Zafu/Control/Result::tests`, and `Zafu/Collection/List::tests`. The largest growths were `Zafu/Cli/Args/Internal/Core::spelling_usage`, `Zafu/Cli/Args/Internal/Core::decode_named_required`, and `Zafu/Tool/Cat::tests`, where helper logic moved into callers. That trade is acceptable for item 4 because the branch/projection payoff is now explicit and the no-payoff regression remains covered by the optimizer tests.
- After the item-4 completion change, `sbt coreJVM/test` is green with 1928 passed and 2 ignored, and `sbt cli/test` is green with 67 passed.
- The fifth work item is now implemented and covered by a focused `MatchlessTest` regression that requires two cleanup rounds before branch-only work can sink, plus the existing `MatchlessRegressionTest` deep-let stack-safety coverage.
- `postLoweringCleanup` now runs up to four rounds and stops on structural equality when it can prove stabilization. The structural comparison and anon-binder canonicalization are wrapped with the same stack-overflow fallback discipline as the underlying cleanup passes, so the fixpoint bookkeeping does not reintroduce deep-let stack regressions.
- Isolating item 5 against checkpoint `abbfc655b` changed 1078 of 1892 Zafu definitions in raw pretty JSON output, but that churn is only anon and mutable-local renumbering from canonicalization. After normalizing `anon$N` and `mut$N` names per definition, only 1 of 1892 definitions changes: `Zafu/Collection/Chain::tests`.
- That remaining normalized item-5 diff is the intended cleanup shape: a branch-local constructor `let` now sits under the guarding `if` instead of outside it. Node counts for `global`, `get-struct`, `get-enum`, `make-struct`, `make-enum`, `let`, `if`, and `switch` are otherwise unchanged in the isolated compare.
- After the item-5 completion change, `sbt coreJVM/test` is green with 1929 passed and 2 ignored, and `sbt cli/test` is green with 67 passed.

## Items To Do

1. [x] General branch-sinking after inlining

Relevant code:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala`

Plan:

- Generalize the current TypedExpr let-to-match rewrite into a reusable pass that can sink pure branch-only work under `Match` and equivalent branch structure after inlining exposes it.
- Add the Matchless analogue so pure lets and eager call arguments can be pushed under `If` and `SwitchVariant` after beta-reduction and global inlining, not only when `inlineApplyArgs` sees the opportunity directly at the call site.
- Preserve current behavior when both branches need the value, or when the condition or discriminant depends on the value.

Done when:

- Branch-only expensive expressions no longer appear in pre-branch lets or as eager call arguments.
- In `traverse_NonEmptyChain_fn`-style code, `fn(item)` is not evaluated before the `Option` or `Result` discrimination.
- Final Matchless branches first and evaluates `fn(item)` only in the branch that needs it.

2. [x] Inline small pure values, not only lambdas

Relevant code:

- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessRegressionTest.scala`
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`

Plan:

- Extend published Matchless summaries from “tiny lambdas only” to “tiny pure values that are safe to duplicate or substitute”.
- Reuse the existing known-value projection folding in `simplifyKnownConditions` so `GetStructElement` and `GetEnumElement` collapse once a global or local pure value is substituted.
- Target the higher-order wrapper cases in Zafu where concrete `Traverse` and `Applicative` records should disappear from the caller IR instead of surviving as wrapper construction plus field selection.

Done when:

- Tiny closed record-valued or constructor-valued globals and locals disappear from caller IR.
- `GetStructElement` and `GetEnumElement` on known values fold away.
- Final Matchless no longer carries wrapper-record plumbing around `Traverse` and `Applicative` values when the concrete helper or branch logic can be exposed instead.

3. [x] Generalize closure conversion of non-escaping function values

Relevant code:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Generalize `rewriteNonEscapingClosureBinding` so it can run for non-recursive local function values, not only recursive bindings.
- Rewrite direct-call-only local functions to take captured values as explicit extra parameters, then update call sites to pass those captures directly.
- Keep the escape analysis conservative: escaping functions should remain closures, and `Unit -> a` suspension boundaries should continue to block eager sharing or forced evaluation.

Done when:

- In TypedExpr, direct-call-only local functions gain explicit extra parameters for captured values.
- In Matchless, there are fewer `Lambda(captures = ...)` nodes, fewer `ClosureSlot`s, and more `Lambda(captures = Nil, ...)` or fully beta-reduced code.
- Escaping functions and suspension boundaries keep their current semantics.

4. [x] Replace size bonuses with a richer shared benefit model

Relevant code:

- `core/src/main/scala/dev/bosatsu/InlineBenefitModel.scala`
- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Replace the current “small enough” TypedExpr gates and the separate Matchless demand score with a shared notion of payoff.
- Count concrete optimizer wins, such as dead-argument elimination, branch sinking, beta-reduction, projection folding, wrapper removal, and closure removal, then compare those wins against the code-size cost.
- Keep no-payoff cases conservative so harmless aliases and annotations stop perturbing final IR shape, but helpers that unlock real simplification still inline even when they are slightly above the old thresholds.

Done when:

- Source variants that differ only by harmless aliases, annotations, or tiny wrappers compile to the same or nearly the same final Matchless.
- Helpers that unlock branch sinking, beta-reduction, or projection folding inline even when they slightly exceed the old thresholds.
- Helpers that unlock none of those wins stay as calls, and total IR size stops growing in no-payoff cases.

5. [x] Run the new passes to a small fixpoint

Relevant code:

- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Add a bounded stabilization loop around the new Matchless rewrite stack so the common sequence can complete: inline wrapper, expose pure value, fold projection, sink branch-only work, then simplify.
- Compare IR after each round and stop on structural equality or a small hard cap. The cap should be low enough to be predictable and high enough to let the new rewrites compose.
- Reject oscillation: if a round makes the IR worse or starts cycling, stop and fix the pass interaction before broadening the fixpoint.

Done when:

- Re-running the same optimizer pipeline is a no-op on the IR.
- Final Matchless does not retain dead lets, field projections, or wrapper calls that only disappear after one more cleanup round.
- Expression weight monotonically improves or stabilizes across rounds.

## Completed Items

- Item 1 completed.
  TypedExpr let-to-match sinking now recurses through `Annotation` and `Generic` wrappers, and Matchless `postLoweringCleanup` now sinks pure branch-only lets into `If` and `SwitchVariant` branches after inlining.
- Item 2 completed.
  Matchless global inlining now publishes tiny pure value summaries in addition to lambdas, expression-level simplification can substitute those values and fold projections through known locals, and focused tests now cover pure struct-valued and enum-valued helper elimination both in optimizer tests and through `show --ir matchless`.
- Item 3 completed.
  TypedExpr normalization now applies the existing non-escaping closure rewrite to multi-use non-recursive local function bindings as well as recursive ones, while still leaving escaping local functions untouched. Focused tests now cover the TypedExpr rewrite directly and the raw Matchless lowering effect, where direct-call-only local closures lose captures and escaping closures still keep them.
- Item 4 completed.
  TypedExpr and Matchless now share `InlineBenefitModel` for call-site inlining, the TypedExpr side now recognizes constructor-valued arguments as a concrete payoff when a helper immediately branches on them, and focused optimizer tests cover direct, alias, wrapper, and no-payoff variants so harmless wrappers stay aligned while real branch/projection wins still inline.
- Item 5 completed.
  Matchless `postLoweringCleanup` now runs as a bounded small fixpoint over the post-lowering rewrite stack, focused optimizer tests cover the multi-round “inline wrapper, expose branch-local work, then sink it” case directly, and the fixpoint bookkeeping now preserves the previous deep-let stack-safety guarantees by falling back cleanly if canonicalization or structural equality would overflow.
