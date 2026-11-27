# Pinned library inspection fixtures

These serialized typed libraries exercise a declared `Parser` graph. The graph
contains a public child and a private child that both depend on `common`, an
older unused-transitive-public `legacy` child, and two wrappers that select
incompatible private releases of `shared`. The `scoped-context.json` fixture
adds the caller-relative compile cases: `equal_left` and two `equal_right`
releases expose byte-identical `equal_api` interfaces from different
releases; `named_left` and `named_right` expose different package names from
two `named_api` releases. `private-core-context.json` carries a
`private_core_conflict` edge to an incompatible `bosatsu` core release.

`parser-root.json` pins the root artifact. `parser-context.json` declares every
dependency artifact by name, version, path, BLAKE3 and SHA-256. The scoped
contexts declare the serialized graphs used by the candidate worker. Library
descriptors retain BLAKE3-only dependency pins, as older released descriptors
do. All generated artifacts are checked in so scoped compilation and graph
validation can run offline.

Representative source snippets used by the builder:

```bosatsu
package Public/Child
from Common/Util import common_value, Box
export public_value, make_box
exposes Common/Util
public_value = common_value
make_box = Box
```

`Public/Child`'s exported `make_box` type refers to the `Common/Util::Box`
provider. This exercises typed exposure ownership through a public edge; the
serialized interface deliberately omits the newer exposure index to exercise
legacy typed-export recovery. The separate `Private/Child` use of `Common/Util`
remains an implementation detail.

```bosatsu
package Private/Child
from Common/Util import common_value
export private_value
private_value = common_value
```

```bosatsu
package Shared/Api
export pick
def pick(value: Int) -> Int: value
```

The second `Shared/Api` release instead accepts `Bool`. `Left` and `Right`
compile against their respective release; `Parser` imports both wrappers,
along with the public and private child packages. `Parser` also contains an
unexported `private_helper` value used to check that implementation edits change
artifact metadata without changing its exported signature bytes.

Regenerate the serialized artifacts and both pin files from the typed fixture
builder with:

```sh
sbt "coreJVM/Test/runMain dev.bosatsu.library.LibraryInspectionFixtures --write"
```

The candidate-jar worker acceptance suite also checks the real released
`core_alpha` 9.0.1 artifact. Prepare a local offline input directory before
running those checks:

```sh
python3 scripts/prepare_inspect_library_fixtures.py --out-dir build/inspection-fixtures
python3 scripts/test_bazel_worker.py \
  --jar <candidate-assembly-jar> \
  --inspection-fixtures build/inspection-fixtures
```

The same candidate suite compiles two members with disjoint private `shared`
releases in one batch, validates the Parser diamond, rejects visible releases
with equal signatures and with different exported package names, and rejects
the private-path core mismatch. It compares direct, `@params`, warm-worker,
cold-worker and restarted-worker results. A warm success followed by a scope
map edit at the same path must reject the conflicting provider, preserve the
published bundle, and recover after the original binding is restored. Reversed
source, interface and label order must preserve output bytes and diagnostics.
The equal-signature conflict must name both routes, through `equal_left` and
through `equal_right`. The suite reuses `left` and the two `named_api`
releases for three more checks. First it compiles a consumer against a
local signature that the candidate jar produced, together with pinned `left`,
under strict unused checking. Then it validates graphs with missing bindings.
Last it validates a source wrapper whose public exposure of `named_api` 1.0.0
conflicts with the consumer's direct `named_api` 2.0.0.

Preparation downloads
`https://github.com/johnynek/bosatsu/releases/download/v0.0.69/core_alpha-v9.0.1.bosatsu_lib`
and verifies its size (937080 bytes) and SHA-256
`b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff` before
staging it. The worker command tests use only the prepared local files. The
legacy BLAKE3 pin is
`098f5eff00611599b54b5e6bce8b7696c8fdf89f4e99f8bfbbec521764ed8927`.
