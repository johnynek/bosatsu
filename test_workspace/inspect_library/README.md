# Pinned library inspection fixtures

These serialized typed libraries exercise a declared `Parser` graph. The graph
contains a public child and a private child that both depend on `common`, an
older unused-transitive-public `legacy` child, and two wrappers that select
incompatible private releases of `shared`.

`parser-root.json` pins the root artifact. `parser-context.json` declares every
dependency artifact by name, version, path, BLAKE3 and SHA-256. Library
descriptors retain BLAKE3-only dependency pins, as older released descriptors
do. The context contains both `shared` versions and all files are checked in so
later tests can run offline.

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

Preparation downloads
`https://github.com/johnynek/bosatsu/releases/download/v0.0.69/core_alpha-v9.0.1.bosatsu_lib`
and verifies its size (937080 bytes) and SHA-256
`b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff` before
staging it. The worker command tests use only the prepared local files. The
legacy BLAKE3 pin is
`098f5eff00611599b54b5e6bce8b7696c8fdf89f4e99f8bfbbec521764ed8927`.
