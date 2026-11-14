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
from Common/Util import common_value
export public_value
public_value = common_value
```

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
along with the public and private child packages.

Regenerate the serialized artifacts and both pin files from the typed fixture
builder with:

```sh
sbt "coreJVM/Test/runMain dev.bosatsu.library.LibraryInspectionFixtures --write"
```
