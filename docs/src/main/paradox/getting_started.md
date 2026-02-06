# Getting started (new repo)

This guide shows how to start a brand new Bosatsu repository using the `bosatsu`
launcher script and a pinned CLI release. It assumes you have `git` and `curl`
installed. For the CLI runtime, pick one of:

- `native` (recommended on macOS/Linux)
- `java` (requires a JVM)
- `node` (requires Node.js)

## 1) Create a new git repo

```sh
mkdir my-bosatsu-project
cd my-bosatsu-project
git init
```

## 2) Add the `bosatsu` launcher

Download the launcher script and make it executable:

```sh
curl -L -o bosatsu https://raw.githubusercontent.com/johnynek/bosatsu/main/bosatsu
chmod +x bosatsu
```

## 3) Find the latest release version

Open the latest release page and note the version:

```
https://github.com/johnynek/bosatsu/releases/latest/
```

Example of a direct asset URL from a release:

```
https://github.com/johnynek/bosatsu/releases/download/v0.0.25/bosatsu
```

(The exact asset filename varies by platform; the launcher handles this for
you.)

## 4) Pin the Bosatsu release version

Create `.bosatsu_version` with the latest Bosatsu release tag (without the
leading `v`). For example, if the latest release is `v0.0.25`, write `0.0.25`:

```sh
echo '0.0.25' > .bosatsu_version
```

## 5) Choose a platform (per-machine)

Create `.bosatsu_platform` with one of `native`, `java`, or `node`. Start with
`native` on macOS/Linux.

```sh
echo 'native' > .bosatsu_platform
```

If you are on Windows, use `java` or `node` instead of `native`.

## 6) Ignore per-machine/cache files

You generally want `.bosatsu_platform` to be per-machine, and the download cache
should stay out of git. Add both to `.gitignore`:

```sh
cat >> .gitignore <<'EOF'
.bosatsu_platform
.bosatsuc/
EOF
```

Commit `.bosatsu_version` so everyone uses the same CLI release.

## 7) Fetch the CLI

Download the release artifact for your platform:

```sh
./bosatsu --fetch
```

This will download into `.bosatsuc/cli/<version>/` based on the value in
`.bosatsu_version` and your chosen platform.

## 8) Install the C runtime

```sh
./bosatsu c-runtime install
```

## 9) Initialize a library config

Pick a source root for your Bosatsu packages (e.g. `src`) and initialize the
library config:

```sh
mkdir -p src
./bosatsu lib init \
  --name mylib \
  --repo_uri <your repo URL> \
  --src_root src \
  --version 0.1.0
```

Notes:

- `lib init` searches for `.git` to find the repo root. If you need to override
  that, add `--repo_root <path>`.
- It writes `bosatsu_libs.json` in the repo root and `src/mylib_conf.json` in the
  source root you selected.

## 10) Add code

Put your `.bosatsu` files under `src/`. For example, create
`src/MyLib/Hello.bosatsu` with a type, a function, and a test:

```bosatsu
package MyLib/Hello

enum Mood: Happy, Sad

def greet(m: Mood) -> String:
  match m:
    case Happy: "hello"
    case Sad: "cheer up"

test = Assertion(greet(Happy) matches "hello", "greet Happy")
```

Use `./bosatsu --help` to explore the available commands (`lib`, `tool`,
`c-runtime`, etc.). Most language-level operations now live under `tool`, for
example: `./bosatsu tool check`, `./bosatsu tool eval`, and
`./bosatsu tool transpile`.

## 11) Run tests

From the repo root:

```sh
./bosatsu lib test
```
