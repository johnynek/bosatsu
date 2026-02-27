# Release process

This repo publishes releases when a `v*` tag is pushed. The GitHub Actions workflow builds and uploads all assets (jar, node JS, native images, c_runtime archive, bosatsu libs, `TypedAst.proto`, `SHA256SUMS`, `B3SUMS`), then commits updated `*_conf.json` files back to `main`.

## Create a release

1) Make sure `main` is clean and updated locally.
2) The project version is derived from the git tag via `sbt-dynver`. No version edit in `build.sbt` is required.
   - `versionString` in `build.sbt` is the Scala version. Only change this if you are upgrading Scala.
3) Tag and push:

```sh
git tag v0.0.7

git push origin v0.0.7
```

That is it. The `Release` workflow runs and publishes the assets.
It also verifies that the `sbt-dynver` version matches the tag.

## Find the latest release

- GitHub UI: open the Releases page for this repo.
- Using `gh`:

```sh
gh release list --limit 1
```

- Using only `git`:

```sh
git tag --list 'v*' --sort=-v:refname | head -n 1
```

If you want the latest tag on the remote without the web UI, you can also do:

```sh
git ls-remote --tags origin 'v*' | awk -F/ '{print $3}' | sort -V | tail -n 1
```
