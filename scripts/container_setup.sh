#!/usr/bin/env bash
# MergeXO executes this standalone script as root before the repository checkout.
set -euo pipefail

case "$(uname -s)/$(uname -m)" in
  Linux/x86_64)
    arch=x64
    jdk_sha=8b9b611d917e3dfd8419d3566b5949226f69b052fed7e109c1368add1dfe8711
    ;;
  Linux/aarch64)
    arch=aarch64
    jdk_sha=3e4eca7ead088f080af1476007decf010a240a1eb4e1e78c54fa32fbcc347084
    ;;
  *) echo "Container setup supports Linux amd64/arm64" >&2; exit 1 ;;
esac

staging_dir="$(mktemp -d)"
trap 'rm -rf "$staging_dir"' EXIT

# Match CI's Temurin 17 JDK; pin the archive and checksum for reproducibility.
curl --fail --location --retry 3 --output "$staging_dir/jdk.tar.gz" \
  "https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.21%2B9/OpenJDK17U-jdk_${arch}_linux_hotspot_17.0.21_9.tar.gz"
printf '%s  %s\n' "$jdk_sha" "$staging_dir/jdk.tar.gz" | sha256sum --check --status
mkdir -p /opt/mergexo-jdk
tar -xzf "$staging_dir/jdk.tar.gz" -C /opt/mergexo-jdk --strip-components=1
# Exports in setup cannot change the parent worker: install commands on its PATH.
for binary in /opt/mergexo-jdk/bin/*; do
  ln -sf "$binary" "/usr/local/bin/${binary##*/}"
done
java -version

# Official launcher; the checked-out project/build.properties selects sbt.
curl --fail --location --retry 3 --output "$staging_dir/sbt.tgz" \
  "https://github.com/sbt/sbt/releases/download/v1.13.1/sbt-1.13.1.tgz"
printf '%s  %s\n' 4a20e36c1642b6d5be34ef641d2ed6c8c0d464e6145283730d5566716e4083be "$staging_dir/sbt.tgz" | sha256sum --check --status
mkdir -p /opt/mergexo-sbt
tar -xzf "$staging_dir/sbt.tgz" -C /opt/mergexo-sbt --strip-components=1
ln -sf /opt/mergexo-sbt/bin/sbt /usr/local/bin/sbt
