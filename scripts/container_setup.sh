#!/usr/bin/env bash
# MergeXO executes this standalone script as root before the repository checkout.
set -euo pipefail

case "$(uname -s)/$(uname -m)" in
  Linux/x86_64)
    arch=x64
    jdk_sha=8b9b611d917e3dfd8419d3566b5949226f69b052fed7e109c1368add1dfe8711
    node_arch=x64
    node_sha=4f2b175a6d53a5c617a80dba2676238a0688cef28c6da4eba35c7d844e66fcfb
    ;;
  Linux/aarch64)
    arch=aarch64
    jdk_sha=3e4eca7ead088f080af1476007decf010a240a1eb4e1e78c54fa32fbcc347084
    node_arch=arm64
    node_sha=6e51a2ada00f0011cc266f65d98922b403d7ad88f809ca2aa615a5fe1ce6e86c
    ;;
  *) echo "Container setup supports Linux amd64/arm64" >&2; exit 1 ;;
esac

staging_dir="$(mktemp -d)"
trap 'rm -rf "$staging_dir"' EXIT

# Tools the GitHub workflows get from the Ubuntu runner image or install in
# their own steps: cmake builds the C runtime's vendored libuv/bdwgc; protoc,
# valgrind, b3sum and musl-tools back the test, testC-valgrind and release
# jobs; nc is the scripts/bin/nc fallback; sudo lets workflow commands run as
# root. zlib1g-dev and build-essential are what GraalVM native-image links with.
export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y --no-install-recommends \
  b3sum build-essential ca-certificates cmake curl file git musl-tools \
  netcat-openbsd pkg-config protobuf-compiler python3 sudo unzip valgrind \
  xz-utils zlib1g-dev
rm -rf /var/lib/apt/lists/*

# GCC 12's ASan runtime keeps its heap at a fixed 0x600000000000. Hosts with 32 bits
# of mmap ASLR (vm.mmap_rnd_bits=32, Ubuntu's kernel default; GitHub runners use 28)
# load a PIE executable over that range about one run in five, and ASan then prints
# AddressSanitizer:DEADLYSIGNAL forever. Containers can neither lower the entropy nor
# disable ASLR (seccomp blocks personality), so link ASan executables as non-PIE.
cat > /opt/mergexo-gcc-launcher <<'EOF'
#!/bin/sh
asan=
for arg do
  case $arg in
    -shared|-r|-pie|-static-pie) exec "/usr/bin/${0##*/}" "$@" ;;
    -fsanitize=*address*) asan=1 ;;
  esac
done
[ -z "$asan" ] || set -- "$@" -no-pie
exec "/usr/bin/${0##*/}" "$@"
EOF
chmod 755 /opt/mergexo-gcc-launcher
ln -sf /opt/mergexo-gcc-launcher /usr/local/bin/cc
ln -sf /opt/mergexo-gcc-launcher /usr/local/bin/gcc
printf 'int main(void) { return 0; }\n' > "$staging_dir/asan.c"
cc -fsanitize=address,undefined "$staging_dir/asan.c" -o "$staging_dir/asan"
for _ in $(seq 20); do
  timeout 10 "$staging_dir/asan" > /dev/null 2>&1 ||
    { echo "AddressSanitizer executables crash on this host" >&2; exit 1; }
done

# Match CI's Temurin 17 JDK; pin the archive and checksum for reproducibility.
curl --fail --location --retry 3 --output "$staging_dir/jdk.tar.gz" \
  "https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.21%2B9/OpenJDK17U-jdk_${arch}_linux_hotspot_17.0.21_9.tar.gz"
printf '%s  %s\n' "$jdk_sha" "$staging_dir/jdk.tar.gz" | sha256sum --check --status
mkdir -p /opt/mergexo-jdk
tar -xzf "$staging_dir/jdk.tar.gz" -C /opt/mergexo-jdk --strip-components=1
# Exports in setup cannot change the parent worker: install commands on its PATH.
# Workers start without a locale, so the JDK would encode file names as ASCII and
# reject non-ASCII paths. Use CI's C.UTF-8 unless a UTF-8 locale is already set.
cat > /opt/mergexo-jdk-launcher <<'EOF'
#!/bin/sh
[ "$(locale charmap 2>/dev/null)" = UTF-8 ] || { LC_ALL=C.UTF-8; export LC_ALL; }
exec "/opt/mergexo-jdk/bin/${0##*/}" "$@"
EOF
chmod 755 /opt/mergexo-jdk-launcher
for binary in /opt/mergexo-jdk/bin/*; do
  ln -sf /opt/mergexo-jdk-launcher "/usr/local/bin/${binary##*/}"
done
java -version
java -XshowSettings:properties -version 2>&1 | grep 'sun.jnu.encoding = UTF-8'

# Official launcher; the checked-out project/build.properties selects sbt.
curl --fail --location --retry 3 --output "$staging_dir/sbt.tgz" \
  "https://github.com/sbt/sbt/releases/download/v1.13.1/sbt-1.13.1.tgz"
printf '%s  %s\n' 4a20e36c1642b6d5be34ef641d2ed6c8c0d464e6145283730d5566716e4083be "$staging_dir/sbt.tgz" | sha256sum --check --status
mkdir -p /opt/mergexo-sbt
tar -xzf "$staging_dir/sbt.tgz" -C /opt/mergexo-sbt --strip-components=1
ln -sf /opt/mergexo-sbt/bin/sbt /usr/local/bin/sbt

# Scala.js tests and bundles (scalajs-bundler) run node and npm; pin an LTS release.
node_version=24.22.3
curl --fail --location --retry 3 --output "$staging_dir/node.tar.xz" \
  "https://nodejs.org/dist/v${node_version}/node-v${node_version}-linux-${node_arch}.tar.xz"
printf '%s  %s\n' "$node_sha" "$staging_dir/node.tar.xz" | sha256sum --check --status
mkdir -p /opt/mergexo-node
tar -xJf "$staging_dir/node.tar.xz" -C /opt/mergexo-node --strip-components=1
for binary in /opt/mergexo-node/bin/*; do
  ln -sf "$binary" "/usr/local/bin/${binary##*/}"
done
# Put `npm install -g` packages on PATH, as on CI runners.
npm config set --location=global prefix /usr/local
node --version
npm --version

# CI's tree-sitter grammar checks pin this CLI (scripts/test_tree_sitter.sh).
npm install --global --allow-scripts=tree-sitter-cli tree-sitter-cli@0.24.7
tree-sitter --version

# The static Linux native image (release.yml) links a musl-built zlib from here.
curl --fail --location --retry 3 --output "$staging_dir/zlib.tar.gz" \
  "https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz"
printf '%s  %s\n' 9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23 "$staging_dir/zlib.tar.gz" | sha256sum --check --status
mkdir "$staging_dir/zlib"
tar -xzf "$staging_dir/zlib.tar.gz" -C "$staging_dir/zlib" --strip-components=1
(
  cd "$staging_dir/zlib"
  CC=musl-gcc ./configure --static --prefix=/usr/local/musl
  make -j"$(nproc)"
  make install
)

cmake --version | head -n 1
protoc --version
valgrind --version
