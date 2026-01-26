{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Scala/JVM toolchain
    sbt
    openjdk17
    scala_3

    # For C codegen testing (clang backend)
    clang
    boehmgc
    pkg-config

    # For Python codegen testing
    python311
    python311Packages.pip

    # For Scala.js testing (Node.js environment)
    nodejs_20

    # Development tools
    git
    gnupg
    gh  # GitHub CLI

    # Code quality
    scalafmt
  ];

  shellHook = ''
    export JAVA_HOME=${pkgs.openjdk17}
    export PATH=$JAVA_HOME/bin:$PATH

    # For C codegen tests
    export CC=${pkgs.clang}/bin/clang
    export PKG_CONFIG_PATH=${pkgs.boehmgc}/lib/pkgconfig:$PKG_CONFIG_PATH

    echo ""
    echo "=== Bosatsu Development Environment ==="
    echo "Java:   $(java -version 2>&1 | head -1)"
    echo "Scala:  $(scala -version 2>&1)"
    echo "sbt:    $(sbt --version 2>&1 | tail -1)"
    echo "Node:   $(node --version)"
    echo "Python: $(python3 --version)"
    echo "Clang:  $(clang --version | head -1)"
    echo ""
    echo "Commands:"
    echo "  sbt compile         - Compile all modules"
    echo "  sbt test            - Run all tests (with coverage)"
    echo "  sbt testPY          - Run Python codegen tests"
    echo "  sbt testC           - Run C codegen tests"
    echo "  sbt 'testOnly *Foo' - Run specific test"
    echo ""
    echo "Coverage:"
    echo "  sbt coverage test   - Run tests with coverage"
    echo "  sbt coverageReport  - Generate coverage report"
    echo ""
    echo "Testing strategy: Complete coverage + property-based testing"
    echo "  - MUnit for unit tests"
    echo "  - ScalaCheck for property-based tests"
    echo "  - All generators in core/src/test/scala/dev/bosatsu/Gen.scala"
    echo ""
  '';
}
