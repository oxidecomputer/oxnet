#!/bin/bash
#:
#: name = "build-and-test / illumos"
#: variety = "basic"
#: target = "helios"
#: rust_toolchain = "stable"
#:

set -o errexit
set -o pipefail
set -o xtrace

rustup show active-toolchain || rustup toolchain install

cargo --version
rustc --version

# The helios image does not ship nextest. Fetch the prebuilt binary rather
# than cargo install it, since compiling nextest would dominate a job whose
# test suite runs in under a second; omicron's buildomat jobs do the same.
NEXTEST_VERSION='0.9.143'
curl -sSfL --retry 10 "https://get.nexte.st/$NEXTEST_VERSION/illumos" | gunzip | tar -xf - -C ~/.cargo/bin

banner build
ptime -m cargo build --all-features --locked --all-targets --verbose

banner test
ptime -m cargo nextest run --all-features --locked

# nextest does not run doctests.
banner doctest
ptime -m cargo test --all-features --locked --doc
