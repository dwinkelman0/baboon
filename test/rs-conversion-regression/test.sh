#!/usr/bin/env bash
set -euo pipefail

fixture_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
test_dir="$(mktemp -d -t baboon-rust-conversions-XXXXXXXX)"
printf 'Rust conversion test output: %s\n' "$test_dir"
"$@" --model-dir "$fixture_dir/models" :rust --output "$test_dir/src" \
  --generate-ueba-codecs=true --generate-ueba-codecs-by-default=true
cp "$fixture_dir/Cargo.toml" "$test_dir/Cargo.toml"
cp -r "$fixture_dir/tests" "$test_dir/tests"
cargo test --manifest-path "$test_dir/Cargo.toml" --test conversions
# Bazel path (cargo logic above retained for local runs):
#   bazel test //test/rs-conversion-regression:test_tests_conversions
