# Bazel build (primary; `mdl/` removed, sbt/nix are a deprecated fallback)

> `build.sbt`, `project/` and `flake.nix` are kept as a deprecated fallback
> and will be removed once all Bazel lanes are green. CI invokes Bazel
> directly (no `mdl`).

## Prerequisites

- Bazel `9.3.3` (pinned in `.bazelversion`) with Bzlmod enabled (see `.bazelrc`).
- Toolchains (all downloaded, never hardcoded local paths):
  - Scala `2.13.18` via `rules_scala`
  - Python `3.12` via `rules_python` (pip parses `test/py-stub/requirements.txt`)
  - Rust `1.90.0` / edition `2021` via `rules_rust` (`crate_universe` reads `test/rs-stub/Cargo.toml`; toolchain registered via `@rust_toolchains//:all`).
    The crates' `rust-version = "1.75"` is an MSRV floor; 1.90 builds current
    transitive deps (e.g. getrandom 0.4.x needs edition 2024) and links
    working Mach-O binaries with the Xcode 27 linker.
  - Java `21` via `remotejdk_21` (`java_runtime_version` + `tool_java_runtime_version` in `.bazelrc`; GraalVM CE 23 download deferred — see TODO in `MODULE.bazel`).

## Commands

```bash
# Full build:
bazel build //...

# Scala compiler + binary:
bazel build //baboon-compiler/...
bazel run //baboon-compiler:baboon-bin -- --help

# Rust stubs (codegen + build + tests):
bazel test //test/rs-stub/...

# Python stubs (codegen + build + tests):
bazel test //test/py-stub/...

# GraalVM native-image (mirrors build.sbt graalVMNativeImageOptions):
# --no-fallback, -H:-CheckToolchain, -H:+UnlockExperimentalVMOptions,
# --enable-https, --enable-http, -march=compatibility,
# plus --static --libc=musl on linux-amd64. Manual: needs GraalVM native-image
# (see baboon-compiler/BUILD.bazel:105-128; JDK toolchain TODO at MODULE.bazel:168-180).
bazel build //baboon-compiler:baboon-native
```

## Layout

- `MODULE.bazel`: bzlmod module, Scala/Python/Rust/Java extensions, `maven.install` deps mirroring `build.sbt` (plus direct pins for transitive-only artifacts Bazel needs explicitly: `fundamentals-bio/basics/functoid/orphans`, `distage-core-api`, `izumi-reflect` + boopickle-shaded, `shapeless`, `sourcecode`, `data-class`, `case-app-util/annotations`, `cats-core/kernel`; `jline` via `http_jar` — see below).
- `tools/bazel/baboon.bzl`: `baboon_codegen` rule. Runs the Baboon JVM compiler at build time and captures generated sources as explicit declared outputs. Replaced the deprecated mdl `test-gen-*` actions. Generated file sets are pinned in `test/rs-stub/generated.bzl`, `test/py-stub/py_generated_*.bzl` (re-list when models change).
- `tools/bazel/py_unittest_discover.py`: `python -m unittest discover` equivalent that takes dotted package names (paths vary by config, names don't).
- `baboon-compiler/BUILD.bazel`: single `scala_library :baboon` mirroring sbt's `baboonJVM` project (shared sources + `.jvm` platform sources; `.js` excluded — needs the Scala.js toolchain, see `baboon-compiler/BUILD.bazel:50-56`), `scala_binary :baboon-bin` (`main_class io.septimalmind.baboon.Baboon`), `filegroup :runtime_templates` + `:codegen_models`, genrule `:gen_runtime_resources` (replaces the izumi `PortableResource.embedSources` macro, which requires `build.sbt` on disk and fails under Bazel), manual genrule `:baboon-native`.
- `scripts/BUILD.bazel`: `py_binary` targets `codegen`, `cross_version_compat`, `gen-big-model`, `gen_baboon_resources`.
- `test/py-stub/BUILD.bazel`: codegen for the main corpus (`genmain/`) and the ReviewModels corpus (`genreview/`), `py_library` layers with `imports`, three `py_test` lanes mirroring the deprecated mdl `test-python-regular` lane (GeneratedTests/pkg0, RuntimeTests, ReviewTests). `legacy_create_init = False` everywhere: auto-added `__init__.py` files would turn the split PEP-420 namespace packages (`BaboonTests/` across hand-written and generated roots) into regular packages, hiding one side.
- `test/rs-stub/BUILD.bazel`: codegen for the `:rust` lane, `rust_library :rs_stub` (crate `baboon_rs_stub`), per-file `rust_test` targets over committed `tests/`.

## Test status

- `//test/rs-stub/...`: 23/23 pass.
- `//test/py-stub:test_py_runtime`, `:test_py_review`: pass.
- `//test/py-stub:test_py_genpkg`: builds; 52/104 tests pass. The 52 errors are all `test_*_produced_by_cs_codecs` tests reading C# golden fixtures (`BABOON_CROSS_LANG_FIXTURE_ROOT`), which don't exist until the C# lane is migrated (the deprecated mdl lane ran that first via `dep action.test-cs-regular`). Self-contained tests pass.

## Bazel-vs-sbt dependency notes (strict deps)

Bazel compiles each target against direct deps only; sbt puts the whole transitive closure on the scalac classpath. When adding deps, mirror the *effective* sbt classpath, not just `build.sbt`'s direct list.

## Known workarounds (all commented at their sites)

- `rules_jvm_external` 6.8's coursier invocation requests a nonexistent `jline-4.4.5-jdk8.jar` for any jline coordinate → jline comes from a pinned `http_jar`.
- `rules_rust` builds host tools with `-C strip=debuginfo`, which mislinks Mach-O dylibs with the Xcode 27 linker (`mis-aligned LINKEDIT`) → `--codegen=strip=none` via `extra_rustc_flags`/`extra_exec_rustc_flags` (last codegen flag wins in rustc).
- Remote JDK 11 is the default *tool* runtime even with `java_runtime_version=remotejdk_21` → also set `tool_java_runtime_version=remotejdk_21`.
- `protobuf` must be pinned ≥ 36.2: older upb C sources don't compile under Xcode 27 (`SDKSettings.json` absolute-path error).
