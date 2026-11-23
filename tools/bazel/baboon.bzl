"""Baboon compiler codegen harness.

Runs the Baboon JVM compiler at build time and captures generated sources as
explicit declared outputs, so language stub libraries/tests can consume them
hermetically. Replaces the mdl `test-gen-*` actions (which rsynced stubs into
target/ and ran the native binary there).

Outputs must be listed explicitly (see test/rs-stub/generated.bzl,
test/py-stub/py_generated_*.bzl): Bazel requires hermetic actions to declare
every output file. The set changes only when the .baboon models change;
regenerate the list then.
"""

def _baboon_codegen_impl(ctx):
    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:runtime_toolchain_type"].java_runtime
    outs = [ctx.actions.declare_file(f) for f in ctx.attr.outs]
    if not outs:
        fail("outs must not be empty for " + str(ctx.label))
    # Execroot-relative directory of this package (e.g. .../bin/test/cs-stub).
    # Output dirs are package-relative (e.g. "genlib/BaboonDefinitions/Generated").
    pkg_marker = ctx.label.package + "/"
    pkg_dir = outs[0].path[:outs[0].path.index(pkg_marker) + len(pkg_marker) - 1]

    def _dir_for(prefix):
        return pkg_dir + "/" + prefix

    # Optional post-copy: {dest: src}, both package-relative paths.
    # Declares dest as an additional output copied from the (already declared)
    # src after codegen runs. Used to fan a single generated file out into
    # per-module dirs that must each compile their own copy (e.g. Swift's
    # CrossLanguageFixturePath.swift, which test files expect as a same-module
    # top-level declaration with no import).
    copies = []
    copy_cmds = []
    for dest, src in ctx.attr.copy_files.items():
        dest_file = ctx.actions.declare_file(dest)
        copies.append(dest_file)
        copy_cmds.append("cp {pkg}/{src} {dest}".format(
            pkg = pkg_dir,
            src = src,
            dest = dest_file.path,
        ))

    args = [
        java_runtime.java_executable_exec_path,
        "-jar",
        ctx.file.compiler_jar.path,
        "--model-dir",
        ctx.attr.model_dir,
        ctx.attr.lang,
        "--output",
        _dir_for(ctx.attr.output),
        "--test-output",
        _dir_for(ctx.attr.test_output),
        "--fixture-output",
        _dir_for(ctx.attr.fixture_output),
    ] + ctx.attr.extra_args

    ctx.actions.run_shell(
        command = " ".join(args + copy_cmds),
        inputs = depset(
            [ctx.file.compiler_jar] + ctx.files.models,
            transitive = [java_runtime.files],
        ),
        outputs = outs + copies,
        mnemonic = "BaboonCodegen",
        progress_message = "Baboon codegen %{label}",
    )
    return [DefaultInfo(files = depset(outs + copies))]

baboon_codegen = rule(
    implementation = _baboon_codegen_impl,
    attrs = {
        "compiler_jar": attr.label(
            allow_single_file = [".jar"],
            mandatory = True,
            doc = "Baboon compiler deploy jar (//baboon-compiler:baboon-bin_deploy.jar).",
        ),
        "models": attr.label(
            mandatory = True,
            doc = "Filegroup of .baboon model files (action inputs).",
        ),
        "model_dir": attr.string(
            mandatory = True,
            doc = "Workspace-relative path of the models root.",
        ),
        "lang": attr.string(
            mandatory = True,
            doc = "Compiler lane selector, e.g. ':rust' or ':python'.",
        ),
        "extra_args": attr.string_list(
            doc = "Lane flags.",
        ),
        "output": attr.string(
            mandatory = True,
            doc = "Package-relative output dir for --output.",
        ),
        "test_output": attr.string(
            mandatory = True,
            doc = "Package-relative output dir for --test-output.",
        ),
        "fixture_output": attr.string(
            mandatory = True,
            doc = "Package-relative output dir for --fixture-output.",
        ),
        "outs": attr.string_list(
            mandatory = True,
            doc = "Generated files, relative to the package.",
        ),
        "copy_files": attr.string_dict(
            default = {},
            doc = "Additional outputs copied from declared outputs: {dest: src}, both package-relative.",
        ),
    },
    toolchains = ["@bazel_tools//tools/jdk:runtime_toolchain_type"],
)

# Backwards-compatible alias for the Rust lane (all three outputs share one dir).
def baboon_rust_gen(name, outs, **kwargs):
    baboon_codegen(
        name = name,
        outs = outs,
        output = "gen",
        test_output = "gen",
        fixture_output = "gen",
        **kwargs
    )
