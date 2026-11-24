"""Baboon compiler codegen harness: runs the JVM compiler at build time so
language stub libraries/tests consume generated sources hermetically.

Two modes: canonical (`output_dir`) declares one TreeArtifact dir
`<output_dir>/<lang>`; legacy (`outs` + dir attrs) is deprecated but kept
for the existing `test/*` stub lanes. Python consumes the tree via
`data` + `imports` (srcs cannot take a TreeArtifact).
"""

load("@rules_python//python:defs.bzl", _py_library = "py_library")


def _baboon_codegen_impl(ctx):
    java_runtime = ctx.toolchains["@bazel_tools//tools/jdk:runtime_toolchain_type"].java_runtime
    compiler_input = ctx.file.compiler_jar
    base_inputs = [compiler_input] + ctx.files.models

    # model_dir is the workspace-relative compiler discovery root; it must agree with the `models` inputs.
    raw_model_dir = ctx.attr.model_dir or ctx.label.package
    if raw_model_dir.startswith("/"):
        fail("baboon_codegen '" + str(ctx.label) + "': model_dir '" + raw_model_dir + "' " +
             "must be workspace-relative, not absolute")
    if ".." in raw_model_dir.split("/"):
        fail("baboon_codegen '" + str(ctx.label) + "': model_dir '" + raw_model_dir + "' " +
             "must not contain '..'")
    model_dir = raw_model_dir

    base_args = [
        java_runtime.java_executable_exec_path,
        "-jar",
        compiler_input.path,
        "--model-dir",
        model_dir,
        ctx.attr.lang,
    ] + ctx.attr.extra_args

    if ctx.attr.output_dir:
        if ctx.attr.outs or ctx.attr.output or ctx.attr.test_output or ctx.attr.fixture_output:
            fail("baboon_codegen '" + str(ctx.label) + "': output_dir (canonical) " +
                 "cannot be combined with outs/output/test_output/fixture_output (legacy)")
        lang_name = ctx.attr.lang[1:] if ctx.attr.lang.startswith(":") else ctx.attr.lang
        out_dir = ctx.actions.declare_directory(ctx.attr.output_dir + "/" + lang_name)
        command = " ".join(base_args + [
            "--output",
            out_dir.path,
            "--test-output",
            out_dir.path,
            "--fixture-output",
            out_dir.path,
        ])
        # Fan helper modules into every generated dir so relative imports resolve.
        for dest, src in ctx.attr.copy_files.items():
            command += " && cp {dir}/{src} {dir}/{dest}".format(
                dir = out_dir.path,
                src = src,
                dest = dest,
            )
        if lang_name == "python":
            # Generated siblings import helpers relatively; fan them out so static imports resolve.
            command += (" && for d in $(find {dir} -name '*.py'" +
                        " -exec dirname {{}} \\; | sort -u); do" +
                        " cp {dir}/baboon_*.py \"$d/\" 2>/dev/null || true;" +
                        " cp {dir}/cross_language_fixture_path.py \"$d/\"" +
                        " 2>/dev/null || true; done").format(dir = out_dir.path)
        ctx.actions.run_shell(
            command = command,
            inputs = depset(base_inputs, transitive = [java_runtime.files]),
            outputs = [out_dir],
            mnemonic = "BaboonCodegen",
            progress_message = "Baboon codegen %{label}",
        )
        return [DefaultInfo(files = depset([out_dir]))]

    # Legacy explicit-outs path (deprecated).
    outs = [ctx.actions.declare_file(f) for f in ctx.attr.outs]
    if not outs:
        fail("baboon_codegen '" + str(ctx.label) + "': either output_dir " +
             "(canonical) or a non-empty outs list (legacy) is required")
    # Execroot-relative directory of this package (e.g. .../bin/test/cs-stub).
    # Output dirs are package-relative (e.g. "genlib/BaboonDefinitions/Generated").
    pkg_marker = ctx.label.package + "/"
    pkg_dir = outs[0].path[:outs[0].path.index(pkg_marker) + len(pkg_marker) - 1]

    def _dir_for(prefix):
        return pkg_dir + "/" + prefix

    # Optional post-copy: {dest: src}; dest is an additional declared output.
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

    args = base_args + [
        "--output",
        _dir_for(ctx.attr.output),
        "--test-output",
        _dir_for(ctx.attr.test_output),
        "--fixture-output",
        _dir_for(ctx.attr.fixture_output),
    ]

    ctx.actions.run_shell(
        command = " ".join(args + copy_cmds),
        inputs = depset(
            base_inputs,
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
            default = "",
            doc = ("Workspace-relative path of the models root, passed unchanged as the " +
                   "compiler's --model-dir discovery root (execroot-relative). Bazel inputs " +
                   "only materialize files in the sandbox; model_dir tells the compiler where " +
                   "on disk to scan. Must agree with `models`. Empty defaults to the calling " +
                   "package. Absolute paths and '..' are rejected. NOTE: this rule-level attr " +
                   "is workspace-relative, unlike baboon_file's model_dir param which is " +
                   "package-relative; baboon_file resolves to this form itself."),
        ),
        "lang": attr.string(
            mandatory = True,
            doc = "Compiler lane selector, e.g. ':rust' or ':python'.",
        ),
        "extra_args": attr.string_list(
            doc = "Lane flags.",
        ),
        "output_dir": attr.string(
            default = "",
            doc = ("Canonical mode: package-relative codegen root. The rule declares a single " +
                   "TreeArtifact `<output_dir>/<lang>` and points --output/--test-output/ " +
                   "--fixture-output at it. Cannot be combined with the legacy attrs below."),
        ),
        "output": attr.string(
            default = "",
            doc = "LEGACY (deprecated, use output_dir): package-relative output dir for --output.",
        ),
        "test_output": attr.string(
            default = "",
            doc = "LEGACY (deprecated, use output_dir): package-relative output dir for --test-output.",
        ),
        "fixture_output": attr.string(
            default = "",
            doc = "LEGACY (deprecated, use output_dir): package-relative output dir for --fixture-output.",
        ),
        "outs": attr.string_list(
            default = [],
            doc = "LEGACY (deprecated, use output_dir): generated files, relative to the package.",
        ),
        "copy_files": attr.string_dict(
            default = {},
            doc = "Additional outputs copied from generated outputs: {dest: src}. In canonical mode both are relative to the lang output dir; in legacy mode both are package-relative.",
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

def baboon_file(
        name,
        model_dir,
        langs,
        compiler_jar = "//baboon-compiler:baboon-bin_deploy.jar",
        output_dir = "baboon_gen",
        extra_args_by_lang = {},
        **kwargs):
    """Fan out the .baboon models under model_dir across backend languages.

    Every `.baboon` under `model_dir` is an action input (no `src` param).
    `latest/**` is excluded: it is a symlink alias, and following it yields
    a "duplicated versions" failure. Python wraps the TreeArtifact via
    `data` + `imports` (srcs cannot take a directory); other langs use a
    filegroup. `model_dir` is package-relative (e.g. "models").

    Args:
        name: Base name; wrappers are `<name>-<lang>`.
        model_dir: Package-relative models root. Globbed at macro time
            (package-relative), then resolved to a workspace-relative path
            for the compiler's `--model-dir` root.
        langs: Backend language names, e.g. ["python", "rust"].
        compiler_jar: Baboon compiler deploy jar.
        output_dir: Package-relative codegen root; per-lang dirs are
            `<output_dir>/<lang>`.
        extra_args_by_lang: Optional {lang: [extra lane flags]}.

    Removed params (fail loudly if passed): `src`, `extra_models`,
    `outs_by_lang`, `output_base` (renamed to `output_dir`).
    """
    if "src" in kwargs:
        fail("baboon_file '" + name + "': src was removed; pass model_dir only " +
             "(e.g. baboon_file(name = \"" + name + "\", model_dir = \"models\", " +
             "langs = [...])) and every model_dir/**/*.baboon file is an input automatically")
    if "extra_models" in kwargs:
        fail("baboon_file '" + name + "': extra_models was removed; " +
             "all model_dir .baboon files are action inputs automatically")
    if "outs_by_lang" in kwargs:
        fail("baboon_file '" + name + "': outs_by_lang was removed; " +
             "consumers now take the <output_dir>/<lang> TreeArtifact")
    if "output_base" in kwargs:
        fail("baboon_file '" + name + "': output_base was renamed to output_dir")
    if kwargs:
        fail("baboon_file '" + name + "': unexpected kwargs " + str(kwargs.keys()))
    if type(output_dir) != "string":
        fail("baboon_file '" + name + "': output_dir must be a package-relative " +
             "dir string (got " + type(output_dir) + "); if you passed the old " +
             "positional extra_models/outs_by_lang, switch to output_dir=...)")

    package = native.package_name()
    # model_dir is package-relative; tolerate the old workspace-relative form by stripping the package prefix.
    raw_model_dir = model_dir if model_dir != None else "."
    if raw_model_dir.startswith("/"):
        fail("baboon_file '" + name + "': model_dir '" + raw_model_dir + "' " +
             "must be package-relative (e.g. \"models\"), not absolute")
    if ".." in raw_model_dir.split("/"):
        fail("baboon_file '" + name + "': model_dir '" + raw_model_dir + "' " +
             "must be package-relative and must not contain '..'")
    if raw_model_dir == package:
        relative_model_dir = "."
    elif raw_model_dir.startswith(package + "/"):
        # Old workspace-relative caller: strip the "<package>/" prefix.
        relative_model_dir = raw_model_dir[len(package) + 1:] or "."
    else:
        relative_model_dir = raw_model_dir or "."
    if relative_model_dir.startswith("./"):
        relative_model_dir = relative_model_dir[2:] or "."
    if len(relative_model_dir) > 1 and relative_model_dir.endswith("/"):
        relative_model_dir = relative_model_dir.rstrip("/")

    if relative_model_dir in (".", ""):
        relative_model_dir = "."
        model_patterns = ["**/*.baboon"]
        # Workspace-relative (execroot-relative) discovery root for --model-dir.
        rule_model_dir = package
    else:
        model_patterns = [relative_model_dir + "/**/*.baboon"]
        # Resolve relative -> workspace-relative exactly once here, so the
        # rule passes it through unchanged (no double prefix).
        rule_model_dir = package + "/" + relative_model_dir if package else relative_model_dir

    models_target = name + "_models"
    found_models = native.glob(model_patterns, exclude = ["**/latest/**"])
    if not found_models:
        fail("baboon_file '" + name + "': no .baboon files found under " +
             "model_dir '" + relative_model_dir + "' " +
             "(glob " + str(model_patterns) + " matched nothing); " +
             "check that model_dir is package-relative and contains .baboon files")
    native.filegroup(
        name = models_target,
        srcs = sorted(depset(found_models).to_list()),
    )

    for lang in langs:
        gen_name = name + "_" + lang + "_gen"
        baboon_codegen(
            name = gen_name,
            compiler_jar = compiler_jar,
            models = ":" + models_target,
            model_dir = rule_model_dir,
            lang = ":" + lang,
            output_dir = output_dir,
            extra_args = extra_args_by_lang.get(lang, []),
        )
        if lang == "python":
            # TreeArtifact cannot go in srcs, so it rides as data with its root on the import path.
            _py_library(
                name = name + "-" + lang,
                data = [":" + gen_name],
                imports = [output_dir + "/" + lang],
            )
        else:
            native.filegroup(
                name = name + "-" + lang,
                srcs = [":" + gen_name],
            )
