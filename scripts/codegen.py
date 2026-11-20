"""Regenerate Baboon Python models for this checkout.

Exposed as the ``baboon-codegen`` console script (see ``[project.scripts]``
in ``pyproject.toml``)::

    uv run baboon-codegen
    uv run baboon-codegen --study power_arch1
    uv run baboon-codegen --study power_arch1 --check
    uv run baboon-codegen --dry-run

Targets:

* default (project) target: ``<checkout>/src/spaced_repetition.baboon``
  -> ``<checkout>/models/python`` in the outer ``baboon-test`` checkout
  (parent of this repo root; kept for backward compat).
* ``--study power_arch1``: single-context codegen — every versioned
  ``models/v*/soc.baboon`` schema is passed together via repeatable
  ``--model-dir <version-dir>`` flags and rendered once into the single
  ``pygen/`` root inside ``baboon/studies/power_arch1`` (what
  run_codegen_check.py imports). One compiler invocation sees all
  versions, so the conversions translator can emit cross-version
  migrations. Use ``--schema``/``--output`` for a single-shot
  ``--model`` override. The single-shot schema resolves via the versioned
  snapshot ``models/latest/soc.baboon`` (the ``latest`` symlink) — the
  single source; there is no live fallback — the same rule as
  ``study_common.schema_path()``. Each ``models/v*/`` dir is passed
  explicitly (rather than ``--model-dir models/``) so the ``latest``
  symlink (``latest -> vX.Y.Z``) is never walked twice.

The ``baboon`` binary is resolved as: ``--baboon`` flag, then the local
dev build ``baboon-compiler/.jvm/target/graalvm-native-image/baboon``
(produced by ``./setup-baboon.sh``, or ``sbt graalvm native-image`` /
``sbt run`` inside this repo root), then the ``baboon`` dev launcher in
this repo root, and finally ``baboon`` on ``PATH``.

NOTE: an OSS ``baboon`` release installed on ``PATH`` (e.g.
``baboon 0.0.201-SNAPSHOT``) is expected to FAIL on ``fn`` syntax — it
predates it. A local dev build is required. Using a ``PATH`` baboon as a
last resort prints a warning. Pass through extra flags to ``baboon`` after
``--`` (e.g. ``-- --generate-json-codecs-by-default=true``).
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# ROOT is this git repo's root (baboon/scripts/../ = baboon/). The default
# (non-study) project target lives in the outer `baboon-test` checkout, i.e.
# the parent of ROOT — kept for backward compat with the script's previous
# location at <checkout>/scripts/codegen.py.
OUTER_ROOT = os.path.dirname(ROOT)

STUDY_DIR = os.path.join(ROOT, "studies", "power_arch1")

PROJECT_SCHEMA = os.path.join(OUTER_ROOT, "src", "spaced_repetition.baboon")
PROJECT_OUTPUT = os.path.join(OUTER_ROOT, "models", "python")

# Local dev build output (see setup-baboon.sh `verify_binary`), relative to
# this repo root.
NATIVE_IMAGE_RELPATH = os.path.join(
    "baboon-compiler", ".jvm", "target", "graalvm-native-image", "baboon"
)


def study_schema() -> str:
    """Versioned snapshot only; fail loudly if latest is missing."""
    snapshot = os.path.join(STUDY_DIR, "models", "latest", "soc.baboon")
    if not os.path.exists(snapshot):
        raise FileNotFoundError(
            f"versioned schema snapshot missing: {snapshot} (models/latest symlink)"
        )
    return snapshot


def study_schemas() -> list[tuple[str, str]]:
    """All versioned schemas as (version, path), sorted oldest-first.

    Kept for introspection/debugging; codegen itself passes version
    *directories* (see ``study_model_dirs``) so one compiler invocation
    sees every version in a single context.
    """
    models = os.path.join(STUDY_DIR, "models")
    out: list[tuple[str, str]] = []
    for name in sorted(os.listdir(models)):
        if not name.startswith("v"):
            continue
        schema = os.path.join(models, name, "soc.baboon")
        if os.path.isfile(schema):
            out.append((name, schema))
    return out


def study_model_dirs() -> list[str]:
    """Versioned model dirs as absolute paths, sorted oldest-first.

    Each ``models/v*/`` dir containing a ``soc.baboon`` is returned
    explicitly; the ``latest`` symlink is deliberately skipped so the
    compiler never loads the same version twice.
    """
    models = os.path.join(STUDY_DIR, "models")
    out: list[str] = []
    for name in sorted(os.listdir(models)):
        if not name.startswith("v"):
            continue
        if os.path.islink(os.path.join(models, name)):
            continue
        d = os.path.join(models, name)
        if os.path.isdir(d) and os.path.isfile(os.path.join(d, "soc.baboon")):
            out.append(os.path.abspath(d))
    return out


def study_version_output(version: str) -> str:
    return os.path.join(STUDY_DIR, "pygen", version)


def study_output() -> str:
    return os.path.join(STUDY_DIR, "pygen")


def find_baboon(explicit: str | None) -> str:
    """Resolve the baboon binary, preferring the local dev build.

    Order: ``--baboon`` flag, then the dev native-image artifact, then the
    ``baboon`` dev launcher, then ``baboon`` on PATH (OSS release —
    expected to fail on ``fn`` syntax, so warn when falling back to it).
    """
    if explicit:
        return explicit
    native_image = os.path.join(ROOT, NATIVE_IMAGE_RELPATH)
    if os.path.isfile(native_image) and os.access(native_image, os.X_OK):
        return native_image
    launcher = os.path.join(ROOT, "baboon")
    if os.path.isfile(launcher) and os.access(launcher, os.X_OK):
        return launcher
    on_path = shutil.which("baboon")
    if on_path:
        print(
            "warning: using `baboon` from PATH; the OSS release "
            "(e.g. 0.0.201-SNAPSHOT) is expected to fail on `fn` syntax — "
            "prefer the local dev build (./setup-baboon.sh, or "
            "`sbt graalvm native-image` / `sbt run` in this repo)",
            file=sys.stderr,
        )
        return on_path
    raise FileNotFoundError(
        "no `baboon` binary found: build the local dev checkout via "
        "./setup-baboon.sh (or `sbt graalvm native-image` / `sbt run` "
        "in this repo), or pass --baboon <path>. NOTE: an OSS `baboon` "
        "release on PATH (e.g. 0.0.201-SNAPSHOT) is expected to fail on "
        "`fn` syntax — a local build is required."
    )


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="baboon-codegen",
        description="Regenerate Baboon Python models (project models by "
        "default, --study power_arch1 for the power_arch1 study). Requires "
        "a local baboon dev build (./setup-baboon.sh, or `sbt graalvm "
        "native-image` / `sbt run` in this repo): an OSS `baboon` release on "
        "PATH (e.g. 0.0.201-SNAPSHOT) is expected to fail on `fn` syntax.",
    )
    p.add_argument(
        "--study",
        choices=["power_arch1"],
        default=None,
        help="regenerate the given study's pygen/ instead of models/python",
    )
    p.add_argument(
        "--schema",
        default=None,
        help="override the .baboon schema file (default: per-target schema)",
    )
    p.add_argument(
        "--output",
        default=None,
        help="override the :python output dir (default: per-target dir)",
    )
    p.add_argument(
        "--baboon",
        default=None,
        help="override the baboon binary path (default: auto-detect — "
        "local dev native-image, then baboon launcher, then PATH; "
        "a PATH/OSS baboon is expected to fail on `fn` syntax)",
    )
    p.add_argument(
        "--check",
        action="store_true",
        help="after regenerating power_arch1, run run_codegen_check.py "
        "(codegen output vs power_model.py oracle)",
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="print the baboon command without executing it",
    )
    p.add_argument(
        "extra",
        nargs=argparse.REMAINDER,
        help="extra flags forwarded to baboon (after --)",
    )
    return p


def run(cmd: list[str], cwd: str, dry_run: bool) -> int:
    print("+", " ".join(cmd))
    if dry_run:
        return 0
    proc = subprocess.run(cmd, cwd=cwd)
    return proc.returncode


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)

    try:
        binary = find_baboon(args.baboon)
    except FileNotFoundError as e:
        print(f"error: {e}", file=sys.stderr)
        return 2

    extra = [a for a in (args.extra or []) if a != "--"]
    cmds: list[list[str]] = []
    cwd = ROOT

    if args.study == "power_arch1":
        cwd = STUDY_DIR
        if args.schema or args.output:
            # Single-shot --model override (debugging/one-off).
            schema = os.path.abspath(args.schema or study_schema())
            output = os.path.abspath(args.output or study_output())
            if not os.path.isfile(schema):
                print(f"error: schema file(s) not found: {[schema]}",
                      file=sys.stderr)
                return 2
            cmds = [[binary, "--model", schema, ":python",
                     "--output", output, *extra]]
        else:
            # Single-context codegen: one invocation sees every version,
            # so the conversions translator can emit cross-version
            # migrations into the single pygen/ root (what
            # run_codegen_check.py imports as pygen.*).
            model_dirs = study_model_dirs()
            if not model_dirs:
                print("error: no versioned schemas under models/v*/",
                      file=sys.stderr)
                return 2
            output = os.path.abspath(study_output())
            cmd = [binary]
            for d in model_dirs:
                cmd += ["--model-dir", d]
            cmd += [":python", "--output", output, *extra]
            cmds = [cmd]
    else:
        schema = args.schema or PROJECT_SCHEMA
        output = args.output or PROJECT_OUTPUT
        cwd = ROOT
        if not os.path.isfile(schema):
            print(f"error: schema file(s) not found: {[schema]}",
                  file=sys.stderr)
            return 2
        cmds = [[binary, "--model", os.path.abspath(schema), ":python",
                 "--output", os.path.abspath(output), *extra]]

    for cmd in cmds:
        rc = run(cmd, cwd=ROOT, dry_run=args.dry_run)
        if rc != 0:
            return rc

    if args.check:
        if args.study != "power_arch1":
            print("error: --check is only supported with --study power_arch1",
                  file=sys.stderr)
            return 2
        check = os.path.join(STUDY_DIR, "run_codegen_check.py")
        rc = run([sys.executable, check], cwd=cwd, dry_run=args.dry_run)
    return rc


if __name__ == "__main__":
    sys.exit(main())
