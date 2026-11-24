"""Shared harness for power_arch1 studies: tolerance checks + provenance."""

import hashlib
import json
import os

import power_model as pm

HERE = os.path.dirname(os.path.abspath(__file__))

# Versioned snapshot via models/latest symlink; BUILD_WORKSPACE_DIRECTORY anchors the workspace root under `bazel run`.
MODELS_DIR = os.path.join(HERE, "models")
LATEST_LINK = os.path.join(MODELS_DIR, "latest")
SCHEMA_SNAPSHOT = os.path.join(LATEST_LINK, "soc.baboon")


def schema_path():
    """Versioned snapshot only; fail loudly if latest is missing."""
    ws = os.environ.get("BUILD_WORKSPACE_DIRECTORY")
    candidates = []
    if ws:
        candidates.append(
            os.path.join(ws, "studies", "power_arch1", "models", "latest", "soc.baboon")
        )
    candidates.append(SCHEMA_SNAPSHOT)
    for cand in candidates:
        if os.path.exists(cand):
            return cand
    raise FileNotFoundError(
        f"versioned schema snapshot missing: {candidates[0]} "
        "(models/latest symlink)"
    )

REL_TOL_POWER = 1e-9   # power/energy: exact table lerp
REL_TOL_PERFW = 1e-6   # perf/W division chain


def rel_err(got, exp):
    if exp == 0.0:
        return 0.0 if got == 0.0 else float("inf")
    return abs(got - exp) / abs(exp)


def check_row(name, got, expected, tols):
    """Return list of (field, ok) and print per-field verdict."""
    rows = []
    for k, exp in expected.items():
        if k == "teff_branch":
            ok = got.get("teff_branch") == exp
            print(f"  [{'PASS' if ok else 'FAIL'}] {name}.{k}: {got.get('teff_branch')} == {exp}")
            rows.append((k, ok))
            continue
        tol = tols.get(k, REL_TOL_POWER)
        err = rel_err(got[k], exp)
        ok = err <= tol
        print(f"  [{'PASS' if ok else 'FAIL'}] {name}.{k}: got={got[k]!r} exp={exp!r} relerr={err:.2e} tol={tol:.0e}")
        rows.append((k, ok))
    return rows


def emit(study, inputs_desc, results, extra=None):
    blob = json.dumps(results, indent=2, sort_keys=True)
    with open(os.path.join(HERE, f"results_{study}.json"), "w") as f:
        f.write(blob + "\n")
    prov = {
        "study": study,
        "inputs": inputs_desc,
        "model_version": pm.MODEL_VERSION,
        "data_versions": pm.DATA_VERSIONS,
        "results_hash": hashlib.sha256(blob.encode()).hexdigest(),
    }
    if extra:
        prov.update(extra)
    with open(os.path.join(HERE, f"provenance_{study}.json"), "w") as f:
        f.write(json.dumps(prov, indent=2, sort_keys=True) + "\n")
    return prov


def summarize(all_rows):
    fails = [r for r in all_rows if not r[1]]
    print(f"== {len(all_rows) - len(fails)}/{len(all_rows)} checks passed ==")
    return 1 if fails else 0
