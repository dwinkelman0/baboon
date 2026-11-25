"""Shared printing helpers for power_arch1 studies.

Inputs+execution now live in the corresponding .bbin files (golden.bbin,
golden_derate.bbin, freq_sweep.bbin, cores_plus1.bbin, derate.bbin); this
module only loads stored results and prints check tables.
"""

import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))

REL_TOL_POWER = 1e-9   # power/energy: exact table lerp
REL_TOL_PERFW = 1e-6   # perf/W division chain


def rel_err(got, exp):
    if exp == 0.0:
        return 0.0 if got == 0.0 else float("inf")
    return abs(got - exp) / abs(exp)


def load_results(study):
    """Load the stored results + provenance for a study (written by the
    .bbin lowering; this script never computes)."""
    with open(os.path.join(HERE, f"results_{study}.json")) as f:
        results = json.load(f)
    with open(os.path.join(HERE, f"provenance_{study}.json")) as f:
        provenance = json.load(f)
    return results, provenance


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


def summarize(all_rows):
    fails = [r for r in all_rows if not r[1]]
    print(f"== {len(all_rows) - len(fails)}/{len(all_rows)} checks passed ==")
    return 1 if fails else 0
