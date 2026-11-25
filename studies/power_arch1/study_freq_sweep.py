"""Study 1 — freq sweep: sweep f (and paired V from the V-f table) at fixed
cores/activity; expect Pdyn ~ V^2 f and a mem->comp crossover in Teff.

Inputs+execution now in freq_sweep.bbin; this script only prints
results_freq_sweep.json / provenance_freq_sweep.json.
"""

import sys

import study_common as sc


def main():
    results, provenance = sc.load_results("freq_sweep")
    print(f"freq_sweep inputs: {provenance['inputs']}")
    points = results["points"]
    for d in points:
        print(f"V={d['voltage']:.2f} f={d['freq']/1e9:.1f}GHz pdyn={d['pdyn']:.4f} ptotal={d['ptotal']:.4f} "
              f"tcomp={d['tcomp']:.6f} tmem={d['tmem']:.6f} teff={d['teff']:.6f} bound={d['bound']}")

    rows = []
    # Pdyn ~ V^2 f: check consecutive ratios.
    for i in range(1, len(points)):
        v0, f0 = points[i - 1]["voltage"], points[i - 1]["freq"]
        v1, f1 = points[i]["voltage"], points[i]["freq"]
        pred = (v1 ** 2 * f1) / (v0 ** 2 * f0)
        got = points[i]["pdyn"] / points[i - 1]["pdyn"]
        ok = sc.rel_err(got, pred) <= sc.REL_TOL_POWER
        print(f"  [{'PASS' if ok else 'FAIL'}] pdyn ratio [{i}]/[{i-1}]: got={got:.12f} pred={pred:.12f}")
        rows.append((f"pdyn_ratio_{i}", ok))

    # Pleak rises monotonically with V (table lerp, leakMult=1).
    ok = all(points[i]["pleak"] > points[i - 1]["pleak"] for i in range(1, len(points)))
    print(f"  [{'PASS' if ok else 'FAIL'}] pleak monotonic in V")
    rows.append(("pleak_monotonic", ok))

    # Teff must equal min(tcomp, tmem) exactly (branch equality) at every point.
    for i, d in enumerate(points):
        ok = d["teff"] == min(d["tcomp"], d["tmem"])
        print(f"  [{'PASS' if ok else 'FAIL'}] teff==min branch at point {i}")
        rows.append((f"teff_branch_{i}", ok))

    # Crossover: first point memory-bound, last point compute-bound.
    ok = points[0]["bound"] == "memory" and points[-1]["bound"] == "compute"
    print(f"  [{'PASS' if ok else 'FAIL'}] crossover memory->compute "
          f"({points[0]['bound']} -> {points[-1]['bound']})")
    rows.append(("crossover", ok))

    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
