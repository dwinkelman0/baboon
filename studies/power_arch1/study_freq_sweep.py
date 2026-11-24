"""Study 1 — freq sweep: sweep f (and paired V from the V-f table) at fixed
cores/activity; expect Pdyn ~ V^2 f and a mem->comp crossover in Teff.

Sweep points ride the V-f table so every point satisfies the f<=fmax assert:
(0.70V,1.6GHz) .. (1.00V,3.4GHz).

Note: Tcomp = ops/(f*cores*ipc) falls with f; Tmem is f-independent, so as f
rises Teff crosses from memory-bound (low f) to compute-bound (high f).
The sweep uses bytesPerTask=2.4e9 (Tmem=0.064s) so Tmem lands inside the
Tcomp range [0.0368, 0.0781]s and the crossover is exercised; the golden
row keeps its own task mix.
"""

import sys

import power_model as pm
import study_common as sc

SWEEP = [(0.70, 1.6e9), (0.80, 2.2e9), (0.90, 2.8e9), (1.00, 3.4e9)]
SWEEP_BYTES_PER_TASK = 2.4e9


def main():
    base = pm.golden_inputs()
    points = []
    for v, f in SWEEP:
        kw = dict(base)
        kw["op"] = pm.OpPoint(voltage=v, freq=f, temp=340.0)
        kw["bytesPerTask"] = SWEEP_BYTES_PER_TASK
        soc = pm.SoC(**kw)
        d = pm.derived(soc)
        d["voltage"] = v
        d["freq"] = f
        d["bound"] = "compute" if soc.tcomp <= soc.tmem else "memory"
        points.append(d)
        print(f"V={v:.2f} f={f/1e9:.1f}GHz pdyn={d['pdyn']:.4f} ptotal={d['ptotal']:.4f} "
              f"tcomp={d['tcomp']:.6f} tmem={d['tmem']:.6f} teff={d['teff']:.6f} bound={d['bound']}")

    rows = []
    # Pdyn ~ V^2 f: check consecutive ratios.
    for i in range(1, len(points)):
        v0, f0 = SWEEP[i - 1]
        v1, f1 = SWEEP[i]
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

    sc.emit("freq_sweep",
            {"sweep": [{"voltage": v, "freq": f} for v, f in SWEEP],
             "fixed": "cores=4, activity=0.5, memIntensity=0.5, bytesPerTask=2.4e9"},
            {"points": points})
    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
