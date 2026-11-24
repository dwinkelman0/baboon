"""Study 2 — cores+1: N vs N+1 cores at fixed V-f.

Expect sublinear speedup when memory-bound (Teff pinned by Tmem while
Tcomp falls), and Pleak scaling with core count at fixed V (leak table is
per-voltage; per-core share ~ Pleak/cores is invariant here, total Pleak
is V-driven so the study asserts the scaling relationship explicitly:
Pdyn grows with Ceff ~ cores, Pleak is cores-independent at fixed V).

Checks:
  - tcomp ratio == N/(N+1) exactly-ish (1e-9).
  - teff speedup <= linear (sublinear when mem-bound) and >= 1.
  - pdyn ratio == (N+1)/N (Ceff linear in cores).
  - pleak unchanged across core count at fixed V.
"""

import sys

import power_model as pm
import study_common as sc


def run_at_cores(cores):
    kw = pm.golden_inputs()
    kw["arch"] = pm.Arch(cores=cores, cacheMB=8.0, memCfg="ddr5-1ch")
    soc = pm.SoC(**kw)
    d = pm.derived(soc)
    d["cores"] = cores
    d["bound"] = "compute" if soc.tcomp <= soc.tmem else "memory"
    return d


def main():
    n0, n1 = 4.0, 5.0
    a = run_at_cores(n0)
    b = run_at_cores(n1)
    print(f"N={n0}: tcomp={a['tcomp']:.6f} tmem={a['tmem']:.6f} teff={a['teff']:.6f} "
          f"pdyn={a['pdyn']:.4f} pleak={a['pleak']:.4f} bound={a['bound']}")
    print(f"N={n1}: tcomp={b['tcomp']:.6f} tmem={b['tmem']:.6f} teff={b['teff']:.6f} "
          f"pdyn={b['pdyn']:.4f} pleak={b['pleak']:.4f} bound={b['bound']}")

    rows = []

    def verdict(name, ok, detail=""):
        print(f"  [{'PASS' if ok else 'FAIL'}] {name} {detail}")
        rows.append((name, ok))

    # Tcomp scales as 1/cores.
    verdict("tcomp_scales_1_over_cores",
            sc.rel_err(b["tcomp"] / a["tcomp"], n0 / n1) <= sc.REL_TOL_POWER,
            f"ratio={b['tcomp']/a['tcomp']:.12f} pred={n0/n1:.12f}")
    # Pdyn scales linearly with cores (Ceff ~ cores).
    verdict("pdyn_scales_with_cores",
            sc.rel_err(b["pdyn"] / a["pdyn"], n1 / n0) <= sc.REL_TOL_POWER,
            f"ratio={b['pdyn']/a['pdyn']:.12f} pred={n1/n0:.12f}")
    # Pleak is V-driven: unchanged at fixed V.
    verdict("pleak_cores_independent_at_fixed_V",
            sc.rel_err(b["pleak"], a["pleak"]) <= sc.REL_TOL_POWER,
            f"{a['pleak']:.6f} vs {b['pleak']:.6f}")
    # Speedup sublinear-or-equal (mem-bound pins Teff): 1 <= S <= N+1/N.
    speedup = a["teff"] / b["teff"]
    verdict("speedup_sublinear",
            1.0 <= speedup <= (n1 / n0) + 1e-12,
            f"S={speedup:.6f} linear={(n1/n0):.6f}")
    # Golden point is compute-bound at N=4 engagement... report, and require
    # the N+1 point stays on the same bound branch (no surprise crossover).
    verdict("bound_stable", a["bound"] == b["bound"],
            f"{a['bound']} -> {b['bound']}")
    # Teff branch equality at both points.
    for d, tag in ((a, "N"), (b, "N+1")):
        verdict(f"teff_branch_{tag}", d["teff"] == min(d["tcomp"], d["tmem"]))

    sc.emit("cores_plus1",
            {"N": n0, "N+1": n1, "fixed": "V=0.9 f=2.8GHz activity=0.5 intensity=0.5"},
            {"N": a, "N+1": b})
    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
