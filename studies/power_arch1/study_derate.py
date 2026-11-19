"""Study 3 — ceff/leak derate: scalars < 1 (0.9/0.8).

Expect proportional Pdyn/Pleak drop and unchanged timing:
  - pdyn(s) / pdyn(1) == ceff_scale.
  - pleak(s) / pleak(1) == leak_mult.
  - tcomp, tmem, teff bit-identical (scalars do not enter timing).
  - eop scales with the power drop; perfW scales with 1/power.
"""

import sys

import power_model as pm
import study_common as sc

DERATES = [
    (1.0, 1.0),
    (0.9, 1.0),
    (1.0, 0.8),
    (0.9, 0.8),
]


def run_at(ceff, leak):
    kw = pm.golden_inputs()
    kw["scalars"] = pm.Scalars(ceffMult=ceff, leakMult=leak, perfAdj=1.0)
    soc = pm.SoC(**kw)
    d = soc.derived()
    d["ceffMult"] = ceff
    d["leakMult"] = leak
    return d


def main():
    base = run_at(1.0, 1.0)
    print(f"base: pdyn={base['pdyn']:.4f} pleak={base['pleak']:.4f} ptotal={base['ptotal']:.4f} "
          f"teff={base['teff']:.6f} eop={base['eop']:.4e} perfW={base['perfW']:.4e}")
    rows = []
    results = {"base": base, "derates": []}

    def verdict(name, ok, detail=""):
        print(f"  [{'PASS' if ok else 'FAIL'}] {name} {detail}")
        rows.append((name, ok))

    for ceff, leak in DERATES[1:]:
        d = run_at(ceff, leak)
        results["derates"].append(d)
        tag = f"ceff={ceff}_leak={leak}"
        print(f"{tag}: pdyn={d['pdyn']:.4f} pleak={d['pleak']:.4f} ptotal={d['ptotal']:.4f} "
              f"teff={d['teff']:.6f}")
        verdict(f"pdyn_proportional[{tag}]",
                sc.rel_err(d["pdyn"] / base["pdyn"], ceff) <= sc.REL_TOL_POWER,
                f"ratio={d['pdyn']/base['pdyn']:.12f} pred={ceff}")
        verdict(f"pleak_proportional[{tag}]",
                sc.rel_err(d["pleak"] / base["pleak"], leak) <= sc.REL_TOL_POWER,
                f"ratio={d['pleak']/base['pleak']:.12f} pred={leak}")
        for k in ("tcomp", "tmem", "teff"):
            verdict(f"{k}_unchanged[{tag}]", d[k] == base[k],
                    f"{d[k]!r} vs {base[k]!r}")
        verdict(f"meff_unchanged[{tag}]", d["meff"] == base["meff"])
        # ptotal is the sum: check absolute recomputation, not a ratio.
        verdict(f"ptotal_consistent[{tag}]",
                sc.rel_err(d["ptotal"], d["pdyn"] + d["pleak"]) <= sc.REL_TOL_POWER)
        # eop scales with total energy; perfW inversely.
        verdict(f"eop_ratio[{tag}]",
                sc.rel_err(d["eop"] / base["eop"], d["ptotal"] / base["ptotal"]) <= sc.REL_TOL_POWER)
        verdict(f"perfW_ratio[{tag}]",
                sc.rel_err(d["perfW"] / base["perfW"], base["ptotal"] / d["ptotal"]) <= sc.REL_TOL_PERFW)

    sc.emit("derate",
            {"derates": [{"ceffMult": c, "leakMult": l} for c, l in DERATES],
             "fixed": "golden point, scalars vary"},
            results)
    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
