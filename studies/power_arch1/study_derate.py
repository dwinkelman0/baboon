"""Study 3 — ceff/leak derate: scalars < 1.

Expect proportional Pdyn/Pleak drop and unchanged timing:
  - pdyn(s) / pdyn(1) == ceff_scale.
  - pleak(s) / pleak(1) == leak_mult.
  - tcomp, tmem, teff bit-identical (scalars do not enter timing).
  - eop scales with the power drop; perfW scales with 1/power.

Inputs+execution now in derate.bbin; this script only prints
results_derate.json / provenance_derate.json.
"""

import sys

import study_common as sc


def main():
    results, provenance = sc.load_results("derate")
    print(f"derate inputs: {provenance['inputs']}")
    base = results["base"]
    print(f"base: pdyn={base['pdyn']:.4f} pleak={base['pleak']:.4f} ptotal={base['ptotal']:.4f} "
          f"teff={base['teff']:.6f} eop={base['eop']:.4e} perfW={base['perfW']:.4e}")
    rows = []

    def verdict(name, ok, detail=""):
        print(f"  [{'PASS' if ok else 'FAIL'}] {name} {detail}")
        rows.append((name, ok))

    for d in results["derates"]:
        ceff, leak = d["ceffMult"], d["leakMult"]
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

    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
