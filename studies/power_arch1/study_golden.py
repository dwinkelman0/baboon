"""Golden row: nominal point (4 cores, mid V-f, activity 0.5, mid intensity).

Freezes studies/power_arch1/golden_expected.json on first run, then verifies
every derived output against it with study tolerances on every later run.
Usage: python3 study_golden.py [--freeze]
"""

import json
import os
import sys

import power_model as pm
import study_common as sc

HERE = os.path.dirname(os.path.abspath(__file__))
GOLDEN_FILE = os.path.join(HERE, "golden_expected.json")

TOLS = {"perfW": sc.REL_TOL_PERFW}


def main():
    soc = pm.SoC(**pm.golden_inputs())
    got = soc.derived()
    got["teff_branch"] = "compute" if soc.tcomp <= soc.tmem else "memory"

    if "--freeze" in sys.argv or not os.path.exists(GOLDEN_FILE):
        with open(GOLDEN_FILE, "w") as f:
            f.write(json.dumps(got, indent=2, sort_keys=True) + "\n")
        print(f"froze {GOLDEN_FILE}:")
        print(json.dumps(got, indent=2, sort_keys=True))

    with open(GOLDEN_FILE) as f:
        expected = json.load(f)

    print("golden row checks:")
    rows = sc.check_row("golden", got, expected, TOLS)
    sc.emit("golden",
            {"desc": "nominal point", "inputs": {k: str(v) for k, v in pm.golden_inputs().items()
                                                 if k not in ("leak_table", "mem_table", "vf_table")}},
            {"golden": got, "expected": expected})
    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
