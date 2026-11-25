"""Golden row: nominal point (4 cores, mid V-f, activity 0.5, mid intensity).

Inputs+execution now in golden.bbin; this script only prints
results_golden.json / provenance_golden.json.
"""

import os
import sys

import study_common as sc

HERE = os.path.dirname(os.path.abspath(__file__))

TOLS = {"perfW": sc.REL_TOL_PERFW}


def main():
    results, provenance = sc.load_results("golden")
    print(f"golden inputs: {provenance['inputs']} "
          f"(model {provenance.get('model_version')})")
    got = results["golden"]
    expected = results["expected"]

    print("golden row checks:")
    rows = sc.check_row("golden", got, expected, TOLS)
    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
