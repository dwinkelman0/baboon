"""Facade check: power_model.py re-exports baboon `:python` codegen.

The old oracle-vs-codegen differential is gone by construction — the oracle
*is* codegen now, so a field-by-field comparison would compare the generator
against itself. Instead this pins what keeps the facade honest:
  1. every re-exported symbol IS the generated class (no shadow copies);
  2. pm.derived() agrees with direct property reads on every study point;
  3. the golden derived row matches golden_expected.json.

Run via `bazel run //studies/power_arch1:run_codegen_check` (imports resolve from `:soc-python`).
"""

import json
import os
import sys

import power_model as pm
import study_common as sc

from power.arch1.Arch import Arch
from power.arch1.LeakPoint import LeakPoint
from power.arch1.LeakTable import LeakTable
from power.arch1.MemPoint import MemPoint
from power.arch1.MemTable import MemTable
from power.arch1.OpPoint import OpPoint
from power.arch1.PowerViolation import OverVoltage, PcapExceed, PowerViolation, TempExceed
from power.arch1.RunProvenance import RunProvenance
from power.arch1.Scalars import Scalars
from power.arch1.SoC import SoC
from power.arch1.VfPoint import VfPoint
from power.arch1.VfTable import VfTable
from power.arch1.Workload import Workload

HERE = os.path.dirname(os.path.abspath(__file__))


def golden_file():
    """Workspace-anchored golden file (HERE is runfiles-rooted under `bazel run`)."""
    ws = os.environ.get("BUILD_WORKSPACE_DIRECTORY")
    if ws:
        cand = os.path.join(ws, "studies", "power_arch1", "golden_expected.json")
        if os.path.exists(cand):
            return cand
    return os.path.join(HERE, "golden_expected.json")


GOLDEN_FILE = golden_file()
TOLS = {"perfW": sc.REL_TOL_PERFW}

REEXPORTS = {
    "OpPoint": OpPoint,
    "Arch": Arch,
    "Workload": Workload,
    "Scalars": Scalars,
    "LeakPoint": LeakPoint,
    "MemPoint": MemPoint,
    "VfPoint": VfPoint,
    "LeakTable": LeakTable,
    "MemTable": MemTable,
    "VfTable": VfTable,
    "PowerViolation": PowerViolation,
    "OverVoltage": OverVoltage,
    "TempExceed": TempExceed,
    "PcapExceed": PcapExceed,
    "RunProvenance": RunProvenance,
    "SoC": SoC,
}


def points():
    base = pm.golden_inputs()
    cases = {"golden": base}
    for i, (v, f) in enumerate([(0.70, 1.6e9), (1.00, 3.4e9)]):
        kw = dict(base)
        kw["op"] = pm.OpPoint(voltage=v, freq=f, temp=340.0)
        cases[f"sweep_{i}"] = kw
    kw = dict(base)
    kw["arch"] = pm.Arch(cores=5.0, cacheMB=8.0, memCfg="ddr5-1ch")
    cases["cores+1"] = kw
    kw = dict(base)
    kw["scalars"] = pm.Scalars(ceffMult=0.9, leakMult=0.8, perfAdj=1.0)
    cases["derate"] = kw
    return cases


def main():
    rows = []

    def verdict(name, ok, detail=""):
        print(f"  [{'PASS' if ok else 'FAIL'}] {name} {detail}")
        rows.append((name, ok))

    for name, cls in REEXPORTS.items():
        verdict(f"reexport:{name}", getattr(pm, name) is cls)

    for name, kw in points().items():
        soc = pm.SoC(**kw)
        d = pm.derived(soc)
        for k, v in d.items():
            verdict(f"derived:{name}.{k}", v == getattr(soc, k))

    soc = pm.SoC(**pm.golden_inputs())
    got = pm.derived(soc)
    with open(GOLDEN_FILE) as f:
        expected = json.load(f)
    for r in sc.check_row("golden_file", got, {k: v for k, v in expected.items() if k != "teff_branch"}, TOLS):
        rows.append(r)

    return sc.summarize(rows)


if __name__ == "__main__":
    sys.exit(main())
