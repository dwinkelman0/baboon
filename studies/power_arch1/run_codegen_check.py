"""Differential check: baboon `:python` codegen output vs the hand-lowered
power_model.py oracle. Instantiates the golden row + one point per study in
the GENERATED classes and requires exact agreement on all derived outputs.

Run from studies/power_arch1:  python3 run_codegen_check.py
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)          # makes `pygen.*` (generated) importable
sys.path.insert(0, HERE)          # and the local study modules

import power_model as pm

from pygen.power.arch1.Arch import Arch
from pygen.power.arch1.LeakPoint import LeakPoint
from pygen.power.arch1.LeakTable import LeakTable
from pygen.power.arch1.MemPoint import MemPoint
from pygen.power.arch1.MemTable import MemTable
from pygen.power.arch1.OpPoint import OpPoint
from pygen.power.arch1.Scalars import Scalars
from pygen.power.arch1.SoC import SoC
from pygen.power.arch1.VfPoint import VfPoint
from pygen.power.arch1.VfTable import VfTable
from pygen.power.arch1.Workload import Workload


def gen_soc(inp):
    return SoC(
        op=OpPoint(voltage=inp["op"].voltage, freq=inp["op"].freq, temp=inp["op"].temp),
        arch=Arch(cores=inp["arch"].cores, cacheMB=inp["arch"].cacheMB, memCfg=inp["arch"].memCfg),
        workload=Workload(activity=inp["workload"].activity, memIntensity=inp["workload"].memIntensity),
        scalars=Scalars(ceffMult=inp["scalars"].ceffMult, leakMult=inp["scalars"].leakMult,
                        perfAdj=inp["scalars"].perfAdj),
        leakTable=LeakTable(points=[LeakPoint(voltage=p.voltage, pleak=p.pleak) for p in inp["leak_table"].points]),
        memTable=MemTable(points=[MemPoint(intensity=p.intensity, meff=p.meff) for p in inp["mem_table"].points]),
        vfTable=VfTable(points=[VfPoint(voltage=p.voltage, fmax=p.fmax) for p in inp["vf_table"].points]),
        opsPerTask=inp["ops_per_task"],
        bytesPerTask=inp["bytes_per_task"],
        bwPeak=inp["bw_peak"],
        ipc=inp["ipc"],
        baseCeff=inp["base_ceff"],
        pcap=inp["pcap"],
        tmax=inp["tmax"],
    )


def derived_of(g):
    return {
        "ceff": g.ceff, "pdyn": g.pdyn, "pleak": g.pleak, "meff": g.meff,
        "tcomp": g.tcomp, "tmem": g.tmem, "teff": g.teff, "ptotal": g.ptotal,
        "eop": g.eop, "perfW": g.perfW,
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
    fails = 0
    for name, inp in points().items():
        ref = pm.SoC(**inp).derived()
        got = derived_of(gen_soc(inp))
        for k in ref:
            ok = got[k] == ref[k]
            if not ok:
                fails += 1
            print(f"  [{'PASS' if ok else 'FAIL'}] {name}.{k}: codegen={got[k]!r} oracle={ref[k]!r}")
    print(f"== codegen-vs-oracle: {'ALL MATCH' if fails == 0 else f'{fails} MISMATCHES'} ==")
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
