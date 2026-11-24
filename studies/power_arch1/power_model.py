"""Thin facade over baboon codegen for power.arch1 (models/latest, v2.0.0).

Single source of truth is the generated package `power.arch1.*` (lowered
from studies/power_arch1/models/latest/soc.baboon via the `:soc-python`
target). This module re-exports the generated types so study code keeps a
single import (`import power_model as pm`), and holds only study-side
fixtures that are not in-schema: the nominal tables, golden_inputs(), the
derived() key set, and harness version strings. No model classes, no
numerics live here.
"""

from __future__ import annotations

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

__all__ = [
    "DATA_VERSIONS",
    "LEAK_TABLE",
    "MEM_TABLE",
    "MODEL_VERSION",
    "VF_TABLE",
    "Arch",
    "LeakPoint",
    "LeakTable",
    "MemPoint",
    "MemTable",
    "OpPoint",
    "OverVoltage",
    "PcapExceed",
    "PowerViolation",
    "RunProvenance",
    "Scalars",
    "SoC",
    "TempExceed",
    "VfPoint",
    "VfTable",
    "Workload",
    "derived",
    "golden_inputs",
]


MODEL_VERSION = "power.arch1@2.0.0"
DATA_VERSIONS = {"vf": "vf-v1", "leak": "leak-nominal-v1", "meff": "meff-v1"}

VF_TABLE = (
    VfPoint(voltage=0.70, fmax=1.6e9),
    VfPoint(voltage=0.80, fmax=2.2e9),
    VfPoint(voltage=0.90, fmax=2.8e9),
    VfPoint(voltage=1.00, fmax=3.4e9),
)
LEAK_TABLE = (
    LeakPoint(voltage=0.70, pleak=1.10),
    LeakPoint(voltage=0.80, pleak=1.65),
    LeakPoint(voltage=0.90, pleak=2.50),
    LeakPoint(voltage=1.00, pleak=3.80),
)
MEM_TABLE = (
    MemPoint(intensity=0.00, meff=0.95),
    MemPoint(intensity=0.25, meff=0.88),
    MemPoint(intensity=0.50, meff=0.75),
    MemPoint(intensity=0.75, meff=0.58),
    MemPoint(intensity=1.00, meff=0.40),
)

DERIVED_KEYS = (
    "ceff",
    "pdyn",
    "pleak",
    "meff",
    "tcomp",
    "tmem",
    "teff",
    "ptotal",
    "eop",
    "perfW",
)


def derived(soc):
    """Study-side derived-output dict (not in-schema; key order pins golden)."""
    return {k: getattr(soc, k) for k in DERIVED_KEYS}


def golden_inputs():
    """Nominal point: 4 cores, mid V-f, activity 0.5, mid intensity."""
    return dict(
        op=OpPoint(voltage=0.90, freq=2.8e9, temp=340.0),
        arch=Arch(cores=4.0, cacheMB=8.0, memCfg="ddr5-1ch"),
        workload=Workload(activity=0.5, memIntensity=0.5),
        scalars=Scalars(ceffMult=1.0, leakMult=1.0, perfAdj=1.0),
        leakTable=LeakTable(points=list(LEAK_TABLE)),
        memTable=MemTable(points=list(MEM_TABLE)),
        vfTable=VfTable(points=list(VF_TABLE)),
        opsPerTask=1.0e9,
        bytesPerTask=4.0e9,
        bwPeak=50.0e9,
        ipc=2.0,
        baseCeff=0.5e-9,
        pcap=60.0,
        tmax=360.0,
        viol=TempExceed(temp=340.0, tmax=360.0),
    )
