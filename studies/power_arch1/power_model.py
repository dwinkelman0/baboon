"""Hand-lowered mirror of studies/power_arch1/models/latest/soc.baboon (model power.arch1 v1.0.1).

Table lookups are table-owned methods (LeakTable.lookup, MemTable.lookup,
VfTable.contains/fmaxAt); SoC delegates to them. Numerics unchanged from v1.0.0.

Faithful manual lowering of every computed fn-property, used two ways:
  1. Differential oracle: cross-checked against baboon `:python` codegen
     output (generated @property getters) for the golden row.
  2. Execution fallback: if the toolchain build is unavailable, the study
     scripts run against this module so the studies still execute end to end.

Violations are assert-only (AssertionError), never propagated as values.
Interpolation is an explicit for-loop lerp over the embedded tables.
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class OpPoint:
    voltage: float
    freq: float
    temp: float


@dataclass(frozen=True)
class Arch:
    cores: float
    cacheMB: float
    memCfg: str


@dataclass(frozen=True)
class Workload:
    activity: float
    memIntensity: float


@dataclass(frozen=True)
class Scalars:
    ceffMult: float = 1.0
    leakMult: float = 1.0
    perfAdj: float = 1.0


@dataclass(frozen=True)
class LeakPoint:
    voltage: float
    pleak: float


@dataclass(frozen=True)
class MemPoint:
    intensity: float
    meff: float


@dataclass(frozen=True)
class VfPoint:
    voltage: float
    fmax: float


@dataclass(frozen=True)
class LeakTable:
    points: tuple = ()

    def lookup(self, x):
        xs = [p.voltage for p in self.points]
        ys = [p.pleak for p in self.points]
        return _lerp(xs, ys, x)


@dataclass(frozen=True)
class MemTable:
    points: tuple = ()

    def lookup(self, x):
        xs = [p.intensity for p in self.points]
        ys = [p.meff for p in self.points]
        return _lerp(xs, ys, x)


@dataclass(frozen=True)
class VfTable:
    points: tuple = ()

    def contains(self, v):
        return any(p.voltage == v for p in self.points)

    def fmax_at(self, v):
        fmax = self.points[0].fmax
        for p in self.points:
            if p.voltage == v:
                fmax = p.fmax
        return fmax


def _lerp(table_x, table_y, x):
    """Explicit for-loop lerp (mirrors the fn bracket idiom in soc.baboon)."""
    px, py = table_x[0], table_y[0]
    qx, qy = table_x[0], table_y[0]
    hit = False
    for xi, yi in zip(table_x, table_y):
        if xi <= x:
            px, py = xi, yi
        if not hit and xi >= x:
            qx, qy = xi, yi
            hit = True
    span = qx - px
    t = 0.0 if span <= 0.0 else (x - px) / span
    return py + t * (qy - py)


@dataclass
class SoC:
    op: OpPoint
    arch: Arch
    workload: Workload
    scalars: Scalars = field(default_factory=Scalars)
    leak_table: LeakTable = field(default_factory=LeakTable)
    mem_table: MemTable = field(default_factory=MemTable)
    vf_table: VfTable = field(default_factory=VfTable)
    ops_per_task: float = 1.0e9
    bytes_per_task: float = 4.0e9
    bw_peak: float = 50.0e9
    ipc: float = 2.0
    base_ceff: float = 0.5e-9
    pcap: float = 60.0
    tmax: float = 360.0

    # 1. Ceff = base_ceff(cores) x ceff_scale
    @property
    def ceff(self):
        per_core = self.base_ceff * self.scalars.ceffMult
        return per_core * self.arch.cores

    # 2. Pdyn = Ceff x V^2 x f x activity
    @property
    def pdyn(self):
        assert 0.0 < self.workload.activity, "activity must be positive"
        assert self.workload.activity <= 1.0, "activity must be within (0, 1]"
        assert 0.0 < self.scalars.ceffMult, "ceffMult must be positive"
        assert self.scalars.ceffMult <= 1.0, "ceffMult derate must be within (0, 1]"
        v2 = self.op.voltage * self.op.voltage
        vf = v2 * self.op.freq
        return self.ceff * vf * self.workload.activity

    # 3. Pleak = leak_table(V) x leakMult (table-owned lerp)
    @property
    def pleak(self):
        assert 0.0 < self.scalars.leakMult, "leakMult must be positive"
        assert self.scalars.leakMult <= 1.0, "leakMult derate must be within (0, 1]"
        return self.leak_table.lookup(self.op.voltage) * self.scalars.leakMult

    # 4. Meff = meff_table(mem_intensity) (table-owned lerp)
    @property
    def meff(self):
        assert 0.0 <= self.workload.memIntensity, "memIntensity must be non-negative"
        assert self.workload.memIntensity <= 1.0, "memIntensity must be within [0, 1]"
        return self.mem_table.lookup(self.workload.memIntensity)

    # 5. Tcomp
    @property
    def tcomp(self):
        assert 0.0 < self.op.freq, "freq must be positive"
        assert 0.0 < self.arch.cores, "cores must be positive"
        assert 0.0 < self.ipc, "ipc must be positive"
        return self.ops_per_task / (self.op.freq * self.arch.cores * self.ipc)

    # 6. Tmem
    @property
    def tmem(self):
        assert 0.0 < self.bw_peak, "bwPeak must be positive"
        assert 0.0 < self.meff, "meff must be positive"
        return self.bytes_per_task / (self.bw_peak * self.meff)

    # 7. Teff = min roofline
    @property
    def teff(self):
        assert 0.0 < self.tcomp, "tcomp must be positive"
        assert 0.0 < self.tmem, "tmem must be positive"
        return min(self.tcomp, self.tmem)

    # 8. Ptotal + envelope asserts (table-owned V-f lookup)
    @property
    def ptotal(self):
        assert self.op.temp <= self.tmax, "temp exceeds tmax"
        v, f = self.op.voltage, self.op.freq
        assert self.vf_table.contains(v), "voltage not in V-f table"
        fmax = self.vf_table.fmax_at(v)
        assert f <= fmax, "freq exceeds fmax for voltage"
        tot = self.pdyn + self.pleak
        assert 0.0 <= tot, "ptotal must be non-negative"
        assert tot <= self.pcap, "ptotal exceeds pcap"
        return tot

    # 9a. E/op
    @property
    def eop(self):
        assert 0.0 < self.ops_per_task, "opsPerTask must be positive"
        assert 0.0 < self.teff, "teff must be positive"
        return self.ptotal * self.teff / self.ops_per_task

    # 9b. perf/W
    @property
    def perf_w(self):
        assert 0.0 < self.ptotal, "ptotal must be positive"
        assert 0.0 < self.teff, "teff must be positive"
        ops_per_sec = self.ops_per_task / self.teff
        return ops_per_sec / self.ptotal * self.scalars.perfAdj

    def derived(self):
        return {
            "ceff": self.ceff,
            "pdyn": self.pdyn,
            "pleak": self.pleak,
            "meff": self.meff,
            "tcomp": self.tcomp,
            "tmem": self.tmem,
            "teff": self.teff,
            "ptotal": self.ptotal,
            "eop": self.eop,
            "perfW": self.perf_w,
        }


MODEL_VERSION = "power.arch1@1.0.1"
DATA_VERSIONS = {"vf": "vf-v1", "leak": "leak-nominal-v1", "meff": "meff-v1"}

VF_TABLE = (
    VfPoint(0.70, 1.6e9),
    VfPoint(0.80, 2.2e9),
    VfPoint(0.90, 2.8e9),
    VfPoint(1.00, 3.4e9),
)
LEAK_TABLE = (
    LeakPoint(0.70, 1.10),
    LeakPoint(0.80, 1.65),
    LeakPoint(0.90, 2.50),
    LeakPoint(1.00, 3.80),
)
MEM_TABLE = (
    MemPoint(0.00, 0.95),
    MemPoint(0.25, 0.88),
    MemPoint(0.50, 0.75),
    MemPoint(0.75, 0.58),
    MemPoint(1.00, 0.40),
)


def golden_inputs():
    """Nominal point: 4 cores, mid V-f, activity 0.5, mid intensity."""
    return dict(
        op=OpPoint(voltage=0.90, freq=2.8e9, temp=340.0),
        arch=Arch(cores=4.0, cacheMB=8.0, memCfg="ddr5-1ch"),
        workload=Workload(activity=0.5, memIntensity=0.5),
        scalars=Scalars(ceffMult=1.0, leakMult=1.0, perfAdj=1.0),
        leak_table=LeakTable(points=LEAK_TABLE),
        mem_table=MemTable(points=MEM_TABLE),
        vf_table=VfTable(points=VF_TABLE),
        ops_per_task=1.0e9,
        bytes_per_task=4.0e9,
        bw_peak=50.0e9,
        ipc=2.0,
        base_ceff=0.5e-9,
        pcap=60.0,
        tmax=360.0,
    )
