# Study PLAN — SoC Power/Performance (power_arch1)

> NOTE (path): dispatch asked for `baboon/studies/power_arch1/PLAN.md` repo-relative,
> but `<repo>/baboon` already exists as an executable script file (dev launcher),
> so `<repo>/baboon/studies/...` is uncreatable (ENOTDIR). This plan is written at
> `<repo>/studies/power_arch1/PLAN.md` instead — i.e. `baboon/studies/...` relative
> to the parent checkout dir. Move it once a `baboon/` directory exists.

## Scope

Model: system-on-chip power + performance per operation.

Dimensions:
- Cores (count / shared vs private)
- Shared memory subsystem
- Voltage-frequency (V-f) operating points
- Workload activity factor (α)
- Memory intensity (comp-vs-mem bound mix)
- Leakage tables (per corner / temperature)
- Memory-efficiency tables (bandwidth / efficiency vs intensity)

Out of scope: thermal feedback, DVFS control policy, process variation statistics.

## Input categories → schema mapping

Four logical input categories, one DTO pattern (no four types):

1. **Hot** (per-sweep / per-point): `freq`, `voltage`, `activity`, `mem_intensity`.
2. **Cold** (config per study): `cores`, `v_f_table_id`, `leak_table_id`, `meff_table_id`.
3. **Scalars** (quasi-static tuning): `ceff_scale`, `leak_mult` (< 1 derate studies).
4. **Tables** (versioned data in-schema): V-f table, leakage table, mem-efficiency table.

Mapping rule: hot + cold + scalars are fields on a single study-input DTO; tables are in-schema data + embedded instances referenced by table ID. No separate Hot/Cold/Scalar/Table DTO types.

## Derived chain (fn)

Order (fn raise-to-IR, bottom-up free + captures):

1. `Ceff = base_ceff(cores) × ceff_scale` — effective switched capacitance.
2. `Pdyn = Ceff × V² × f × α` — dynamic power.
3. `Pleak = leak_table(V, temp/corner) × leakMult` — leakage power.
4. `Meff = meff_table(mem_intensity)` — memory efficiency.
5. `Tcomp = ops_per_task / (f × cores × ipc)` — compute time.
6. `Tmem = bytes_per_task / (bw_peak × Meff)` — memory time.
7. `Teff = min(Tcomp, Tmem)` — roofline effective time (compute/memory bound).
8. `Ptotal = Pdyn + Pleak` — total power.
9. `E/op = Ptotal × Teff / ops` and `perf/W = ops / (Ptotal × Teff)` — energy per op, performance per watt.
10. **Asserts**: `V ∈ v_f_table`, `f ≤ fmax(V)`, `0 < α ≤ 1`, `0 < ceff_scale, leak_mult ≤ 1` (for derate studies), `Ptotal ≥ 0`, `Teff > 0`. Violations assert, never propagate.

## Golden row

One checked-in golden row (nominal point: e.g. 4 cores, mid V-f, α=0.5, mid intensity):

- Inputs: full hot+cold+scalar set + table versions.
- Expected: `Pdyn`, `Pleak`, `Ptotal`, `Tcomp`, `Tmem`, `Teff`, `E/op`, `perf/W`.
- Tolerances: relative `1e-9` for power/energy (exact table lerp), `1e-6` for perf/W division chain; exact equality for bound-selection (`Teff == min` branch).

## Three studies

1. **Freq sweep** — sweep `f` (and paired `V` from V-f table) at fixed cores/activity; expect `Pdyn ∝ V²f`, `Teff` crosses comp→mem bound.
2. **Cores+1** — N vs N+1 cores at fixed V-f; expect sublinear speedup when mem-bound, `Pleak` scales with core count.
3. **Ceff+leak scalars < 1** — derate `ceff_scale`, `leak_mult` below 1 (e.g. 0.9/0.8); expect proportional `Pdyn`/`Pleak` drop, timing unchanged.

Each study: inputs JSON + expected JSON + provenance JSON, checked in under this directory.

## End-to-end execution (steps 1–9 via Python lowering)

1. Define schema (containers, tables, derived fns, asserts).
2. Write study inputs (hot/cold/scalars + table IDs).
3. Raise fn to IR (captures, star-free; topo check).
4. Lower to Python (computed-no-wire, per-backend exclusion).
5. Instantiate discrete SoC instances in a loop — no Study DTO; harness loops over plain input dicts.
6. Run derived chain per instance.
7. Collect outputs (`Pdyn`, `Pleak`, `Ptotal`, `Tcomp`, `Tmem`, `Teff`, `E/op`, `perf/W`).
8. Diff against golden/expected with tolerances.
9. Emit provenance JSON: inputs / model version / data (table) versions / results.

## Outputs + provenance

- `results.json`: per-point derived outputs.
- `provenance.json`: `{ inputs, model_version, data_versions: { v_f, leak, meff }, results_hash }` — manual JSON for now.

## Container-self simplification audit (2026-11-20)

- All `= fn` computed use `self: Self` = enclosing container (`SoC`) with
  sibling reads (`self.op.*`, `self.arch.*`, `self.scalars.*`, tables).
  No fn treats `self` as a field value — compliant, no change needed.
- Captures: self-only fns, zero extra captures (single-capture max satisfied
  vacuously). No capture paths to narrow (`s: SoC.arch.cores.<field>` style
  absent). No explicit `.Some`, no multi-capture, no `!` — nothing to drop.
- Collections transparent: `for (p in self.leakTable / self.memTable /
  self.vfTable)` iterates the collection field directly — compliant.
- Parameterized child methods (curve lookup with an `x` argument): kept as
  host-callable Python methods in `power_model.py` / study harness (Python
  oracle). Thin `SoC` delegation stays host-side until intra-fn user-method
  calls land (parser whitelist rejects them — banned, noted as gap, not
  attempted). PENDING: re-lower lerp/lookup as intra-fn calls once supported.

## What stays manual

- **Provenance JSON**: hand-written/stamped; no framework provenance record yet.
- **Sweep harness**: plain Python loop over discrete SoC instances; no first-class Study/sweep construct.
- **Study deltas**: diffing studies is manual file diff; golden diff harness is a gap (see `plans/gaps-and-principles.md`).
- **Table interpolation**: `lerp` for between-point lookups hand-rolled until interpolation builtins land.
