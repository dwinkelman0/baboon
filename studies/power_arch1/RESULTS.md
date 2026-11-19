# power_arch1 — execution record

Executed end-to-end per `PLAN.md` (this directory). Model: `power.arch1@1.0.0`
(`soc.baboon`), lowered with the baboon toolchain (`:python` codegen) to
`pygen/`, executed in Python, verified against golden + tolerances.

## Artifacts

| File | Role |
|---|---|
| `soc.baboon` | Study-agnostic schema: `SoC` root (OpPoint/Arch/Workload/Scalars inputs, `lst[Point]` embedded tables, 10 computed fn-properties, assert-only constraints) |
| `power_model.py` | Faithful hand-lowering of the schema; differential oracle + execution fallback |
| `study_golden.py` | Golden-row freeze + verify (`golden_expected.json`) |
| `study_freq_sweep.py` | Study 1: f/V sweep at fixed cores/activity |
| `study_cores_plus1.py` | Study 2: N vs N+1 cores (file is `study_cores_plus1.py`) |
| `study_derate.py` | Study 3: ceff/leak derate < 1 |
| `study_common.py` | Tolerance checks (`1e-9` power/energy, `1e-6` perfW, exact branch equality) + provenance emission |
| `run_codegen_check.py` | Differential check: generated classes vs oracle (exact match) |
| `results_*.json` | Per-study derived outputs |
| `provenance_*.json` | `{inputs, model_version, data_versions, results_hash}` per study |
| `pygen/` | Toolchain `:python` output for `power.arch1` (checked in as lowering evidence) |

## Results (all passing)

- Golden row (4 cores, 0.90V/2.8GHz, α=0.5, mi=0.5): 11/11 — Pdyn=2.268W,
  Pleak=2.50W, Ptotal=4.768W, Tcomp=0.044643s (compute-bound), E/op=2.129e-10J,
  perfW=4.698e9 /s/W.
- Study 1 freq sweep: 9/9 — Pdyn ratios match V²f exactly, Pleak monotonic,
  teff==min branch equality at all 4 points, memory→compute crossover observed
  (sweep-local bytesPerTask=2.4e9 so Tmem=0.064s lands inside the Tcomp range).
- Study 2 cores+1: 7/7 — Tcomp ∝ 1/N, Pdyn ∝ N, Pleak V-driven (unchanged),
  speedup 1.25 (linear here: point stays compute-bound), bound stable.
- Study 3 derate: 27/27 — Pdyn/Pleak proportional to ceff/leakMult, all timing
  bit-identical, Ptotal consistent, E/op and perfW scale with power.
- Codegen-vs-oracle: exact match on golden + sweep/cores/derate points.
- Assert negatives (all fire): activity>1, ceffMult>1, V∉V-f table, f>fmax,
  temp>tmax.

## Toolchain fixes (minimal, commented in-code)

1. `BaboonTranslator`: fn-assembly owner stub now carries the full sibling
   field list (was: only the field under conversion, so multi-field fn bodies
   never resolved); `Self` in fn params/captures rewrites to the owner type;
   the rewritten fn is what enters `FnSiteInputs` (was: original passed through).
2. `FnDepGraphBuilder`: shape-only self-overlap no longer edges (for-in/match
   over an own-DTO collection put the owner in shapeReads, self-cycling every
   such fn); same-slot self-reads remain hard errors.

No new builtins; interpolation stays an explicit for-loop lerp in-schema.
Scala regression: `FnSyntaxParserTest`, `FnPipelineTest`, `FnOrderingTest`,
`FnBodyTypingTest` via `sbt testOnly` (see dispatch log for verdict).
