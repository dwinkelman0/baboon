# studies/power_arch1 under Bazel

The uv instructions in `provenance/README` still apply to the external
checkout (`uv run --project /Users/dwinkelman/Code/baboon-test python
baboon/studies/power_arch1/study_*.py`). This note documents the Bazel
alternative inside this repo — uv instructions are intentionally kept.

## Run the studies with Bazel

```sh
bazel run //studies/power_arch1:study_golden
bazel run //studies/power_arch1:study_freq_sweep
bazel run //studies/power_arch1:study_cores_plus1
bazel run //studies/power_arch1:study_derate
```

`study_golden.py [--freeze]` passes through: `bazel run
//studies/power_arch1:study_golden -- --freeze`.

## Notes

- The studies are stdlib-only, so they run on the hermetic rules_python 3.12
  toolchain from `MODULE.bazel`, whereas the uv checkout pins Python 3.13.
  Only stable stdlib APIs are used, so results are identical.
- Scripts resolve inputs (`models/latest/soc.baboon`, `golden_expected.json`)
  relative to `__file__`; under `bazel run` that is the runfiles tree, and the
  `study_data` filegroup carries those inputs along. Conversely,
  `results_*.json` / `provenance_*.json` outputs are written next to `__file__`,
  i.e. ephemerally under `bazel-bin/` — use the uv flow when you want outputs
  committed back to the source tree.
- `//studies/power_arch1:run_codegen_check` compares codegen output against
  the oracle (imports resolve from `:soc-python`).
