# power.arch1 model v1.0.0

Checked-in snapshot of the study-agnostic schema for version tracking.

- Model: `power.arch1@1.0.0` (`version "1.0.0"` in `soc.baboon`)
- Source schema: `studies/power_arch1/soc.baboon` (copied here as `soc.baboon`)
- Golden row: `golden_expected.json` (copied from `studies/power_arch1/golden_expected.json`)
- Data (table) versions: vf=`vf-v1`, leak=`leak-nominal-v1`, meff=`meff-v1`
  (see `MODEL_VERSION` / `DATA_VERSIONS` in `studies/power_arch1/power_model.py`)
- Resolve via `studies/power_arch1/models/latest` symlink (points at `v1.0.0`).

Codegen outputs (`pygen/`, `generated/`, `codegen/`, `__pycache__/`, `*.pyc`)
are git-ignored and never checked in; this directory holds only versioned inputs.
