# power.arch1 model v2.0.0

Checked-in snapshot of the study-agnostic schema for version tracking.

- Model: `power.arch1@2.0.0` (`version "2.0.0"` in `soc.baboon`)
- Golden row: `golden_expected.json` (numerics unchanged from v1.0.1/v1.0.0)
- Data (table) versions: vf=`vf-v1`, leak=`leak-nominal-v1`, meff=`meff-v1`
  (see `MODEL_VERSION` / `DATA_VERSIONS` in `studies/power_arch1/power_model.py`)
- Resolve via `studies/power_arch1/models/latest` symlink (points at `v2.0.0`).
- Change since v1.0.1: typed violations + provenance specials; no numeric change.
  - `adt PowerViolation #Assertion` (variants `OverVoltage` / `TempExceed` /
    `PcapExceed`): the single per-package `#Assertion` special. An assert
    message may be `str` (unchanged default) or this type; `SoC.viol` carries
    the payload and `SoC.ptotal`'s temp assert uses it (`assert(..., self.viol)`).
  - `data RunProvenance #Provenance` (every field `@default`): the single
    per-package `#Provenance` special; inert provenance record.
  - Specials are implicitly root (no `root` keyword needed) and at most one
    of each kind per package (typer-enforced, `DuplicateSpecial`).

Codegen outputs (`pygen/`, `generated/`, `codegen/`, `__pycache__/`, `*.pyc`)
are git-ignored and never checked in; this directory holds only versioned inputs.
