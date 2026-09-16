# SHIELD runtime and calibration contract

This document is the canonical operational contract for running SHIELD from
source or from a container. It describes the current source entrypoints; a
container or scheduler may wrap them, but must preserve these semantics.

## Design rules

- Source code and dependencies are prepared before the run starts. SHIELD does
  not fetch Git branches, reset repositories, or install R packages at runtime.
- Runtime directories are explicit. Machine names and a particular checkout
  layout are not part of the contract.
- A fresh run is destructive and must be requested explicitly. Resume is the
  default and fails if no checkpoint exists.
- Model or configuration failures are not retried. Only a narrow set of
  transient storage failures may be retried, with a finite attempt limit.
- Failed or incomplete calibrations are not assembled unless the caller
  explicitly opts in.
- Recorded work must use immutable input identifiers and retain the generated
  provenance receipt with its outputs.

## Required paths

| Variable | Meaning | Access during a run |
| --- | --- | --- |
| `JHEEM_ANALYSES_PATH` | Exact `jheem_analyses` source tree | Read-only is sufficient |
| `JHEEM2_PATH` | Exact `jheem2` source tree used for source loading and provenance | Read-only is sufficient |
| `JHEEM_ROOT_DIR` | Calibration checkpoints, simulations, and provenance receipts | Read-write |
| `JHEEM_CACHE_DIR` | Cached/versioned input-manager objects | Read-only when offline; read-write when online |

`JHEEM_ROOT_DIR` and `JHEEM_CACHE_DIR` are always required. The source paths
currently retain legacy sibling-directory defaults for interactive development,
but automation and containers must set them explicitly.

## Runtime controls

| Variable | Default | Contract |
| --- | --- | --- |
| `JHEEM2_MODE` | `source` | `source` loads `JHEEM2_PATH` with `pkgload`; `package` uses the already installed package |
| `JHEEM_ANALYSES_REF` | unset | Full baked `jheem_analyses` commit declared by an immutable image when no Git metadata is included |
| `JHEEM2_REF` | unset | Full baked `jheem2` commit declared by an immutable image when no Git metadata is included |
| `SHIELD_RUN_MODE` | `resume` | `fresh` clears matching prior state; `resume` requires a checkpoint |
| `SHIELD_INPUT_OFFLINE` | `true` | Prevent input-manager network refresh during the run |
| `SHIELD_REQUIRE_IMMUTABLE_INPUTS` | `false` | When true, exact census- and syphilis-manager tags are mandatory |
| `JHEEM_CENSUS_MANAGER_TAG` | unset | Exact version tag for the census input manager |
| `JHEEM_SYPHILIS_MANAGER_TAG` | unset | Exact version tag for the syphilis input manager |
| `SHIELD_ALLOW_INCOMPLETE` | `false` | Permit assembly from incomplete chains only when explicitly true |
| `SHIELD_MAX_ATTEMPTS` | `1` | Total attempts, including the first attempt |
| `SHIELD_RETRY_DELAY_SECONDS` | `30` | Delay between recognized transient-storage retries |
| `SHIELD_RANDOM_SEED` | `0` | Base random seed; modular chains add `chain - 1` |
| `SHIELD_CACHE_FREQUENCY` | `500` | Calibration checkpoint frequency |
| `SHIELD_UPDATE_FREQUENCY` | `50` | Calibration status update frequency |
| `SHIELD_RUN_ID` | UTC timestamp plus process ID | Caller-provided stable identifier for logs and provenance events |

Boolean values accept `true`, `false`, `1`, `0`, `yes`, or `no`. Invalid
values and invalid integer settings fail before calibration starts.

## Entry points

Run a single chain from setup through assembly:

```bash
Rscript applications/SHIELD/shield_calib_setup_and_run.R \
  C.12580 shield_calibration_stage
```

Run modular setup, chain execution, or assembly:

```bash
Rscript applications/SHIELD/shield_calib_setup_and_run_modular.R \
  C.12580 shield_calibration_stage setup
Rscript applications/SHIELD/shield_calib_setup_and_run_modular.R \
  C.12580 shield_calibration_stage run 1
Rscript applications/SHIELD/shield_calib_setup_and_run_modular.R \
  C.12580 shield_calibration_stage assemble
```

The modular `all` stage performs setup, one selected chain, and assembly in one
process. Never run two writers for the same location, calibration code, and
chain. The caller or scheduler is also responsible for assigning a unique log
file and preventing duplicate concurrent launches.

## Development example

Use disposable output state while reusing a local input cache:

```bash
export JHEEM_ANALYSES_PATH="$HOME/jheem/code/jheem_analyses"
export JHEEM2_PATH="$HOME/jheem/code/jheem2"
export JHEEM_ROOT_DIR="$HOME/jheem/runs/shield-dev-001"
export JHEEM_CACHE_DIR="$HOME/jheem/cache"
export JHEEM2_MODE=source
export SHIELD_INPUT_OFFLINE=true
export SHIELD_RUN_MODE=fresh
export SHIELD_RUN_ID=shield-dev-001
mkdir -p "$JHEEM_ROOT_DIR"

Rscript applications/SHIELD/shield_calib_setup_and_run_modular.R \
  C.12580 shield_calibration_stage all 1
```

Subsequent continuation of the same checkpoint must change
`SHIELD_RUN_MODE` to `resume`. Do not use `fresh` against state that must be
preserved.

## Recorded-run requirements

For a result intended to be retained, compared, or published:

1. Use exact source commits and a prebuilt dependency environment. A recorded
   image without embedded Git metadata must declare full `JHEEM_ANALYSES_REF`
   and `JHEEM2_REF` commits; provenance reports a mismatch if a declared ref
   disagrees with an available worktree.
2. Set `SHIELD_INPUT_OFFLINE=true`.
3. Set `SHIELD_REQUIRE_IMMUTABLE_INPUTS=true` and provide exact
   `JHEEM_CENSUS_MANAGER_TAG` and `JHEEM_SYPHILIS_MANAGER_TAG` releases
   available in `JHEEM_CACHE_DIR`.
4. Give the run a durable, unique `SHIELD_RUN_ID` and output directory.
5. Preserve the output directory, including its provenance receipt and logs.
6. Treat an incomplete run as failed; do not set `SHIELD_ALLOW_INCOMPLETE=true`
   for a recorded result.

The current source entrypoints enforce the critical failure behavior, but they
do not provide a cross-process lock or create log files. Those remain the
responsibility of the container/scheduler wrapper.

## Source-level validation

The runtime-policy tests do not require model inputs:

```bash
Rscript commoncode/tests/test_shield_runtime.R
Rscript commoncode/tests/test_run_provenance.R
Rscript commoncode/tests/test_data_manager_release_selection.R
```

The integration test runs a real median-parameter simulation and therefore
requires compatible cached managers:

```bash
Rscript applications/SHIELD/shield_engine_test.R
```

`SHIELD_TEST_LOCATION` and `SHIELD_TEST_END_YEAR` may override its defaults of
`C.12580` and `2030`.
