# Running SHIELD in a container

The container provides the R setup for running SHIELD and keeps a record of the
code and data used. Your usual R installation and calibration outputs are unchanged.

**Currently supported:** single-chain calibrations and sequential stages 0–2.
Multi-chain stage 3 and transferring container results into a native stage-3 run
are not supported yet. Use the usual workflow for full calibrations.

## Get started

On a server with the container installed, open an SSH terminal (see the
[server guide](Readme.md)), rather than the R console or RStudio web terminal:

```bash
alias shield-run=/home/jheem-shared/shield-container/shield-run.sh
shield-run setup
# Open your jheem_analyses checkout before starting a new calibration.
cd /path/to/jheem_analyses
```

Setup loads the image on first use and prints your output directory. You don't
need another repository checkout or any R package installation.

The first start of a calibration code saves a copy of your committed analysis
code. Commit any edits first; local commits don't have to be pushed. New
calibration codes pick up your updated checkout without rebuilding the container.
Later locations and resumes for the same code use its saved copy. A pipeline
saves one copy for all its stages, even if you update the checkout while it runs.

To rerun the same calibration code with different code or inputs, use a separate
output directory. Package or engine changes may still need an updated container.
Manager versions are shown during startup and saved with the run; updating your
checkout does not select newer data. Confirm those versions before an analysis
run. A startup check catches some source/package incompatibilities, not all model
errors or scientific differences.

## Try a small run

```bash
shield-run start C.12580 container.smoke.stage0
shield-run status
shield-run logs C.12580 container.smoke.stage0
```

This is a two-iteration Baltimore test, usually taking a few minutes. Run
`status` again to check progress. When it finishes, expect `exited (exit 0)`
and `outputs recorded (not rechecked)`. The latter means output records exist;
the status command doesn't check their contents. This test isn't for analysis.

## Stop and continue

For a longer calibration defined in your selected source, for example:

```bash
shield-run start C.12580 calib.9.28.stage0
shield-run status
shield-run logs C.12580 calib.9.28.stage0
```

Jobs keep running when you log out after successful setup. To stop and resume:

```bash
shield-run stop C.12580 calib.9.28.stage0
shield-run resume C.12580 calib.9.28.stage0
```

Wait for at least one saved checkpoint before practicing this. Resume repeats
work since the last checkpoint. `start` won't replace an existing run.

To run stages in order:

```bash
shield-run pipeline C.12580 calib.9.28.stage0 calib.9.28.stage1 calib.9.28.stage2
```

Repeat the same command to continue a stopped pipeline. It checks completed
outputs before skipping them and stops if a stage fails. Stopping any listed
stage stops the whole pipeline. A stage interrupted before its first checkpoint
cannot resume; preserve it and use a new output directory for a new attempt.

## Results and troubleshooting

Results go to `/mnt/jheem_nas_share/tmp/shield-container/<your username>/`,
under `mcmc_runs/`, `mcmc_summaries/`, and `simulations/`. The `run_records/`
directory holds the code/data versions and start/resume history. `run_sources/`
holds the saved code and the selections shared across locations. Keep these
directories together and don't share one run directory between accounts.

If setup fails, a run exits with a nonzero code, or resume refuses to start,
share the message, server name, output directory, and `status`/`logs` output.
Keep the existing files, including failed runs; don't delete them to force a
restart. A run stopped before its first checkpoint may need a separate output
directory for a new attempt.

For implementation details, see the [recorded-runtime reference](RECORDED-RUN-PILOT.md).
