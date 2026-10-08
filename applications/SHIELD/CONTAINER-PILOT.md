# Running SHIELD in a container

The container runs your SHIELD calibrations with a fixed R setup and keeps a
record of exactly which code and data each run used. It runs the same stages as
the usual launcher, including stage 3 with four chains, for a list of cities at a
time. Your own R installation and your usual results are not touched.

In a test on shield2 (October 2026), `calib.10.5.stage0.1x` for Baltimore and
Seattle gave exactly the same results in the container as with the usual
launcher: the same samples, likelihoods, simulations, and outcomes, from the
same code, data, and seed. Two container runs with the same seed were also
identical.

## Get started

On shield2, open an SSH terminal (see the [server guide](Readme.md)). Don't use
the RStudio terminal for long runs: if one job runs out of memory there, others
started from RStudio can be stopped with it.

```bash
alias shield-run=/home/jheem-shared/shield-container-20261008/shield-run.sh
shield-run setup
cd ~/jheem/code/jheem_analyses
```

Setup loads the container the first time and prints your results folder. To keep
the `shield-run` shortcut in later sessions, add the `alias` line to `~/.bashrc`.

**Commit your changes first.** The container uses your committed
`jheem_analyses` code and your committed `jheem2` code (the `jheem2` folder next
to `jheem_analyses`). It refuses to start if either has uncommitted changes.
Local commits are fine; they don't have to be pushed.

## Run a list of cities

```bash
shield-run batch C.12580,C.35620,C.12060 calib.10.5.stage0 calib.10.5.stage1 calib.10.5.stage2 calib.10.5.stage3
```

This runs the stages in order for each city, each stage after the previous one
finishes. Five cities run at a time by default; set `SHIELD_MAX_CITIES` to change
that, for example `SHIELD_MAX_CITIES=8 shield-run batch ...`. Allow about 12 GB
of memory for a city in stages 0–2, and about 40 GB for a city in stage 3, whose
four chains run at the same time.

For one city, use `shield-run pipeline C.12580 calib.10.5.stage0 calib.10.5.stage1`.

Starting takes a few minutes: the container saves a copy of your code, checks
it, and the first run with a new `jheem2` version prepares it. Runs keep going
after you log out.

## Check progress

```bash
shield-run status
shield-run logs C.12580 calib.10.5.stage3
```

`status` shows each batch and city, and how many checkpoints each chain has
saved. Stage 3 chains also write their own logs, under
`run_records/shield/<city>/<calibration>/logs/` in your results folder.

## Stop and continue

```bash
shield-run stop-batch <batch-id>      # the ID is shown when the batch starts and in status
shield-run stop C.12580 calib.10.5.stage3
```

Run the same `batch` or `pipeline` command again to continue: finished stages
are checked and skipped, and the others continue from their last checkpoint
(within about 30 minutes of where they stopped). A continued run is a valid
calibration, but it is not sample-for-sample identical to one that was never
stopped.

## Use the results

Results are saved in your results folder in the usual layout (`mcmc_runs/`,
`mcmc_summaries/`, `simulations/`). To read them with your usual analysis
scripts, run

```bash
shield-run where
```

and copy the `Sys.setenv(JHEEM_ROOT_DIR = ...)` line it prints for your computer
(server, Mac, or Windows) to the top of your script, before it sources the
SHIELD code. Remove that line to go back to your usual results.

## If something goes wrong

- **A stage fails during setup:** fix the problem, register the calibration under
  a new code, and run the batch again with the new code. Finished earlier stages
  in your results folder are reused.
- **Anything else:** run `shield-run status` and `shield-run logs <city>
  <calibration>`, and share the output with the server name and the batch ID.
- Keep the files of failed runs; don't delete them to force a restart.
- The same calibration code shouldn't be run both the usual way and in the
  container in the same results folder.

To check or continue a run started with an earlier container installation, keep
using that installation's `shield-run.sh`.

For implementation details, see the [recorded-runtime reference](RECORDED-RUN-PILOT.md).
