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
alias shield-run=/home/jheem-shared/shield-container-20261019/shield-run.sh
shield-run setup
cd ~/jheem/code/jheem_analyses
```

Setup loads the container the first time and prints your results folder. To keep
the `shield-run` shortcut in later sessions, add the `alias` line to `~/.bashrc`.

**Data managers.** SHIELD's data managers are listed in
`applications/SHIELD/data_managers.csv`, and both the usual launcher and the
container load exactly those releases. To change one, edit its release there and
commit. In the container, a calibration that already started keeps the managers
it started with; with the usual launcher, keep the same releases across a
calibration's stages yourself.
Census is `unpinned` for now: the usual launcher uses the census named by the
shared cached-metadata file, and the container uses the October 8 release
(`census-manager-v2026.10.08`, currently the same file). The first run with a new
release downloads it once and checks it; releases are kept on the NAS in
`data_manager_releases`.

**Commit your changes first.** The container uses your committed
`jheem_analyses` code and your committed `jheem2` code (the `jheem2` folder next
to `jheem_analyses`). It refuses to start if either has uncommitted changes.
Local commits are fine; they don't have to be pushed.

## Try it first

A short test run (about 10 minutes, two cities, ending with four chains) shows the
whole cycle without using real calibration codes:

```bash
shield-run batch C.12580,C.35620 container.smoke.stage0 container.smoke.pre3 container.smoke.stage3
```

Its results are tiny and not meant for analysis.

## Run a list of cities

```bash
shield-run batch C.12580,C.35620,C.12060 calib.10.15.stage0 calib.10.15.stage1 calib.10.15.stage2 calib.10.15.stage3
```

This runs the stages in order for each city, each stage after the previous one
finishes. Five cities run at a time by default; set `SHIELD_MAX_CITIES` to change
that, for example `SHIELD_MAX_CITIES=8 shield-run batch ...`. Allow about 12 GB
of memory for a city in stages 0–2, and about 40 GB for a city in stage 3, whose
four chains run at the same time.

For one city, use `shield-run pipeline C.12580 calib.10.15.stage0 calib.10.15.stage1`.

Starting takes a few minutes: the container saves a copy of your code, checks
it, and the first run with a new `jheem2` version prepares it. Runs keep going
after you log out.

## Check progress

```bash
shield-run status
shield-run logs C.12580 calib.10.15.stage3
```

`status` shows each batch and city, and how many checkpoints each chain has
saved. `logs` shows the latest output of each part of a stage (setup, each chain,
and the final assembly).

Everything is also saved in your results folder, so someone helping you can read
it on the server without copying output: what each `shield-run` command printed
is in `run_logs/`, and each stage's output is under
`run_records/shield/<city>/<calibration>/logs/`.

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

- **The check before starting fails:** nothing was started. The message ends
  with the file that has the full output.
- **A new stage after earlier ones already ran** (for example, you registered
  `calib.10.15.stage3b` to continue from stage 2): commit it and run only the new
  code, `shield-run batch C.12580,C.35620 calib.10.15.stage3b`. Listing the
  finished stages too makes the container check the new code against the code
  those stages ran with, which fails; the message says so. Finished earlier stages
  in your results folder are found and reused.
- **A stage fails during setup:** fix the problem, register the calibration under
  a new code, and run only the new code as above.
- **"Batch ... is still running":** the same cities and stages are already running
  in another batch. Wait for it, or stop it with `shield-run stop-batch <id>`, then
  run the command again.
- **Anything else:** run `shield-run status` and `shield-run logs <city>
  <calibration>`, and share the output with the server name and the batch ID.
- Keep the files of failed runs; don't delete them to force a restart.
- The same calibration code shouldn't be run both the usual way and in the
  container in the same results folder.

To check or continue a run started with an earlier container installation, keep
using that installation's `shield-run.sh`.

## Commands

| Command | What it does |
|---|---|
| `shield-run setup` | Checks the server and loads the container; prints your results folder. |
| `shield-run batch <city,city,...> <code>...` | Runs the stages in order for each city, a few cities at a time. Run it again to continue. |
| `shield-run pipeline <city> <code>...` | The same for one city. |
| `shield-run start <city> <code>` | Starts one stage for one city. |
| `shield-run resume <city> <code>` | Continues one stage from its last checkpoint. |
| `shield-run status` | Shows your batches, cities, stages, and checkpoints. |
| `shield-run logs <city> <code>` | Shows a stage's latest output. |
| `shield-run stop <city> <code>` | Stops one running stage or pipeline. |
| `shield-run stop-batch <batch-id>` | Stops a batch and its running cities. |
| `shield-run where` | Prints your results folder and how to read it from analysis scripts. |

Settings go before the command, for example `SHIELD_MAX_CITIES=8 shield-run batch ...`:
`SHIELD_MAX_CITIES` (cities of a batch at once, default 5) and
`SHIELD_MAX_PARALLEL_CHAINS` (chains of one stage at once, default all). Leave the
others at their defaults.

For implementation details, see the [recorded-runtime reference](RECORDED-RUN-PILOT.md).
