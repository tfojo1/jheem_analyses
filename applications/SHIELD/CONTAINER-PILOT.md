# Trying the SHIELD container

The container runs SHIELD with a fixed set of code, R packages, and input data,
without changing your usual R setup. It can continue from saved checkpoints
and records which versions produced each result.

**This is a pilot, not a replacement for your next calibration.** Single-chain
stages are supported; multi-chain stage 3 is not. Transferring pilot results
into the usual stage-3 workflow has not been validated either. Please continue
using the usual workflow for full calibrations; don't wait for the container.

## Before trying it

Ask the administrator to confirm that **shield2 and your account** are ready.
The installation and account checks are separate from the code being available
in this repository. You don't need to clone the container repository, build an
image, install R packages, or change your analysis checkout.

The retained October 1 pilot uses analyses revision `06505412` and the July 27
syphilis manager. It does **not** include the subsequent STI-screening modifier
change (`3f9e2019`) or transmission-prior change (`207672f2`). Updating your
checkout does not update the code inside the container. This snapshot is for
trying the mechanics; agree on the code and inputs before a scientific run.

Connect to shield2 over SSH, using the [server guide](Readme.md) if needed.
Run the commands below in that SSH terminal, not the R console or RStudio's
web terminal:

```bash
alias shield-run=/home/jheem-shared/shield-container/shield-run.sh
shield-run setup
```

Setup loads the prepared image into your account the first time (a few minutes)
and prints the output directory. If it reports a missing prerequisite, send that
message to the administrator rather than trying to install or configure it.

## Start with the small test

```bash
shield-run start C.12580 container.smoke.stage0
shield-run status
shield-run logs C.12580 container.smoke.stage0
```

This runs a two-iteration Baltimore test using the real model. Allow a few
minutes; it is an execution check, not a useful scientific calibration. Run
`status` again to check progress. Success appears as `exited (exit 0)` and
`outputs recorded (not rechecked)`. The latter means output records exist;
`status` doesn't revalidate the files. An exit other than zero needs inspection.

The job continues when you log out once the administrator has enabled that
account setting. `start` refuses to replace existing results, so don't repeat
it to restart a test that already ran. Keep the results and ask for help if you
want another clean attempt.

## Where the results are

The default pilot directory is
`/mnt/jheem_nas_share/tmp/shield-container/<your username>/`. It is separate
from the team's ordinary calibration outputs. It contains the usual
`mcmc_runs/`, `mcmc_summaries/`, and `simulations/` directories, plus
`run_records/shield/<location>/<calibration>/` recording code/data versions,
output locations, and each start or resume.

Keep the whole run directory together. If something goes wrong, send the
server name, calibration code, output directory, and the output of `status`
and `logs`. Please preserve failed runs and their records; don't delete files
to get past a refusal to start or resume.

## A longer exercise, when agreed

The retained image includes `calib.9.28.stage0`. To try stopping and continuing
that stage, agree on the server load and snapshot first, then:

```bash
shield-run start C.12580 calib.9.28.stage0
shield-run status
# Wait until status shows at least one saved checkpoint before stopping.
shield-run stop C.12580 calib.9.28.stage0
shield-run resume C.12580 calib.9.28.stage0
```

Checkpoints are normally saved every 500 iterations; the earlier server trial
took roughly half an hour per checkpoint and many hours for the full stage.
Work since the last checkpoint is redone on resume. These timings are only
guidance, not a performance comparison. If setup stopped before any checkpoint,
preserve that run and ask for help rather than retrying `start` on it.

Single-chain stages can also run in sequence:

```bash
shield-run pipeline C.12580 calib.9.28.stage0 calib.9.28.stage1 calib.9.28.stage2
```

Repeating the same pipeline verifies completed outputs before skipping them,
resumes a checkpointed stage, and starts remaining stages. A failure stops the
sequence. `stop` with any listed stage stops the whole pipeline. Keep the same
output directory and inputs when continuing. Don't point two accounts at the
same run directory; the wrapper cannot detect another account's containers.

The pilot does not yet establish identical results after interruption or
across servers. Its purpose here is to test operation, checkpoint continuation,
and traceable outputs. Installation and maintenance details are in the
[administrator runbook](https://github.com/ncsizemore/jheem-containers/blob/main/workloads/shield/RUNBOOK.md);
the [recorded-runtime reference](RECORDED-RUN-PILOT.md) describes the source interface.
