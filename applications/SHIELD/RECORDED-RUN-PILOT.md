# Recorded SHIELD calibration path (pilot)

For the prepared server pilot, use [Trying the SHIELD container](CONTAINER-PILOT.md).
This page is the technical reference for the recorded runtime, not the operator
setup procedure.

Status: opt-in pilot, not yet the team's full calibration procedure. The ordinary
`shield_calib_setup_and_run.R` path is unchanged when `SHIELD_RECORDED_RUN` is
unset or `false`. A container canary and a server pilot (one realistic stage on
shield2) have passed. Multi-chain stages run in phases (below); the
current-runtime operator trial remains a separate follow-up.

The recorded path uses the same SHIELD specification, likelihoods, calibration
register, and MCMC call as the ordinary path. It changes startup and state
handling only: no Git pull or package installation at run time; explicit source
revisions, manager releases, input cache, and output root; and a non-destructive
choice between `fresh` and `resume`.

## Required inputs

Set `SHIELD_RECORDED_RUN=true` and provide:

An image declaring `SHIELD_CONTAINER_PROFILE=recorded` or
`SHIELD_REQUIRE_IMMUTABLE_INPUTS=true` refuses to use the ordinary launcher if
the opt-in flag is missing; it must never fall through to the path that clears
calibration cache.

| Variable | Meaning |
|---|---|
| `JHEEM_ANALYSES_PATH`, `JHEEM2_PATH` | Prepared source trees. The analyses directory must currently be named `jheem_analyses` because model files still use repo-root-relative paths. When the trees contain Git metadata, their clean HEADs must match the supplied revisions. |
| `JHEEM_ANALYSES_REF`, `JHEEM2_REF`, `LOCATIONS_REF`, `BAYESIAN_SIMULATIONS_REF`, `DISTRIBUTIONS_REF` | Full 40-character source commit SHAs. An image without Git metadata must be built from verified contexts and retain these identities in its labels. |
| `JHEEM_CACHE_DIR` | Existing input-cache tree, separate from the output tree. Dated manager assets and their verified `resolution.json` files must already be present. It may be mounted read-only. |
| `JHEEM_ROOT_DIR` | Existing, writable output tree. This path is not inferred from the host name or NAS mount. |
| `JHEEM_CENSUS_MANAGER_TAG`, `JHEEM_SYPHILIS_MANAGER_TAG` | Immutable dated release tags, for example `data-managers-v2026.08.26` and `syphilis-manager-v2026.07.27`. Mutable `latest` aliases are rejected. |
| `SHIELD_RANDOM_SEED` | Explicit nonnegative integer seed. |

`JHEEM2_MODE` defaults to `package` and may be `source`. The package mode uses
an already installed jheem2; source mode uses an already installed `pkgload` to
load the prepared checkout. `SHIELD_RUN_MODE` defaults to `resume`, and may be
`fresh`. Cache and log intervals can be adjusted with
`SHIELD_CACHE_FREQUENCY` and `SHIELD_UPDATE_FREQUENCY`.

For the container pilot, the image build must verify that the installed
jheem2, locations, bayesian.simulations, and distributions packages were built
from the recorded source contexts. A bare server's installed package version
alone does not prove its source commit; source mode or a separately attested
package build is needed there.

The command remains `Rscript <analyses-path>/applications/SHIELD/shield_calib_setup_and_run.R
<location> <calibration-code>`, with the variables above exported in its
environment. The recorded entrypoint changes into the analyses repository
before sourcing the model's existing relative paths. No new calibration API is
required.

Only this calibration entrypoint supports recorded mode. The modular
`setup`/`run`/`assemble` launcher explicitly refuses the recorded profile
because its ordinary setup still clears cache.

## Phases and multiple chains

By default the entrypoint runs a stage in one process: setup when `fresh`, then
chain 1, then assembly. That path takes single-chain calibrations only. A runner
can instead set `SHIELD_RECORDED_PHASE`, as the native stage-3 launcher does
with separate processes:

| Phase | Run mode | Does |
|---|---|---|
| `setup` | `fresh` | Checks state, writes the input receipt, sets up every chain, and writes the chain count to `chains.txt` beside the receipt |
| `run` (with `SHIELD_RECORDED_CHAIN=<k>`) | `resume` | Samples chain `k` from its own checkpoint; chains run in parallel |
| `assemble` | `resume` | Refuses unless every chain is complete, rebuilds the MCMC summary, assembles and saves the simulation set, and writes `outputs.json` |

Each chain process caches the summary when it finishes, and chains finishing
together can race to write it, so assembly rebuilds it once before recording it.
Running a chain again continues it from its last checkpoint; a finished chain
returns immediately. The SHIELD container runs every stage this way.

## State rules

The calibration directory comes from jheem2's `get.calibration.dir()`, so it
follows the layout of the jheem2 revision actually loaded (since `jheem2@ccb1f9b`,
`mcmc_runs/shield/<calibration-code>/<location>`). The state check therefore runs
after the specification has loaded jheem2, before any calibration setup.

`fresh` refuses an existing calibration directory and does not call
`clear.calibration.cache()`. It writes an input receipt under
`JHEEM_ROOT_DIR/run_records/shield/<location>/<calibration-code>/inputs.json`
before setup. `resume` requires a nonempty control file for its chain, readable
calibration progress, and the matching receipt. A missing or changed identity
stops before `run.calibration()`; it is not repaired or silently replaced.

The receipt records the five source revisions, seed, resolved release tags and
SHA-256 digests for the census and syphilis managers, and, for a stage with
preceding calibrations, the SHA-256 of each preceding stage's `outputs.json` in
the same output tree. A stage whose preceding stage has no recorded outputs does
not start. Because the preceding outputs are inputs, `resume` also refuses if
they changed.

Reusing a completed stage verifies both record identities, the actual output
files' sizes and SHA-256 digests, and the recorded preceding-stage lineage.
The container's completion command additionally checks that the completed
stage used the requested source revisions, manager assets, and seed. A leftover
`outputs.json` is not sufficient to skip a stage. Failed verification stops;
it never clears, repairs, or reruns the affected stage automatically.

If setup was interrupted after creating its input receipt but before a usable
checkpoint, neither fresh nor resume silently replaces it. Preserve that run
tree for diagnosis and use a separate output root for a deliberate new attempt.

After the simulation set is saved, the run writes `outputs.json` beside the
receipt: the same inputs, and the path (relative to `JHEEM_ROOT_DIR`), size, and
SHA-256 of the MCMC summary and the simulation set. Assembling a completed
calibration again rewrites it, so it describes the files on disk. Neither file
proves deterministic replay. A runner can add per-attempt records (image,
operator, host, times, exit status) under `attempts/` in the same directory; the
SHIELD container does.

## Container canary calibration

With `SHIELD_ENABLE_CONTAINER_SMOKE=true`, a recorded run also registers
`container.smoke.stage0` (`shield_calib_register_container_smoke.R`): the real
SHIELD model and stage-0 likelihood, two fixed-start transmission parameters,
one chain, and two iterations. Run it with `SHIELD_CACHE_FREQUENCY=1` so the
first iteration is a durable checkpoint that an interrupted run can resume
from. It validates execution and checkpoint continuation only; it is not for
scientific inference. `container.smoke.stage1` is the same size and starts from
`container.smoke.stage0`'s outputs, to validate stage chaining and its lineage
record. `container.smoke.stage3` runs four two-iteration chains seeded from
`container.smoke.pre3`, a 16-iteration single-chain stage whose summary holds
four samples, to validate phased multi-chain execution.

## Focused checks

From the repository root:

```sh
Rscript applications/SHIELD/tests/test-recorded-runtime.R
Rscript applications/SHIELD/tests/test-recorded-cache.R
Rscript applications/SHIELD/tests/test-output-checks.R
```

These test preflight rejection, receipt matching, and verified offline loading
without launching a calibration. Passing them does not substitute for an
interrupted/resumed MCMC canary using the current source and manager releases.

## Inspecting completed results

The output inspector checks the recorded files' digests and lineage before
loading the simset with its matching installed jheem2 package. Use it only on
trusted, completed outputs, not a live calibration's files:

```sh
Rscript applications/SHIELD/tests/inspect-recorded-outputs.R \
  /path/to/run-root C.12580 container.smoke.stage0 output-report.json
```

It checks the stage identity, simulation count, named finite parameters, and
finite population, incidence, total diagnoses, and primary/secondary diagnoses
at five-year intervals from 2010 through 2030. Total population must be positive.
Negative values in other outcomes are reported rather than assigned an arbitrary
scientific tolerance. Missing outcomes or years fail; missing/infinite values
are not dropped or replaced with zero by the getter.

The JSON report includes per-parameter and per-year minima, medians, maxima,
negative-value counts, and actual values from up to five simulations, together
with the input identities and simset digest. These are descriptive checks, not
posterior intervals or convergence evidence: the CI canary has only two
iterations. They supplement, rather than replace, the byte-integrity checks.
The report is written separately from console messages and refuses to replace
an existing file; choose a new filename for another inspection.

For a native/container comparison, first hold the scientific sources, manager
bytes, initial conditions, and a saved parameter vector constant. Compare the
resulting trajectories and individual likelihood contributions, reporting
absolute and relative differences with justified numerical tolerances. That
comparison is separate work; this inspector does not run the model or calculate
likelihoods. MCMC traces or serialized-file digests need not match across new
runs, and the current pilot does not promise deterministic sampler replay.
## Seed and checkpoint replay diagnostic

`container.smoke.replay` is loaded only when the existing
`SHIELD_ENABLE_CONTAINER_SMOKE=true` test switch is enabled. It uses the real
Baltimore stage-0 model and likelihood, samples the two transmission rates, and
runs eight iterations with no burn-in or thinning. The container test harness
sets a checkpoint every two iterations, creating four chunks.

The `jheem-containers` SHIELD workflow has an opt-in **Compare calibration
traces** input. It starts two separate fresh processes under the same seed,
then another run stopped after checkpoints one and two and resumed in new
processes. A fourth run changes the seed. Each run has a separate empty output
root and uses the same identified image and offline manager bytes.

`tests/inspect-calibration-trace.R` exports the saved original model parameters,
initial sampled parameters, checkpoint seeds, sampled values, likelihood and
prior traces, acceptance counts, and adaptive state at each checkpoint. Numeric
values use full-precision decimal strings. It reads completed trusted test
state in its matching package environment; it does not evaluate model functions.
`tests/check-calibration-checkpoint.R` verifies a paused test run's complete
checkpoint before interruption. The harness verifies that resuming preserves
the earlier chunks byte-for-byte.

The comparison reports exact agreement or the first differing checkpoint,
field, and coordinate. Runtime durations, timestamps, and serialized simulation
identifiers are excluded. The changed-seed control must change both stored
checkpoint seeds and sampled values or likelihoods. Eight iterations of one
chain characterize this setup; they do not establish convergence, long-run or
multi-chain replay, or the behavior of the sampler's explicit seed argument.

The [October 3 comparison](tests/REPLAY-COMPARISON.md) found exact agreement
between two fresh same-seed runs, but a different trajectory after resuming.
With the tested package revisions, enabling this optional comparison therefore
fails the workflow on the measured resume difference. Successful operational
continuation is not a promise of identical sampling across restarts.

## Actual stage-1 predecessor handoff diagnostic

With the same opt-in test switch, `container.actual.stage0` and
`container.actual.stage1` copy the current `calib.10.1` registrations. Their
likelihoods, sampled parameter sets, aliases, solver settings, manager, and
predecessor weighting remain unchanged. Only test codes, predecessor code,
iteration count (two), burn-in (zero), thinning (one), and descriptions change.
The fixtures reject an unexpected multi-chain or predecessor configuration.
They are not substitutes for a scientifically meaningful calibration.

The container workflow's optional **Check stage-1 handoff** input runs both in
an isolated output root. Use `september-2026` inputs. The completed-state
inspector verifies output digests and predecessor lineage, checks the saved
stage-1 likelihood includes the MSM diagnosis term, and verifies that its
initial model parameters exactly equal the predecessor summary's saved values.
Both stages' stored sample/likelihood/prior values must be finite. Numerical
simset inspection follows, then a second pipeline invocation must verify and
skip both completed stages. Reports/logs are retained as `shield-stage1-handoff`.

This closes a different gap from the older stage-chaining canary, which uses
stage-0 likelihoods in both stages, and the fixed-parameter compatibility check,
which does not reuse predecessor outputs or start MCMC. A passing short handoff
still does not establish convergence, realistic-stage performance, stage 2/3,
multi-chain execution, or identical trajectories across restarts.

The [October 4 hosted result](tests/STAGE1-HANDOFF.md) passed: 173 predecessor
parameters transferred exactly, the actual twelve-term stage-1 likelihood was
used, both stages produced two simulations, and a repeated pipeline verified
and skipped their outputs. The installed server image remains unchanged.
