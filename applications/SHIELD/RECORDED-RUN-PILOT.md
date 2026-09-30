# Recorded SHIELD calibration path (pilot)

Status (2026-09-30): opt-in implementation under validation. The ordinary
`shield_calib_setup_and_run.R` path is unchanged when `SHIELD_RECORDED_RUN` is
unset or `false`. A container canary and a server pilot (one realistic stage on
shield2) have passed; this path is not yet the team's calibration procedure.

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

Only this monolithic calibration entrypoint supports recorded mode in the
current slice, and it runs chain 1 only, so recorded mode refuses calibrations
registered with more than one chain (stage 3). The modular
`setup`/`run`/`assemble` launcher explicitly refuses the recorded profile
because its ordinary setup still clears cache.

## State rules

The calibration directory comes from jheem2's `get.calibration.dir()`, so it
follows the layout of the jheem2 revision actually loaded (since `jheem2@ccb1f9b`,
`mcmc_runs/shield/<calibration-code>/<location>`). The state check therefore runs
after the specification has loaded jheem2, before any calibration setup.

`fresh` refuses an existing calibration directory and does not call
`clear.calibration.cache()`. It writes an input receipt under
`JHEEM_ROOT_DIR/run_records/shield/<location>/<calibration-code>/inputs.json`
before setup. `resume` requires a nonempty chain-1 control file, readable
calibration progress, and the matching receipt. A missing or changed identity
stops before `run.calibration()`; it is not repaired or silently replaced.

The receipt records the five source revisions, seed, resolved release tags and
SHA-256 digests for the census and syphilis managers, and, for a stage with
preceding calibrations, the SHA-256 of each preceding stage's `outputs.json` in
the same output tree. A stage whose preceding stage has no recorded outputs does
not start. Because the preceding outputs are inputs, `resume` also refuses if
they changed.

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
record.

## Focused checks

From the repository root:

```sh
Rscript applications/SHIELD/tests/test-recorded-runtime.R
Rscript applications/SHIELD/tests/test-recorded-cache.R
```

These test preflight rejection, receipt matching, and verified offline loading
without launching a calibration. Passing them does not substitute for an
interrupted/resumed MCMC canary using the current source and manager releases.
