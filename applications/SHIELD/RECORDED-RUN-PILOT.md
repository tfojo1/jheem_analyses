# Recorded SHIELD calibration path (pilot)

Status (2026-09-23): opt-in implementation under validation. The ordinary
`shield_calib_setup_and_run.R` path is unchanged when `SHIELD_RECORDED_RUN` is
unset or `false`. This path is not yet a production calibration procedure; it
still needs a current-source container canary and a retained server pilot.

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
| `JHEEM_ANALYSES_REF`, `JHEEM2_REF`, `LOCATIONS_REF` | Full 40-character source commit SHAs. An image without Git metadata must be built from verified contexts and retain these identities in its labels. |
| `JHEEM_CACHE_DIR` | Existing input-cache tree, separate from the output tree. Dated manager assets and their verified `resolution.json` files must already be present. It may be mounted read-only. |
| `JHEEM_ROOT_DIR` | Existing, writable output tree. This path is not inferred from the host name or NAS mount. |
| `JHEEM_CENSUS_MANAGER_TAG`, `JHEEM_SYPHILIS_MANAGER_TAG` | Immutable dated release tags, for example `data-managers-v2026.08.26` and `syphilis-manager-v2026.07.27`. Mutable `latest` aliases are rejected. |
| `SHIELD_RANDOM_SEED` | Explicit nonnegative integer seed. |

`JHEEM2_MODE` defaults to `package` and may be `source`. The package mode uses
an already installed jheem2; source mode uses an already installed `pkgload` to
load the prepared checkout. `SHIELD_RUN_MODE` defaults to `resume`, and may be
`fresh`. Cache and log intervals can be adjusted with
`SHIELD_CACHE_FREQUENCY` and `SHIELD_UPDATE_FREQUENCY`.

For the container pilot, the image build must verify that the installed jheem2
and locations packages were built from the recorded source contexts. A bare
server's installed package version alone does not prove its source commit;
source mode or a separately attested package build is needed there.

The command remains `Rscript <analyses-path>/applications/SHIELD/shield_calib_setup_and_run.R
<location> <calibration-code>`, with the variables above exported in its
environment. The recorded entrypoint changes into the analyses repository
before sourcing the model's existing relative paths. No new calibration API is
required.

Only this monolithic calibration entrypoint supports recorded mode in the
current slice. The modular `setup`/`run`/`assemble` launcher explicitly refuses
the recorded profile because its ordinary setup still clears cache. The
existing container's `calibration-stage` canary must be reconciled with that
launcher before it can validate this source revision.

## State rules

`fresh` refuses an existing calibration directory and does not call
`clear.calibration.cache()`. It writes an input receipt under
`JHEEM_ROOT_DIR/run_records/shield/<location>/<calibration-code>/inputs.json`
before setup. `resume` requires a nonempty chain-1 control file, readable
calibration progress, and the matching receipt. A missing or changed identity
stops before `run.calibration()`; it is not repaired or silently replaced.

The receipt records the three source revisions, seed, and resolved release tags
and SHA-256 digests for the census and syphilis managers. It does not by itself
prove deterministic replay or capture the final simset. Retain the container
image digest, complete log, checkpoint tree, and any output artifact alongside
the receipt during a server pilot.

## Focused checks

From the repository root:

```sh
Rscript applications/SHIELD/tests/test-recorded-runtime.R
Rscript applications/SHIELD/tests/test-recorded-cache.R
```

These test preflight rejection, receipt matching, and verified offline loading
without launching a calibration. Passing them does not substitute for an
interrupted/resumed MCMC canary using the current source and manager releases.
