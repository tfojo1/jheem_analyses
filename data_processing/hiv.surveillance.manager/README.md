# HIV surveillance-manager merge

The section-to-final merge is
`hiv.surveillance.processing.for.merging/hiv.surveillance.manager.merge.R`.
Its default paths preserve the existing interactive workflow: load five sections
and the census manager from the usual `Q:`/`cached/` locations, then write the
local cached result, shared current manager, and dated shared archive.

An isolated candidate build sets `WRITE_SHARED_OUTPUT=false` and all five
explicit paths below before sourcing the merge. The candidate mode refuses to
run if any path is missing and skips the legacy shared current and archive
writes. The caller must keep `CACHED_DIR` in an isolated workspace.

| Variable | Purpose |
|---|---|
| `SECTION_DIR` | Directory containing `surveillance.manager_section1.rdata` through `surveillance.manager_section5.rdata`. |
| `CACHED_DIR` | Candidate output directory for the pre-outlier and final manager files. |
| `CENSUS_MANAGER_CACHE_FILE` | Census manager used by the first aggregation step. |
| `CENSUS_MANAGER_SHARED_FILE` | Census manager used by the later death aggregation step. |
| `COUNTY_TO_COUNTY_DIR` | Directory of county-to-county migration workbooks used for Oakland. |

The merge also needs the compatible installed R packages and a sibling `jheem2`
source checkout. `locations` must support the `include.partial` argument used by
current aggregation code. `Rscript data_processing/hiv.surveillance.manager/test_merge_paths.R`
checks the path and shared-write contract without loading real manager files or
running the scientific transformations.

## CI build, promotion, and loading

Operator steps (upload sections, build, promote, refresh the shared copy) are in
[`scripts/README.md`](../../scripts/README.md#hiv-surveillance-manager). They
match the syphilis manager's steps. This section describes what the build checks.

The **Build HIV Surveillance Manager** workflow
(`.github/workflows/build-hiv-surveillance-manager.yml`) runs manually. By default
it reads `hiv-sections-latest`, resolves the dated `hiv-sections-v*` snapshot it
points to, and requires both to carry the same `SHA256SUMS.txt`. All five section
digests are verified before use. An optional input can require a specific manifest
digest. Movement, census, historical, and syphilis-consumer inputs are fixed by
tag and digest in the workflow. R is 4.4.2; `jheem2` and `locations` use exact
commits. Other R dependencies are installed without a lockfile; their observed
versions are retained in `session_info.txt`.

The build runs in an isolated workspace and never writes to shared storage. When
every check passes, it publishes an `hiv-surveillance-manager-v*` release with
the manager, `CANDIDATE.md`, `input_identity.txt`, a validation-evidence archive,
and `SHA256SUMS.txt`. Publishing a build doesn't change what anyone loads; only
the **Promote HIV Surveillance Manager** workflow moves
`hiv-surveillance-manager-latest`.

### What the checks mean

- **Regression baseline:** the currently promoted `hiv-surveillance-manager-latest`
  (before the first promotion, the fixed August 31 snapshot). Structural
  regressions fail the build. Value and metadata differences are reported in
  `active_baseline_report.json` and `CANDIDATE.md` for review; they don't fail the
  build, and the baseline is never updated automatically.
- **Historical comparison:** `historical_baseline_report.json` preserves the
  comparison with the August 26 snapshot as evidence, not as a gate.
- **Consumers:** `consumer_report.json` checks eight Ryan White year-series
  queries (four outcomes in Texas and California), EHE's national stratified
  prevalence pull, and the actual syphilis adult-population transfer into an
  empty manager with the released syphilis source registry. These checks don't
  run full calibrations or establish compatibility with every application.
- **Scope:** this is section-to-final assembly. An upstream processing edit is
  tested only after its sections are rebuilt and uploaded. Record which sections
  were reused rather than implying all five were rebuilt together. Reproducing a
  baseline doesn't certify scientific correctness.

The first CI trial (run 35952637832) reproduced the August 31 manager exactly:
603 structural checks, no differences in 408 shared data arrays, and all ten
consumer checks. The merge takes about 20 minutes and 7.8 GiB peak memory.

### Loading and rollback

After a promotion, `source_code.R` loads the promoted manager through
`load.data.manager.from.cache()`, the same way as the syphilis manager. To pin an
earlier build, pass its tag as `release.tag`. To inspect a build without loading
it by default, download it and verify its checksums:

```sh
gh release download hiv-surveillance-manager-v2026.09.25 \
  --repo tfojo1/jheem_analyses --dir candidate-review
(cd candidate-review && shasum -a 256 -c SHA256SUMS.txt)
```

## Local checks

Run from the repository root:

```sh
Rscript data_processing/hiv.surveillance.manager/test_merge_paths.R
Rscript data_processing/hiv.surveillance.manager/test_manager_value_delta.R
Rscript data_processing/hiv.surveillance.manager/test_manager_reproduction.R
Rscript data_processing/hiv.surveillance.manager/validate_candidate.R baseline.rdata candidate.rdata report.json
Rscript data_processing/hiv.surveillance.manager/check_consumers.R candidate.rdata active-baseline.rdata syphilis.rdata consumers.json
```

The first three use synthetic inputs; the last two require real artifacts.
Neither changes the input files or scientific transformation code.
