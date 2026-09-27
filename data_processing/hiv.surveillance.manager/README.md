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
(`.github/workflows/build-hiv-surveillance-manager.yml`) runs manually. Its inputs
are the sections release (default `hiv-sections-latest`), the raw-data release
(`data-raw-latest`; only the county-to-county movement workbooks are used), the
dependencies release (`data-managers-latest`; the census manager), and the `jheem2`
ref (`dev`). `locations` comes from its default branch.

Inputs can move; builds are traceable anyway. A `*-latest` sections release must
name a dated snapshot with an identical `SHA256SUMS.txt`, and all five section
digests are verified. Every other download is checked against the SHA-256 digest
GitHub publishes for the asset. `input_identity.txt` records each release, its
digests, and the exact `jheem2`, `locations`, and analyses commits; the release
notes summarize them. R is 4.4.2. Other R dependencies are installed without a
lockfile; their observed versions are retained in `session_info.txt`.

The build runs in an isolated workspace and never writes to shared storage. On
`master`, a successful build publishes an `hiv-surveillance-manager-v*` release
with the manager, `CANDIDATE.md`, `input_identity.txt`, `build_status.json`, a
validation-evidence archive, and `SHA256SUMS.txt`. Runs from other branches only
upload a 30-day Actions artifact. Publishing a build doesn't change what anyone
loads; only the **Promote HIV Surveillance Manager** workflow moves
`hiv-surveillance-manager-latest`.

### What the checks mean

- **Regression baseline:** the currently promoted `hiv-surveillance-manager-latest`.
  The baseline only moves when someone promotes; building never changes it.
- **Structural removals** (an outcome, source, ontology, or stratification in the
  promoted manager but not the candidate) don't fail the build. They mark it
  **Needs review**: the release is a pre-release, its notes list the removals, and
  the promote workflow refuses it unless the review box is ticked.
- **Additions and value changes** are reported in `active_baseline_report.json` and
  `CANDIDATE.md`. They're expected whenever data or processing changes.
- **Consumers:** `consumer_report.json` checks eight Ryan White year-series
  queries (four outcomes in Texas and California), EHE's national stratified
  prevalence pull, and the actual syphilis adult-population transfer into an
  empty manager with the promoted syphilis manager's source registry. A failure
  here fails the build. These checks don't run full calibrations or establish
  compatibility with every application.
- **Scope:** this is section-to-final assembly. An upstream processing edit is
  tested only after its sections are rebuilt and uploaded. Reproducing a
  baseline doesn't certify scientific correctness.

On 2026-09-26 the build reproduced the promoted September 24 manager's data
exactly from the September 23 sections (0 of 408 arrays differing). The merge
takes about 20 minutes and 7.8 GiB peak memory.

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
