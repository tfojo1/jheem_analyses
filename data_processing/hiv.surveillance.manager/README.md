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

## Candidate workflow

**Activation status, 2026-09-23:** the original hosted merge trial completed.
The active-baseline comparison and focused consumer checks below have passed
locally against its downloaded output. This workflow revision still needs its
fixed `hiv-surveillance-baseline-2026.08.31` input release published and a hosted
verification run. It is not yet the default distribution path.

`.github/workflows/trial-hiv-surveillance-manager.yml` runs manually. It takes a
dated five-section release tag and the SHA-256 of that release's
`SHA256SUMS.txt`. All five section digests are verified before use. Movement,
census, active-baseline, and syphilis-consumer snapshots are fixed by tag and
digest in the workflow. R is 4.4.2; `jheem2` and `locations` use exact commits.
Only the county-to-county movement inputs are extracted.

The job builds in an isolated workspace and uploads a candidate and evidence as
a **30-day Actions artifact**. It cannot publish releases, write to shared
storage, or change a `latest` alias. Durable publication is a separate step.

### Supplying changed sections

A processing-script edit upstream of the merge is **not tested by rebuilding
the same old sections**. Rebuild the affected sections through the existing
section workflow, then assemble a complete, deliberately selected set of
`surveillance.manager_section1.rdata` through `surveillance.manager_section5.rdata`.
Unchanged sections may be reused; record that reuse rather than implying that
all five were rebuilt together.

In a directory containing only that chosen section set, generate the manifest:

```sh
shasum -a 256 surveillance.manager_section{1,2,3,4,5}.rdata > SHA256SUMS.txt
shasum -a 256 SHA256SUMS.txt
```

Publish the five files and manifest together under a new dated
`hiv-sections-*` release. Record which processing revision, raw inputs, and
reused sections are known; mark unknown lineage explicitly. Do not replace
assets in an existing snapshot or use a mutable `latest` tag. Use the new tag
and manifest digest as the workflow's two inputs. The defaults reproduce the
September 23 snapshot.

### Reading the result

Download `hiv-surveillance-hosted-trial` from the completed run and begin with
`CANDIDATE.md` (also shown in the Actions summary). A failed run may still have
partial artifacts: their presence is not a successful build verdict.

- `active_baseline_report.json` compares structure, shared-array values, and
  stored metadata with the August 31 manager observed active on September 22. Metadata
  comparison excludes build timestamps and runtime methods. Structural
  regressions fail the job; value and metadata differences are reported for
  review, not silently accepted or used to update the baseline.
- `historical_baseline_report.json` preserves comparison with August 26. Its
  historical differences are not the active regression gate. The first trial
  reproduced the active manager while differing from August 26 in NSDUH MSA
  coverage and Massachusetts adult-population values. Reproduction does not
  certify the scientific correctness of either baseline.
- `consumer_report.json` checks eight Ryan White year-series queries (four
  outcomes in Texas and California), EHE's national stratified-prevalence pull,
  and the actual syphilis adult-population transfer into an empty manager with
  the released syphilis source registry. It uses the application's ontology
  mapping definitions. These checks do not run full calibrations, rebuild the
  entire syphilis manager, or establish compatibility with every application.
- `input_identity.txt`, `session_info.txt`, build/resource logs, and the output
  digest identify the build and its inputs.

The direct candidate/active comparison on September 23 found 603 structural
checks passing, no differences in 408 shared data arrays, equivalent compared
metadata, and all ten consumer checks passing. This local consumer run used
installed `jheem2` 1.12.3.9000, not a freshly verified package-source checkout;
the hosted workflow supplies an exact source pin.

### Retaining and selecting a candidate

Before the Actions artifact expires, a reviewed successful build can be retained
as a new GitHub **prerelease**, with `--latest=false`, under a distinct
`hiv-surveillance-candidate-*` tag. Preserve the final
`surveillance.manager.rdata`, `CANDIDATE.md`, reports, logs, input identities,
package/session information, and SHA-256 manifest together. Target the recorded
analyses build commit, not whatever happens to be current at publication time.
Publishing this review candidate is not promotion to the active manager.

For isolated inspection, download that specific release to a new directory,
verify its published digest, and load the exact file:

```r
candidate <- jheem2::load.data.manager(
  file = "path/to/selected-release/surveillance.manager.rdata"
)
```

Selecting a previously retained file gives an inspection rollback without
overwriting `cached/surveillance.manager.rdata` or shared storage. The existing
[`release.tag` cache-loader option](../../commoncode/data-manager-releases.md)
currently applies to configured GitHub-backed managers (syphilis), **not** to
the HIV surveillance manager's default OneDrive route. Wiring an HIV release
selector into application loading and switching its default distribution are
separate integration steps; this workflow does neither.

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
