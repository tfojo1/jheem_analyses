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

## Hosted trial

`.github/workflows/trial-hiv-surveillance-manager.yml` is a manually dispatched
section-to-final trial. It pins the five-section snapshot, the published movement
archive and census manager by SHA-256, uses exact `jheem2` and `locations`
commits with R 4.4.2, and extracts only the county-to-county movement inputs.
The job builds in a temporary workspace, records Linux resource use and input
identities, and uploads its candidate and reports as a 30-day Actions artifact.
It does not write to shared storage or move a `latest` alias.

`validate_candidate.R` compares the result structurally with the published
August 26 surveillance-manager snapshot and reports data differences in shared
arrays. That baseline predates the active August 31 manager, and additions and
changed values require review. The value-delta report is diagnostic: it does
not alter the structural pass/fail result or judge scientific suitability. The
first completed hosted trial found two structural warnings for NSDUH MSA
additions; a separate comparison found changed adult-population values for
three Massachusetts substate regions that the structural check cannot see.
This trial does not establish active-model compatibility, raw-to-section
provenance, or a durable candidate release. Those checks and a reviewed baseline
must precede a production candidate/promotion workflow.
