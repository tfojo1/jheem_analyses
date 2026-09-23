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
checks the path
and shared-write contract without loading real manager files or running the
scientific transformations.

This is path isolation, not yet a hosted CI pipeline. A CI build must identify
the five section files and auxiliary inputs by immutable artifact and digest,
pin its source/runtime dependencies, validate a candidate against a reviewed
baseline and active consumers, and publish a versioned candidate separately
from promotion. It should not describe a section-to-final build as raw-to-final
provenance.
