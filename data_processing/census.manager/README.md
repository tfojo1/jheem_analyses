# Census manager build

The census manager is built from three sections, each saved by its processing script
in `census.processing.for.merging/`:

| Section | Script | Contents |
|---|---|---|
| `census.manager_section1.rdata` | `section.1.processing.R` | Population, 1970–2020, including single-year ages (Census, CDC Wonder) |
| `census.manager_section2.rdata` | `section.2.processing.R` | County population (total, stratified, single-year ages) and postcensal deaths, with national aggregates |
| `census.manager_section3.rdata` | `section.3.processing.R` | Births, deaths, fertility, metro and state deaths |

`census.processing.for.merging/census.manager.merge.R` merges them. Operator steps
(upload sections, build, promote) are in [`scripts/README.md`](../../scripts/README.md#census-manager).

## The merge

Its default paths keep the interactive workflow: load the sections from
`Q:/data_managers/data.manager.merge/`, then write `../../cached/census.manager.rdata`,
the shared `Q:/data_managers/census.manager.rdata`, and a dated copy in
`Q:/data_managers/Archive/`.

A CI build sets `WRITE_SHARED_OUTPUT=false` with explicit paths, so it reads the given
sections and writes only the candidate:

| Variable | Purpose |
|---|---|
| `SECTION_DIR` | Directory containing `census.manager_section1.rdata` through `_section3.rdata` (required). |
| `CACHED_DIR` | Output directory for the merged `census.manager.rdata` (required). |
| `Q_ROOT`, `SHARED_MANAGER_DIR`, `ARCHIVE_DIR` | Shared-drive locations for interactive runs. |

`Rscript data_processing/census.manager/test_merge_paths.R` checks the path and
shared-write contract without loading real manager files.

## What a build checks

- **Structure and values against the promoted census** (`validate_candidate.R` and the
  value-delta and reproduction tools in `../hiv.surveillance.manager/`, shared with the
  HIV build): removed or added outcomes, sources, ontologies, and dimension values, and
  which arrays' values changed.
- **SHIELD starts** (`check_consumers.R`): the SHIELD model is started in recorded mode,
  offline, with the candidate census and the syphilis release in
  `applications/SHIELD/data_managers.csv`; its specification is compiled and one
  location's engine (Baltimore) is built, which builds its kernel from the census data. The same check on the promoted census is a
  control: if that fails, the check itself is broken and the build stops, rather than
  blaming the candidate.
- **Data quality** (`quality_report.R`, informational): missing values and dimension
  sanity, with accepted issues listed in `known_issues.json`.

`summarize_candidate.R` turns these into the release notes and `build_status.json`. A
build **needs review** when it removes structure the promoted census has or SHIELD doesn't
start with it; it is then published as a pre-release, and promotion requires confirming
the changes are intended.

## Scope

The build tests section-to-final assembly, not rebuilding sections from raw files; an
edit to a section script is tested once that section is rebuilt and uploaded. The SHIELD
check shows that the model starts, not that results are scientifically right, and other
applications that use census are not checked here.

The first sections (`census-sections-v2026.10.23`) were assembled from the files the
census scripts had saved before the October 21 rename, plus the committed national-deaths
step; merged, they reproduce the census published on October 21 exactly.
