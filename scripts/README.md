# CI Pipeline — Quick Reference

Scripts and workflows for building, validating, and publishing data managers.

## How It Works

1. **Sections** are built (locally or via CI) and uploaded to a GitHub Release
2. The **merge workflow** downloads sections + dependencies, merges them, runs validation, and publishes a versioned release
3. After reviewing the quality report, a **promotion workflow** marks a build as `latest`

Each manager has its own upload script and workflows. Both upload scripts read the
same NAS folder (`data_managers/data.manager.merge/`) but take only their own files:

| | Syphilis manager | HIV surveillance manager | Census manager |
|---|---|---|---|
| Section files | `syphilis.manager_section1–4.rdata` | `surveillance.manager_section1–5.rdata` | `census.manager_section1–3.rdata` |
| Upload | `scripts/upload_sections.R` | `scripts/upload_hiv_sections.R` | `scripts/upload_census_sections.R` |
| Build workflow | Build Syphilis Manager | Build HIV Surveillance Manager | Build Census Manager |
| Promote workflow | Promote Syphilis Manager | Promote HIV Surveillance Manager | Promote Census Manager |
| Steps | [Syphilis manager](#syphilis-manager) | [HIV surveillance manager](#hiv-surveillance-manager) | [Census manager](#census-manager) |

## Syphilis Manager

### Uploading Syphilis Sections After a Local Rebuild

After rebuilding one or more sections locally and saving to the Q: drive:

```bash
# From the repo root. Point to wherever the NAS is mounted.
Rscript scripts/upload_sections.R Q:                       # Windows
Rscript scripts/upload_sections.R /mnt/jheem_nas_share     # Linux (shield1)
Rscript scripts/upload_sections.R /Volumes/jheem$          # macOS
```

This uploads the 4 syphilis section files from `{nas}/data_managers/data.manager.merge/`
to the `syphilis-sections-latest` release. The merge workflow will pick them up.

**Requires:** `gh` CLI installed and authenticated (`gh auth login`).

### Triggering a Syphilis Build

Go to **Actions > Build Syphilis Manager > Run workflow** on GitHub, or:

```bash
gh workflow run "Build Syphilis Manager" \
  --repo tfojo1/jheem_analyses \
  -f sections_release_tag=syphilis-sections-latest \
  -f raw_data_release_tag=data-raw-v2026.03.10 \
  -f deps_release_tag=data-managers-v2026.03.10
```

If the structure has changed intentionally (new outcomes, removed stratifications, etc.),
set `update_spec=true` to regenerate the spec. The release notes will include a
summary of what changed.

### Reading the Build Results

Each successful build creates a release like `syphilis-manager-v2026.03.11`.
The release notes contain:

- **Build Info** — which inputs were used (sections, raw data, deps, jheem2 version)
- **Spec Changes** (if spec was updated) — what structural changes occurred vs the previous spec
- **Validation Report** — structural check results + data quality summary

#### Structural Validation (pass/fail)

Checks that the built manager matches the expected spec. Failures mean
something is missing or changed unexpectedly. Warnings are informational
(e.g., new years of data added).

#### Data Quality Report (informational)

- **NA Analysis** — percentage of empty cells per stratification
- **Component Consistency** — totals >= sum of components
- **Marginal Consistency** — demographic breakdowns sum to aggregates

### Promoting a Syphilis Build

After reviewing the quality report:

```bash
gh workflow run "Promote Syphilis Manager" \
  --repo tfojo1/jheem_analyses \
  -f release_tag=syphilis-manager-v2026.03.11
```

This copies the manager to `syphilis-manager-latest`, which provides a
stable download URL for the team.

## HIV Surveillance Manager

The HIV surveillance manager uses the same steps, with its own script and
workflows. The merge step (aggregation, outliers, Oakland additions) runs in CI,
so there's no need to run `hiv.surveillance.manager.merge.R` locally.

1. **Rebuild sections locally** as usual (they save to `Q:/data_managers/data.manager.merge/`).
2. **Upload them:**

   ```bash
   Rscript scripts/upload_hiv_sections.R Q:                       # Windows
   Rscript scripts/upload_hiv_sections.R /mnt/jheem_nas_share     # Linux (shield1)
   Rscript scripts/upload_hiv_sections.R /Volumes/jheem$          # macOS
   ```

   This uploads all 5 section files as a dated release (e.g. `hiv-sections-v2026.09.25`)
   and points `hiv-sections-latest` at it. Add `--dry-run` to check the files first.

3. **Build:** go to **Actions > Build HIV Surveillance Manager > Run workflow**, or:

   ```bash
   gh workflow run "Build HIV Surveillance Manager" --repo tfojo1/jheem_analyses
   ```

   The inputs work like the syphilis build: sections (default `hiv-sections-latest`),
   raw data (`data-raw-latest`), dependencies (`data-managers-latest`, for the census
   manager), and the jheem2 ref (`dev`). Change any of them to build from a specific
   release or branch. Whatever you choose, the release notes record the exact
   version and SHA-256 of every input.

   A successful build publishes a release like `hiv-surveillance-manager-v2026.09.25`.
   Its notes compare the new manager with the current `latest`:

   - **Structural changes**: every removed and added outcome, source, ontology,
     stratification, or dimension value, listed by path.
   - **Value changes**: a table of each array whose values changed, with the number
     of changed cells and which locations, years, and strata changed.
   - **Data quality report**: the same missing-value and dimension checks as the
     syphilis build. Accepted issues can be listed in
     `data_processing/hiv.surveillance.manager/known_issues.json` to hide them.
   - Whether common queries (Ryan White, EHE, the syphilis adult-population import)
     still work.

   Expected changes, such as a fix you just made, show up in these lists. If the build *removes* something the current
   manager has (an outcome, source, or stratification), the release is marked
   **Needs review** and listed as a pre-release. The build fails only if the merge
   itself errors or a common query stops working. Runs from branches other than
   `master` check everything but don't publish.

4. **Promote:** after reviewing the notes,

   ```bash
   gh workflow run "Promote HIV Surveillance Manager" \
     --repo tfojo1/jheem_analyses \
     -f release_tag=hiv-surveillance-manager-v2026.09.25
   ```

   This copies the build to `hiv-surveillance-manager-latest`. The next time anyone
   sources `source_code.R`, their `SURVEILLANCE.MANAGER` updates automatically,
   the same way the syphilis manager does. For a **Needs review** build, tick the
   review box (or add `-f reviewed_removals=true`) once you've confirmed the
   removals are intended.

5. **Refresh the shared copy.** Syphilis section builds and the syphilis merge read
   the HIV manager from `Q:/data_managers/`, and CI doesn't write there:

   ```bash
   gh release download hiv-surveillance-manager-latest --repo tfojo1/jheem_analyses \
     --pattern surveillance.manager.rdata --dir Q:/data_managers --clobber
   ```

   If syphilis CI builds should use this version too, re-run `upload_manager_deps.R`.
   `data_processing/QA/sync cached data manager.R` is no longer needed for HIV.

To load a specific earlier version (for example, to roll back one analysis):

```r
SURVEILLANCE.MANAGER <- load.data.manager.from.cache(
  "surveillance.manager.rdata", set.as.default = TRUE,
  release.tag = "hiv-surveillance-manager-v2026.09.25")
```

More detail on the HIV checks: `data_processing/hiv.surveillance.manager/README.md`.

## Census Manager

The census manager uses the same steps, with its own script and workflows. The
merge runs in CI, so there's no need to run `census.manager.merge.R` locally.

1. **Rebuild sections locally** with
   `data_processing/census.manager/census.processing.for.merging/section.1.processing.R`
   through `section.3.processing.R`. They save to
   `Q:/data_managers/data.manager.merge/census.manager_section1.rdata` through `_section3.rdata`.
2. **Upload them:**

   ```bash
   Rscript scripts/upload_census_sections.R Q:                       # Windows
   Rscript scripts/upload_census_sections.R /mnt/jheem_nas_share     # Linux (shield1)
   Rscript scripts/upload_census_sections.R /Volumes/jheem$          # macOS
   ```

   This uploads the three section files as a dated release (e.g. `census-sections-v2026.10.23`)
   and points `census-sections-latest` at it. Add `--dry-run` to check the files first.

3. **Build:** go to **Actions > Build Census Manager > Run workflow**, or:

   ```bash
   gh workflow run "Build Census Manager" --repo tfojo1/jheem_analyses
   ```

   The inputs are the sections (default `census-sections-latest`), the census to compare
   against (default `census-manager-latest`, the promoted one), and the jheem2 ref (`dev`).
   A successful build publishes a release like `census-manager-v2026.10.23`. Its notes show:

   - **Structural changes** compared with the promoted census: every removed or added
     outcome, source, ontology, stratification, or dimension value.
   - **Value changes**: each array whose values changed, with the number of changed cells.
   - **SHIELD check**: whether the SHIELD model still starts with this census (its
     specification compiles and a Baltimore engine builds), using the syphilis release in
     `applications/SHIELD/data_managers.csv`. The same check runs on the promoted census
     as a control.
   - **Data quality report**: the same missing-value and dimension checks as the other
     builds. Accepted issues can be listed in `data_processing/census.manager/known_issues.json`.

   If the build removes something the promoted census has, or SHIELD doesn't start with it,
   the release is marked **Needs review** and listed as a pre-release. Runs from branches
   other than `master` check everything but don't publish.

4. **Promote:** after reviewing the notes,

   ```bash
   gh workflow run "Promote Census Manager" \
     --repo tfojo1/jheem_analyses \
     -f release_tag=census-manager-v2026.10.23
   ```

   This copies the build to `census-manager-latest`. For a **Needs review** build, tick the
   review box (or add `-f reviewed_changes=true`) once you've confirmed the changes are intended.

**What this changes for people using census, for now.** Nothing loads `census-manager-latest`
automatically yet. SHIELD loads the dated census named in `applications/SHIELD/data_managers.csv`;
other applications load census through the cached-metadata (OneDrive) path; and the syphilis and
HIV builds take census from the `data-managers-*` dependency bundle. So after promoting, update
those as before (the cached-metadata file, and `upload_manager_deps.R` if the builds should use it).
To use a new census in SHIELD, change its line in `data_managers.csv` to the new dated release.

More detail on the census build: `data_processing/census.manager/README.md`.

## Copies of OneDrive Managers

Managers published through the OneDrive cache (census, Ryan White, Medicaid) are replaced
in place by each update. The **Snapshot OneDrive Managers** workflow keeps a copy of each
version: whenever `commoncode/data_manager_cache_metadata.Rdata` changes on master, it
downloads each manager the metadata names, checks it is the version the metadata describes,
and publishes it as an immutable release named after the file and its last-modified date,
e.g. `census-manager-onedrive-v2026.10.26`. Versions already kept are skipped. Nothing
changes in how managers are published or loaded.

To load a kept census version:

```r
CENSUS.MANAGER <- load.data.manager.from.cache(
  "census.manager.rdata", release.tag = "census-manager-onedrive-v2026.10.26")
```

The workflow can also be run by hand (**Actions > Snapshot OneDrive Managers**), and the
script directly: `Rscript scripts/snapshot_onedrive_managers.R [--dry-run] [file ...]`.

## Uploading New Raw Data

When new data files are added to the NAS:

```bash
# Best run from shield1 for fast upload speeds
Rscript scripts/upload_raw_data.R /mnt/jheem_nas_share
```

Creates one archive per `data_raw/` subdirectory, uploaded as a new
immutable release (e.g., `data-raw-v2026.03.11`). Update the merge
workflow's default `raw_data_release_tag` to use the new version.

## Uploading Manager Dependencies

When surveillance.manager or census.manager are rebuilt:

```bash
Rscript scripts/upload_manager_deps.R /mnt/jheem_nas_share
```

## Upload Scripts Reference

| Script | What it uploads | Release tag |
|--------|----------------|-------------|
| `upload_sections.R` | 4 section .rdata files | `syphilis-sections-latest` (mutable) |
| `upload_raw_data.R` | All of `data_raw/` (per-subdir archives) | `data-raw-v{date}` (immutable) |
| `upload_hiv_sections.R` | 5 HIV section .rdata files + `SHA256SUMS.txt` | `hiv-sections-v{date}` (immutable) and `hiv-sections-latest` (mutable) |
| `upload_census_sections.R` | 3 census section .rdata files + `SHA256SUMS.txt` | `census-sections-v{date}` (immutable) and `census-sections-latest` (mutable) |
| `upload_manager_deps.R` | surveillance + census managers + strat results | `data-managers-v{date}` (immutable) |
| `snapshot_onedrive_managers.R` | each OneDrive manager version (run by its workflow) | `{file}-onedrive-v{date}` (immutable) |

The upload scripts take the NAS root as their first argument. All of these scripts require the `gh` CLI.
