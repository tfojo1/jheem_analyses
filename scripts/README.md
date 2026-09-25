# CI Pipeline — Quick Reference

Scripts and workflows for building, validating, and publishing data managers.

## How It Works

1. **Sections** are built (locally or via CI) and uploaded to a GitHub Release
2. The **merge workflow** downloads sections + dependencies, merges them, runs validation, and publishes a versioned release
3. After reviewing the quality report, a **promotion workflow** marks a build as `latest`

## Uploading Sections After a Local Rebuild

After rebuilding one or more sections locally and saving to the Q: drive:

```bash
# From the repo root. Point to wherever the NAS is mounted.
Rscript scripts/upload_sections.R Q:                       # Windows
Rscript scripts/upload_sections.R /mnt/jheem_nas_share     # Linux (shield1)
Rscript scripts/upload_sections.R /Volumes/jheem$          # macOS
```

This uploads all 4 section files from `{nas}/data_managers/data.manager.merge/`
to the `syphilis-sections-latest` release. The merge workflow will pick them up.

**Requires:** `gh` CLI installed and authenticated (`gh auth login`).

## Triggering a Merge Build

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

## Reading the Build Results

Each successful build creates a release like `syphilis-manager-v2026.03.11`.
The release notes contain:

- **Build Info** — which inputs were used (sections, raw data, deps, jheem2 version)
- **Spec Changes** (if spec was updated) — what structural changes occurred vs the previous spec
- **Validation Report** — structural check results + data quality summary

### Structural Validation (pass/fail)

Checks that the built manager matches the expected spec. Failures mean
something is missing or changed unexpectedly. Warnings are informational
(e.g., new years of data added).

### Data Quality Report (informational)

- **NA Analysis** — percentage of empty cells per stratification
- **Component Consistency** — totals >= sum of components
- **Marginal Consistency** — demographic breakdowns sum to aggregates

## Promoting a Build

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

   A successful build publishes a release like `hiv-surveillance-manager-v2026.09.25`.
   Its notes compare the new manager with the current `latest`: structural checks
   (a structural regression fails the build), which values changed, and whether
   common queries (Ryan White, EHE, the syphilis adult-population import) still work.
   Expected changes, such as a fix you just made, show up as changed values.

4. **Promote:** after reviewing the notes,

   ```bash
   gh workflow run "Promote HIV Surveillance Manager" \
     --repo tfojo1/jheem_analyses \
     -f release_tag=hiv-surveillance-manager-v2026.09.25
   ```

   This copies the build to `hiv-surveillance-manager-latest`. The next time anyone
   sources `source_code.R`, their `SURVEILLANCE.MANAGER` updates automatically,
   the same way the syphilis manager does.

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
| `upload_manager_deps.R` | surveillance + census managers + strat results | `data-managers-v{date}` (immutable) |

All scripts take the NAS root as their first argument and require the `gh` CLI.
