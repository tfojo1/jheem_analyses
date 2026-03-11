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
| `upload_manager_deps.R` | surveillance + census managers + strat results | `data-managers-v{date}` (immutable) |

All scripts take the NAS root as their first argument and require the `gh` CLI.
