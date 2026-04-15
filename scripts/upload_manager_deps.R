# Upload pre-built manager dependencies to a GitHub Release.
#
# These are managers and build artifacts that section builds or merge
# pipelines depend on, but that don't yet have their own CI pipelines.
# As managers get their own pipelines, they graduate out of this release.
#
# Current contents:
#   - surveillance.manager.rdata (used by sections 1, 4 and merge)
#   - census.manager.rdata (used by section 3)
#   - stratification_analysis_results_county_based.rdata (used by merge)
#
# Usage:
#   Rscript scripts/upload_manager_deps.R Q:                       # Windows
#   Rscript scripts/upload_manager_deps.R /mnt/jheem_nas_share     # Linux
#   Rscript scripts/upload_manager_deps.R /Volumes/jheem$          # macOS
#
# Optional second argument overrides the version tag:
#   Rscript scripts/upload_manager_deps.R Q: v2026.03.09
#
# Requires: gh CLI installed and authenticated

REPO <- "tfojo1/jheem_analyses"
TAG_PREFIX <- "data-managers"

# Files to include (paths relative to NAS root)
DEP_FILES <- c(
    "data_managers/surveillance.manager.rdata",
    "data_managers/census.manager.rdata",
    "data_managers/stratification_analysis_results_county_based.rdata"
)

# --- Parse arguments ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
    cat("Usage: Rscript scripts/upload_manager_deps.R <nas_root> [version_tag]\n\n")
    cat("  nas_root      Path to NAS root (e.g. Q:, /mnt/jheem_nas_share)\n")
    cat("  version_tag   Optional (default: today's date, e.g. v2026.03.09)\n")
    quit(status = 1)
}

nas_root <- args[1]
version_tag <- if (length(args) >= 2) args[2] else paste0("v", format(Sys.Date(), "%Y.%m.%d"))
release_tag <- paste0(TAG_PREFIX, "-", version_tag)

# --- Check gh CLI ---
if (system2("gh", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("gh CLI not found. Install from https://cli.github.com/")
}

# --- Check for existing release ---
if (system2("gh", c("release", "view", release_tag, "--repo", REPO),
            stdout = FALSE, stderr = FALSE) == 0) {
    stop("Release ", release_tag, " already exists.\n",
         "Use a different version tag, or delete the existing release.")
}

# --- Verify files exist ---
cat("=== Checking dependency files ===\n\n")
full_paths <- file.path(nas_root, DEP_FILES)
missing <- !file.exists(full_paths)

for (i in seq_along(DEP_FILES)) {
    size <- if (!missing[i]) {
        format(structure(file.info(full_paths[i])$size, class = "object_size"), units = "auto")
    } else "MISSING"
    status <- if (missing[i]) " *** NOT FOUND ***" else ""
    cat(sprintf("  %s (%s)%s\n", DEP_FILES[i], size, status))
}

if (any(missing)) {
    stop("Missing files:\n  ", paste(DEP_FILES[missing], collapse = "\n  "))
}

# --- Upload ---
cat("\nCreating release:", release_tag, "\n")
cat("Repository:", REPO, "\n\n")

notes <- paste0(
    "## Manager Dependencies\n\n",
    "Pre-built managers and build artifacts needed by section builds\n",
    "and merge pipelines.\n\n",
    "**Source:** `", nas_root, "`\n",
    "**Created:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n\n",
    "### Contents\n",
    paste(sprintf("- `%s`", basename(DEP_FILES)), collapse = "\n"), "\n"
)

notes_file <- file.path(tempdir(), "release_notes.md")
writeLines(notes, notes_file)

result <- system2("gh", c(
    "release", "create", release_tag,
    "--repo", REPO,
    "--title", shQuote(paste("Manager Dependencies", version_tag)),
    "--notes-file", notes_file,
    full_paths
))

if (result != 0) stop("Failed to create release. Check gh CLI authentication.")

cat("\nDone. Release:", release_tag, "\n")

# --- Update latest tag ---
LATEST_TAG <- paste0(TAG_PREFIX, "-latest")
cat("\nUpdating", LATEST_TAG, "to point to this release...\n")

system2("gh", c("release", "delete", LATEST_TAG, "--repo", REPO,
                "--cleanup-tag", "--yes"),
        stdout = FALSE, stderr = FALSE)  # OK if it doesn't exist yet

latest_notes <- paste0(notes, "\n**Points to:** `", release_tag, "`\n")
latest_notes_file <- file.path(tempdir(), "latest_release_notes.md")
writeLines(latest_notes, latest_notes_file)

result <- system2("gh", c(
    "release", "create", LATEST_TAG,
    "--repo", REPO,
    "--title", shQuote(paste("Manager Dependencies (Latest)")),
    "--notes-file", latest_notes_file,
    full_paths
))

if (result != 0) {
    warning("Failed to update ", LATEST_TAG, " — versioned release is still available at ", release_tag)
} else {
    cat("Done. Latest:", LATEST_TAG, "\n")
}
