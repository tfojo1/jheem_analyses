# Upload section .rdata files to the syphilis-sections-latest release.
#
# This updates the "latest" sections that the merge workflow consumes.
# Can be run after building sections locally (Zoe's workflow) or used
# by CI after automated section builds.
#
# Usage:
#   Rscript scripts/upload_sections.R Q:                       # Windows
#   Rscript scripts/upload_sections.R /mnt/jheem_nas_share     # Linux
#   Rscript scripts/upload_sections.R /Volumes/jheem$          # macOS
#
# By default, reads from data_managers/data.manager.merge/ under the
# NAS root. Override with a second argument:
#   Rscript scripts/upload_sections.R /mnt/jheem_nas_share /path/to/sections
#
# Requires: gh CLI installed and authenticated

REPO <- "tfojo1/jheem_analyses"
RELEASE_TAG <- "syphilis-sections-latest"
SECTION_FILES <- paste0("syphilis.manager_section", 1:4, ".rdata")

# --- Parse arguments ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
    cat("Usage: Rscript scripts/upload_sections.R <nas_root> [section_dir]\n\n")
    cat("  nas_root      Path to NAS root (e.g. Q:, /mnt/jheem_nas_share)\n")
    cat("  section_dir   Optional path to directory containing section files\n")
    cat("                (default: <nas_root>/data_managers/data.manager.merge)\n")
    quit(status = 1)
}

nas_root <- args[1]
section_dir <- if (length(args) >= 2) {
    args[2]
} else {
    file.path(nas_root, "data_managers", "data.manager.merge")
}

# --- Check gh CLI ---
if (system2("gh", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("gh CLI not found. Install from https://cli.github.com/")
}

# --- Verify section files exist ---
cat("=== Checking section files in", section_dir, "===\n\n")
full_paths <- file.path(section_dir, SECTION_FILES)
missing <- !file.exists(full_paths)

for (i in seq_along(SECTION_FILES)) {
    if (!missing[i]) {
        info <- file.info(full_paths[i])
        size <- format(structure(info$size, class = "object_size"), units = "auto")
        modified <- format(info$mtime, "%Y-%m-%d %H:%M")
        cat(sprintf("  %s (%s, modified %s)\n", SECTION_FILES[i], size, modified))
    } else {
        cat(sprintf("  %s *** NOT FOUND ***\n", SECTION_FILES[i]))
    }
}

if (any(missing)) {
    stop("Missing section files:\n  ", paste(SECTION_FILES[missing], collapse = "\n  "))
}

# --- Delete existing release and recreate ---
# Unlike raw data releases (which are immutable/versioned), the sections
# "latest" release is mutable — it always reflects the current sections.
cat("\nUpdating release:", RELEASE_TAG, "\n")
cat("Repository:", REPO, "\n\n")

system2("gh", c("release", "delete", RELEASE_TAG, "--repo", REPO,
                "--cleanup-tag", "--yes"),
        stdout = FALSE, stderr = FALSE)  # OK if it doesn't exist

notes <- paste0(
    "## Syphilis Manager Sections (Latest)\n\n",
    "**Source:** `", section_dir, "`\n",
    "**Updated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n\n",
    "### Section files\n",
    paste(sprintf("- `%s` (modified %s)",
                  SECTION_FILES,
                  format(file.info(full_paths)$mtime, "%Y-%m-%d %H:%M")),
          collapse = "\n"), "\n\n",
    "These files are consumed by the **Build Syphilis Manager** workflow.\n"
)

notes_file <- file.path(tempdir(), "release_notes.md")
writeLines(notes, notes_file)

result <- system2("gh", c(
    "release", "create", RELEASE_TAG,
    "--repo", REPO,
    "--title", shQuote("Syphilis Manager Sections (Latest)"),
    "--notes-file", notes_file,
    full_paths
))

if (result != 0) stop("Failed to create release. Check gh CLI authentication.")

cat("\nDone. Release:", RELEASE_TAG, "\n")
