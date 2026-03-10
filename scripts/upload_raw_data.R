# Upload a snapshot of raw data from the NAS/Q: drive to a GitHub Release.
#
# Creates a compressed tarball of everything under data_raw/ and uploads
# it as a versioned, immutable release. Any manager build pipeline can
# reference it.
#
# Usage:
#   Rscript scripts/upload_raw_data.R Q:                       # Windows
#   Rscript scripts/upload_raw_data.R /mnt/jheem_nas_share     # Linux
#   Rscript scripts/upload_raw_data.R /Volumes/jheem$          # macOS
#
# Optional second argument overrides the version tag:
#   Rscript scripts/upload_raw_data.R Q: v2026.03.09
#
# Requires: gh CLI installed and authenticated (https://cli.github.com/)

REPO <- "tfojo1/jheem_analyses"
TAG_PREFIX <- "data-raw"

# --- Parse arguments ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
    cat("Usage: Rscript scripts/upload_raw_data.R <nas_root> [version_tag]\n\n")
    cat("  nas_root      Path to NAS root (e.g. Q:, /mnt/jheem_nas_share)\n")
    cat("  version_tag   Optional (default: today's date, e.g. v2026.03.09)\n")
    quit(status = 1)
}

nas_root <- args[1]
version_tag <- if (length(args) >= 2) args[2] else paste0("v", format(Sys.Date(), "%Y.%m.%d"))
release_tag <- paste0(TAG_PREFIX, "-", version_tag)

data_raw <- file.path(nas_root, "data_raw")
if (!dir.exists(data_raw)) {
    stop("data_raw directory not found at: ", data_raw)
}

# --- Check gh CLI ---
if (system2("gh", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("gh CLI not found. Install from https://cli.github.com/")
}

# --- Check for existing release ---
if (system2("gh", c("release", "view", release_tag, "--repo", REPO),
            stdout = FALSE, stderr = FALSE) == 0) {
    stop("Release ", release_tag, " already exists.\n",
         "Raw data releases are immutable. Use a different version tag,\n",
         "or delete the existing release if it was created in error.")
}

# --- Survey contents ---
cat("=== Scanning", data_raw, "===\n\n")

top_dirs <- list.dirs(data_raw, recursive = FALSE, full.names = FALSE)
for (d in top_dirs) {
    full_path <- file.path(data_raw, d)
    files <- list.files(full_path, recursive = TRUE)
    total_bytes <- sum(file.info(file.path(full_path, files))$size, na.rm = TRUE)
    cat(sprintf("  %s/ (%d files, %s)\n", d, length(files),
                format(structure(total_bytes, class = "object_size"), units = "auto")))
}

# --- Create tarball ---
tarball_name <- paste0("data-raw-", version_tag, ".tar.gz")
tarball_path <- file.path(tempdir(), tarball_name)

cat("\nCompressing all of data_raw/ ...\n")
cat("This may take several minutes for large datasets.\n\n")

old_wd <- setwd(nas_root)
tar(tarball_path, files = "data_raw", compression = "gzip")
setwd(old_wd)

tarball_size <- file.info(tarball_path)$size
cat(sprintf("Archive: %s (%s)\n", tarball_name,
            format(structure(tarball_size, class = "object_size"), units = "auto")))

# --- Check 2GB limit ---
if (tarball_size > 2 * 1024^3) {
    stop("Archive exceeds GitHub's 2GB per-file limit (",
         round(tarball_size / 1024^3, 1), " GB).\n",
         "The raw data may need to be split into multiple archives.")
}

# --- Upload ---
cat("\nCreating release:", release_tag, "\n")
cat("Repository:", REPO, "\n\n")

notes <- paste0(
    "## Raw Data Snapshot\n\n",
    "**Source:** `", data_raw, "`\n",
    "**Created:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n",
    "**Size:** ", format(structure(tarball_size, class = "object_size"), units = "auto"),
    " compressed\n\n",
    "### Contents\n",
    paste(sprintf("- `data_raw/%s/`", top_dirs), collapse = "\n"), "\n"
)

notes_file <- file.path(tempdir(), "release_notes.md")
writeLines(notes, notes_file)

result <- system2("gh", c(
    "release", "create", release_tag,
    "--repo", REPO,
    "--title", shQuote(paste("Raw Data", version_tag)),
    "--notes-file", notes_file,
    tarball_path
))

if (result != 0) stop("Failed to create release. Check gh CLI authentication.")

cat("\nDone. Release:", release_tag, "\n")
