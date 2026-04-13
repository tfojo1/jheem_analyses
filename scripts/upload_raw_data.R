# Upload a snapshot of raw data from the NAS/Q: drive to a GitHub Release.
#
# Creates one compressed tarball per data_raw/ subdirectory and uploads
# them as assets on a single versioned, immutable release.
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

# --- Survey and compress each subdirectory ---
cat("=== Scanning", data_raw, "===\n\n")

top_dirs <- list.dirs(data_raw, recursive = FALSE, full.names = FALSE)
tarball_info <- list()

for (d in top_dirs) {
    full_path <- file.path(data_raw, d)
    files <- list.files(full_path, recursive = TRUE)
    total_bytes <- sum(file.info(file.path(full_path, files))$size, na.rm = TRUE)
    cat(sprintf("  %s/ (%d files, %s)\n", d, length(files),
                format(structure(total_bytes, class = "object_size"), units = "auto")))
}

cat("\n=== Compressing each subdirectory ===\n\n")

for (d in top_dirs) {
    tarball_name <- paste0("data-raw-", d, ".tar.gz")
    tarball_path <- file.path(tempdir(), tarball_name)

    cat(sprintf("Compressing %s/ ... ", d))

    old_wd <- setwd(nas_root)
    tar(tarball_path, files = file.path("data_raw", d), compression = "gzip")
    setwd(old_wd)

    tarball_size <- file.info(tarball_path)$size
    cat(sprintf("%s\n", format(structure(tarball_size, class = "object_size"), units = "auto")))

    if (tarball_size > 2 * 1024^3) {
        stop("Archive for ", d, " exceeds GitHub's 2GB per-file limit (",
             round(tarball_size / 1024^3, 1), " GB). ",
             "This subdirectory may need further splitting.")
    }

    tarball_info[[d]] <- list(name = tarball_name, path = tarball_path, size = tarball_size)
}

total_size <- sum(sapply(tarball_info, `[[`, "size"))
cat(sprintf("\nTotal: %d archives, %s compressed\n",
            length(tarball_info),
            format(structure(total_size, class = "object_size"), units = "auto")))

# --- Create release with all assets ---
cat("\nCreating release:", release_tag, "\n")
cat("Repository:", REPO, "\n\n")

notes <- paste0(
    "## Raw Data Snapshot\n\n",
    "One archive per `data_raw/` subdirectory.\n\n",
    "**Source:** `", data_raw, "`\n",
    "**Created:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n",
    "**Total:** ", format(structure(total_size, class = "object_size"), units = "auto"),
    " compressed (", length(tarball_info), " archives)\n\n",
    "### Contents\n",
    paste(sprintf("- `%s` (%s)", sapply(tarball_info, `[[`, "name"),
                  sapply(tarball_info, function(x) format(structure(x$size, class = "object_size"), units = "auto"))),
          collapse = "\n"), "\n\n",
    "### Extraction\n",
    "```bash\n",
    "# Extract all archives into a directory:\n",
    "for f in data-raw-*.tar.gz; do tar -xzf \"$f\" -C .; done\n",
    "# Result: data_raw/{subdirectory}/...\n",
    "```\n"
)

notes_file <- file.path(tempdir(), "release_notes.md")
writeLines(notes, notes_file)

# Create the release (no assets yet)
result <- system2("gh", c(
    "release", "create", release_tag,
    "--repo", REPO,
    "--title", shQuote(paste("Raw Data", version_tag)),
    "--notes-file", notes_file
))
if (result != 0) stop("Failed to create release. Check gh CLI authentication.")

# Upload each tarball as an asset
for (d in names(tarball_info)) {
    info <- tarball_info[[d]]
    cat(sprintf("Uploading %s (%s) ... ", info$name,
                format(structure(info$size, class = "object_size"), units = "auto")))

    result <- system2("gh", c(
        "release", "upload", release_tag,
        "--repo", REPO,
        info$path
    ))

    if (result != 0) {
        cat("FAILED\n")
        warning("Failed to upload ", info$name, ". You can retry with:\n",
                "  gh release upload ", release_tag, " --repo ", REPO, " ", info$path)
    } else {
        cat("OK\n")
    }
}

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

# Create the latest release (no assets yet)
result <- system2("gh", c(
    "release", "create", LATEST_TAG,
    "--repo", REPO,
    "--title", shQuote("Raw Data (Latest)"),
    "--notes-file", latest_notes_file
))
if (result != 0) {
    warning("Failed to create ", LATEST_TAG, " — versioned release is still available at ", release_tag)
} else {
    # Upload each tarball
    for (d in names(tarball_info)) {
        info <- tarball_info[[d]]
        system2("gh", c("release", "upload", LATEST_TAG, "--repo", REPO, info$path))
    }
    cat("Done. Latest:", LATEST_TAG, "\n")
}
