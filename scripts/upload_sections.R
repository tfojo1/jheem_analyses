# Upload syphilis-manager section .rdata files for the CI merge build.
#
# Each upload creates a new immutable, dated release
# (syphilis-sections-vYYYY.MM.DD) with a SHA256SUMS.txt manifest, then points
# the syphilis-sections-latest release at it, as the census and HIV uploads
# do. The Build Syphilis Manager workflow reads syphilis-sections-latest by
# default and records which dated snapshot it used.
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
# Add --dry-run to check the files and print what would be uploaded.
#
# Requires: gh CLI installed and authenticated; the digest and jsonlite R packages

REPO <- "tfojo1/jheem_analyses"
LATEST_TAG <- "syphilis-sections-latest"
SECTION_FILES <- paste0("syphilis.manager_section", 1:4, ".rdata")

# --- Parse arguments ---
args <- commandArgs(trailingOnly = TRUE)
dry.run <- "--dry-run" %in% args
args <- setdiff(args, "--dry-run")
if (length(args) < 1) {
    cat("Usage: Rscript scripts/upload_sections.R <nas_root> [section_dir] [--dry-run]\n\n")
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

for (package in c("digest", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
        stop("The '", package, "' package is required: install.packages('", package, "')")
    }
}
# provenance.json: where this upload ran from (scripts/section_provenance.R)
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(script_dir, "section_provenance.R"))

# --- Check gh CLI ---
if (!dry.run && system2("gh", "--version", stdout = FALSE, stderr = FALSE) != 0) {
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

# --- Write the checksum manifest ---
cat("\nComputing SHA-256 digests...\n")
digests <- vapply(full_paths, function(path) digest::digest(file = path, algo = "sha256"),
                  character(1), USE.NAMES = FALSE)
staging_dir <- file.path(tempdir(), "syphilis_sections_upload")
dir.create(staging_dir, showWarnings = FALSE)
manifest_file <- file.path(staging_dir, "SHA256SUMS.txt")
writeLines(paste(digests, SECTION_FILES, sep = "  "), manifest_file)
manifest_sha <- digest::digest(file = manifest_file, algo = "sha256")
provenance_file <- file.path(staging_dir, "provenance.json")
provenance_line <- write.section.provenance("syphilis", section_dir, SECTION_FILES, digests,
                                            provenance_file, repo = file.path(script_dir, ".."))

# --- Choose the dated tag ---
release.exists <- function(tag) {
    system2("gh", c("release", "view", tag, "--repo", REPO),
            stdout = FALSE, stderr = FALSE) == 0
}
version_tag <- paste0("syphilis-sections-v", format(Sys.Date(), "%Y.%m.%d"))
if (!dry.run && release.exists(version_tag)) {
    seq <- 2
    while (release.exists(paste0(version_tag, ".", seq))) seq <- seq + 1
    version_tag <- paste0(version_tag, ".", seq)
}

file_lines <- sprintf("- `%s` (modified %s) `%s`",
                      SECTION_FILES,
                      format(file.info(full_paths)$mtime, "%Y-%m-%d %H:%M"),
                      digests)
common_notes <- c(
    paste0("**Source:** `", section_dir, "`"),
    paste0("**Uploaded:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    paste0("**Manifest SHA-256:** `", manifest_sha, "`"),
    provenance_line,
    "",
    "### Section files",
    file_lines,
    "",
    "These files are consumed by the **Build Syphilis Manager** workflow.",
    paste("`provenance.json` records the checkout and packages this upload ran from.",
          "The sections may have been built earlier, as their modified times show;",
          "raw inputs are not recorded.")
)

cat("\nDated release:  ", version_tag, "\n")
cat("Latest pointer: ", LATEST_TAG, "\n")
cat("Manifest SHA-256:", manifest_sha, "\n")
cat(sub("^[*]+Uploaded from:[*]+", "Uploaded from:  ", provenance_line), "\n")

if (dry.run) {
    cat("\nprovenance.json:\n", paste(readLines(provenance_file), collapse = "\n"), "\n", sep = "")
    cat("\nDry run: nothing uploaded.\n")
    quit(status = 0)
}

create.release <- function(tag, title, notes) {
    notes_file <- file.path(staging_dir, paste0(tag, "_notes.md"))
    writeLines(notes, notes_file)
    result <- system2("gh", c(
        "release", "create", tag,
        "--repo", REPO,
        "--title", shQuote(title),
        "--notes-file", notes_file,
        "--latest=false",
        full_paths, manifest_file, provenance_file
    ))
    if (result != 0) stop("Failed to create release ", tag, ". Check gh CLI authentication.")
}

# --- Immutable dated snapshot ---
cat("\nCreating", version_tag, "...\n")
create.release(version_tag,
               paste("Syphilis Manager Sections", version_tag),
               c(paste0("## Syphilis Manager Sections (", version_tag, ")"), "",
                 common_notes, "",
                 "This release is a fixed snapshot. Do not replace its assets."))

# --- Move the latest pointer ---
# Like census-sections-latest, this release is mutable. The build records
# the dated snapshot named below, so every build stays traceable.
system2("gh", c("release", "delete", LATEST_TAG, "--repo", REPO,
                "--cleanup-tag", "--yes"),
        stdout = FALSE, stderr = FALSE)  # OK if it doesn't exist
cat("Updating", LATEST_TAG, "...\n")
create.release(LATEST_TAG,
               "Syphilis Manager Sections (Latest)",
               c("## Syphilis Manager Sections (Latest)", "",
                 paste0("**Snapshot:** `", version_tag, "`"),
                 common_notes))

cat("\nDone. Release:", version_tag, "(also available as", LATEST_TAG, ")\n")
cat("Next: run the 'Build Syphilis Manager' workflow.\n")
