# Keep an immutable copy of each data manager published through the OneDrive cache.
#
# commoncode/data_manager_cache_metadata.Rdata points each manager file at a OneDrive
# link, and each update replaces the file behind that link. For each manager in the
# metadata (or each one named), this script downloads the OneDrive file, checks that it
# is the version the metadata describes (the same creation and last-modified dates), and
# publishes it as a release named after the file and its last-modified date (UTC), e.g.
# census-manager-onedrive-v2026.10.26 for census.manager.rdata. A second version from the
# same day gets .2, and so on. A version that already has a copy is skipped, so the script
# can be re-run at any time.
#
# The asset keeps the file's name, so a manager that loads from GitHub Releases (see
# commoncode/data_manager_sources.json) can load the copy by tag:
#   load.data.manager.from.cache("census.manager.rdata",
#                                release.tag = "census-manager-onedrive-v2026.10.26")
#
# The Snapshot OneDrive Managers workflow runs this whenever the metadata changes on master.
#
# Usage, from the repository root:
#   Rscript scripts/snapshot_onedrive_managers.R                        # every manager
#   Rscript scripts/snapshot_onedrive_managers.R census.manager.rdata   # named managers
# Add --dry-run to download and check without publishing.
#
# A problem with one manager (a failed download, or a file that doesn't match the metadata)
# is reported and that manager is skipped, so a copy is never published under the wrong
# version; the script still exits 0. If SNAPSHOT_SUMMARY names a file, a Markdown summary
# is appended to it (the workflow uses the job summary).
#
# Requires: gh CLI installed and authenticated; jheem2 (to read the managers), httr2,
# jsonlite, openssl

REPO <- Sys.getenv("SNAPSHOT_REPO", "tfojo1/jheem_analyses")
METADATA_FILE <- "commoncode/data_manager_cache_metadata.Rdata"
SOURCES_FILE <- "commoncode/data_manager_sources.json"

# --- Parse arguments ---
args <- commandArgs(trailingOnly = TRUE)
dry.run <- "--dry-run" %in% args
requested <- setdiff(args, "--dry-run")

for (package in c("jheem2", "httr2", "jsonlite", "openssl")) {
    if (!requireNamespace(package, quietly = TRUE)) {
        stop("The '", package, "' package is required")
    }
}
if (!file.exists(METADATA_FILE)) {
    stop("Run this from the jheem_analyses root: ", METADATA_FILE, " not found")
}
if (system2("gh", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("gh CLI not found. Install from https://cli.github.com/")
}

cache.metadata <- get(load(METADATA_FILE))
files <- if (length(requested) > 0) requested else names(cache.metadata)
unknown <- setdiff(files, names(cache.metadata))
if (length(unknown) > 0) {
    stop("Not in ", METADATA_FILE, ": ", paste(unknown, collapse = ", "))
}

# Each copy's tag points at the commit whose metadata describes it.
if (length(system2("git", c("status", "--porcelain", "--", METADATA_FILE), stdout = TRUE)) > 0) {
    stop(METADATA_FILE, " has uncommitted changes")
}
metadata.commit <- system2("git", c("log", "-1", "--format=%H", "--", METADATA_FILE), stdout = TRUE)
if (length(metadata.commit) != 1 || !grepl("^[0-9a-f]{40}$", metadata.commit)) {
    stop("Cannot find the commit that last changed ", METADATA_FILE)
}

loads.by.tag <- if (file.exists(SOURCES_FILE)) names(jsonlite::fromJSON(SOURCES_FILE)) else character()
copied.by <- if (nzchar(Sys.getenv("GITHUB_RUN_ID"))) {
    paste0(Sys.getenv("GITHUB_SERVER_URL"), "/", Sys.getenv("GITHUB_REPOSITORY"),
           "/actions/runs/", Sys.getenv("GITHUB_RUN_ID"))
} else {
    "scripts/snapshot_onedrive_managers.R, run locally"
}
utc <- function(time) format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
staging_dir <- file.path(tempdir(), "onedrive_snapshots")
dir.create(staging_dir, showWarnings = FALSE)

# --- Releases already made ---
release.exists <- function(tag) {
    system2("gh", c("release", "view", tag, "--repo", REPO),
            stdout = FALSE, stderr = FALSE) == 0
}
# The version recorded in a copy's snapshot.json, or NULL if it has none
kept.version <- function(tag) {
    dir <- file.path(staging_dir, paste0("kept_", tag))
    dir.create(dir, showWarnings = FALSE)
    status <- system2("gh", c("release", "download", tag, "--repo", REPO,
                              "--pattern", "snapshot.json", "--dir", shQuote(dir), "--clobber"),
                      stdout = FALSE, stderr = FALSE)
    record.file <- file.path(dir, "snapshot.json")
    if (status != 0 || !file.exists(record.file)) return(NULL)
    jsonlite::fromJSON(record.file)
}

# The tag for this version: its existing copy, or the first free tag for its date
choose.tag <- function(file, entry) {
    stem <- gsub(".", "-", sub("[.]rdata$", "", file), fixed = TRUE)
    base <- paste0(stem, "-onedrive-v", format(entry$last.modified.date, "%Y.%m.%d", tz = "UTC"))
    for (tag in c(base, paste0(base, ".", 2:20))) {
        if (!release.exists(tag)) return(list(tag = tag, kept = FALSE))
        kept <- kept.version(tag)
        if (!is.null(kept) && identical(kept$file, file) &&
            identical(kept$creation_date, utc(entry$creation.date)) &&
            identical(kept$last_modified_date, utc(entry$last.modified.date))) {
            return(list(tag = tag, kept = TRUE))
        }
    }
    stop("No free tag for ", file, " from ", base)
}

# --- Copy one manager ---
# Returns a one-line outcome; stops with a message if it can't be copied.
snapshot.manager <- function(file) {
    entry <- cache.metadata[[file]]
    if (is.null(entry$onedrive.link) || is.null(entry$creation.date) ||
        is.null(entry$last.modified.date)) {
        stop("its metadata has no OneDrive link or dates")
    }
    choice <- choose.tag(file, entry)
    if (choice$kept) return(paste0("already kept as `", choice$tag, "`"))

    # Download under the file's own name, which becomes the asset name
    dir <- file.path(staging_dir, choice$tag)
    dir.create(dir, showWarnings = FALSE)
    path <- file.path(dir, file)
    request <- httr2::req_timeout(httr2::request(entry$onedrive.link), 1800)
    response <- httr2::req_perform(request, path = path)
    if (httr2::resp_status(response) != 200 || !file.exists(path) || file.size(path) == 0) {
        stop("the OneDrive download failed (HTTP ", httr2::resp_status(response), ")")
    }

    # Check it is the version the metadata describes
    loaded <- new.env()
    names <- load(path, envir = loaded)
    if (length(names) != 1) stop("the OneDrive file does not hold a single object")
    data.manager <- get(names, envir = loaded)
    if (!inherits(data.manager, "R6") || !is(data.manager, "jheem.data.manager")) {
        stop("the OneDrive file does not hold a JHEEM data manager")
    }
    if (is.null(data.manager$creation.date) || is.null(data.manager$last.modified.date) ||
        as.numeric(data.manager$creation.date) != as.numeric(entry$creation.date) ||
        as.numeric(data.manager$last.modified.date) != as.numeric(entry$last.modified.date)) {
        stop("the OneDrive file (last modified ", utc(data.manager$last.modified.date),
             ") is not the version in the metadata (last modified ",
             utc(entry$last.modified.date), ")")
    }
    manager.name <- data.manager$name
    rm(loaded, data.manager)
    invisible(gc())

    size <- file.size(path)
    connection <- file(path, open = "rb")
    sha256 <- as.vector(as.character(openssl::sha256(connection)))
    close(connection)
    record <- list(schema_version = 1L,
                   file = file,
                   tag = choice$tag,
                   size = size,
                   sha256 = sha256,
                   manager_name = manager.name,
                   creation_date = utc(entry$creation.date),
                   last_modified_date = utc(entry$last.modified.date),
                   metadata_file = METADATA_FILE,
                   metadata_commit = metadata.commit,
                   copied_at = utc(Sys.time()),
                   copied_by = copied.by)
    record.file <- file.path(dir, "snapshot.json")
    jsonlite::write_json(record, record.file, auto_unbox = TRUE, pretty = TRUE, digits = NA)

    day <- format(entry$last.modified.date, "%Y-%m-%d", tz = "UTC")
    notes <- c(
        paste0("## `", file, "` as published to OneDrive, ", day),
        "",
        paste0("A copy of the file that the OneDrive link in `", METADATA_FILE, "` served at `",
               substr(metadata.commit, 1, 8), "`, kept because each update replaces that file. ",
               "Its creation and last-modified dates match the metadata. It is a copy, not a build: ",
               "no other checks were run."),
        "",
        "| | |",
        "|---|---|",
        paste0("| **Asset** | `", file, "` (", format(size, big.mark = ","), " bytes) |"),
        paste0("| **SHA-256** | `", sha256, "` |"),
        paste0("| **Manager name** | `", manager.name, "` |"),
        paste0("| **Created / last modified (UTC)** | ", utc(entry$creation.date), " / ",
               utc(entry$last.modified.date), " |"),
        paste0("| **Metadata commit** | `", metadata.commit, "` |"),
        paste0("| **Copied** | ", record$copied_at, ", by ", copied.by, " |"),
        "",
        "`snapshot.json` holds the same facts for scripts.",
        if (file %in% loads.by.tag) c(
            "",
            "Load this version with",
            "```r",
            paste0("load.data.manager.from.cache(\"", file, "\", release.tag = \"", choice$tag, "\")"),
            "```")
    )
    notes.file <- file.path(dir, "notes.md")
    writeLines(notes, notes.file)

    if (dry.run) {
        cat("Release notes:\n", paste(notes, collapse = "\n"), "\n\nsnapshot.json:\n",
            paste(readLines(record.file), collapse = "\n"), "\n", sep = "")
        return(paste0("would be kept as `", choice$tag, "` (dry run)"))
    }
    status <- system2("gh", c("release", "create", choice$tag,
                              "--repo", REPO,
                              "--target", metadata.commit,
                              "--title", shQuote(paste0("OneDrive copy: ", file, ", ", day)),
                              "--notes-file", shQuote(notes.file),
                              "--latest=false",
                              shQuote(path), shQuote(record.file)))
    unlink(path)
    if (status != 0) stop("creating release ", choice$tag, " failed")
    paste0("kept as `", choice$tag, "`")
}

# --- Run ---
cat("Metadata:", METADATA_FILE, "at", substr(metadata.commit, 1, 8), "\n")
outcomes <- character()
for (file in files) {
    cat("\n===", file, "===\n")
    outcome <- tryCatch(snapshot.manager(file),
                        error = function(e) paste0("**not kept**: ", conditionMessage(e)))
    cat(outcome, "\n")
    if (startsWith(outcome, "**not kept**") && nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
        cat("::warning::", file, " not kept: ", sub("^[*]+not kept[*]+: ", "", outcome), "\n", sep = "")
    }
    outcomes[file] <- outcome
}

summary <- c("## OneDrive manager copies", "",
             paste0("Metadata at `", substr(metadata.commit, 1, 8), "`", if (dry.run) " (dry run)"), "",
             "| File | Outcome |", "|---|---|",
             paste0("| `", names(outcomes), "` | ", outcomes, " |"))
summary.file <- Sys.getenv("SNAPSHOT_SUMMARY")
if (nzchar(summary.file)) write(summary, summary.file, append = TRUE)
cat("\n", paste(summary, collapse = "\n"), "\n", sep = "")
