# Record where a section upload came from: the jheem_analyses checkout it ran from
# (commit, branch, uncommitted changes under data_processing/ and commoncode/) and the
# R and package versions installed. The section upload scripts source this and attach
# the result to each section release as provenance.json.
#
# This describes the upload, not the section builds: a section file may have been
# built earlier, with other code, so each file's modified time is recorded too.
# Nothing here is enforced; builds report it.

# Commit and branch from .git, for machines where git is not on the PATH
read.git.head <- function(repo) {
    git.dir <- file.path(repo, ".git")
    head.file <- file.path(git.dir, "HEAD")
    if (!file.exists(head.file)) return(NULL)
    head <- readLines(head.file, n = 1, warn = FALSE)
    if (!startsWith(head, "ref: ")) return(list(commit = head, branch = NA_character_))
    ref <- sub("^ref: ", "", head)
    loose <- file.path(git.dir, ref)
    commit <- if (file.exists(loose)) {
        readLines(loose, n = 1, warn = FALSE)
    } else if (file.exists(file.path(git.dir, "packed-refs"))) {
        packed <- readLines(file.path(git.dir, "packed-refs"), warn = FALSE)
        sub(" .*$", "", packed[endsWith(packed, paste0(" ", ref))][1])
    } else NA_character_
    list(commit = commit, branch = sub("^refs/heads/", "", ref))
}

checkout.provenance <- function(repo) {
    git <- function(...) {
        out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(repo), ...),
                                                 stdout = TRUE, stderr = FALSE)),
                        error = function(e) NULL)
        if (is.null(out) || !is.null(attr(out, "status"))) NULL else out
    }
    commit <- git("rev-parse", "HEAD")
    if (is.null(commit)) {
        head <- read.git.head(repo)
        if (is.null(head)) return(list(commit = NULL, note = "not a git checkout"))
        return(list(commit = head$commit, branch = head$branch, uncommitted_changes = NULL,
                    note = "git not available: commit read from .git; uncommitted changes unknown"))
    }
    changes <- git("status", "--porcelain", "--", "data_processing", "commoncode")
    ahead <- git("rev-list", "--count", "origin/master..HEAD")
    list(commit = commit,
         branch = git("rev-parse", "--abbrev-ref", "HEAD"),
         commit_date = git("log", "-1", "--format=%cI"),
         commits_not_on_origin_master = if (is.null(ahead)) NULL else as.integer(ahead),
         uncommitted_changes = if (is.null(changes)) NULL else I(head(changes, 100)),
         uncommitted_change_count = if (is.null(changes)) NULL else length(changes))
}

package.provenance <- function(name) {
    description <- suppressWarnings(utils::packageDescription(name))
    if (!is.list(description)) return(NULL)
    list(version = description$Version,
         remote_type = description$RemoteType,
         remote_sha = description$RemoteSha,
         remote_ref = description$RemoteRef)
}

utc.time <- function(time) format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Writes provenance.json to out_file and returns a one-line summary for release notes.
# repo is the jheem_analyses checkout the upload script runs from.
write.section.provenance <- function(manager, section_dir, files, digests, out_file, repo) {
    repo <- normalizePath(repo, mustWork = FALSE)
    paths <- file.path(section_dir, files)
    record <- list(
        schema_version = 1L,
        manager = manager,
        uploaded_at = utc.time(Sys.time()),
        section_dir = section_dir,
        sections = lapply(seq_along(files), function(i) list(
            file = files[i],
            sha256 = digests[i],
            size = file.size(paths[i]),
            modified = utc.time(file.mtime(paths[i])))),
        checkout = checkout.provenance(repo),
        r_version = paste(R.version$major, R.version$minor, sep = "."),
        platform = R.version$platform,
        packages = list(jheem2 = package.provenance("jheem2"),
                        locations = package.provenance("locations"),
                        distributions = package.provenance("distributions")))
    writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", pretty = TRUE),
               out_file)

    checkout <- record$checkout
    where <- if (is.null(checkout$commit)) {
        "a directory that is not a git checkout"
    } else {
        branch <- if (is.null(checkout$branch) || is.na(checkout$branch)) "" else
            paste0(" (`", checkout$branch, "`)")
        changes <- if (is.null(checkout$uncommitted_change_count)) {
            ", uncommitted changes unknown"
        } else if (checkout$uncommitted_change_count == 0) {
            ", no uncommitted changes under `data_processing/` or `commoncode/`"
        } else {
            paste0(", ", checkout$uncommitted_change_count,
                   " uncommitted change(s) under `data_processing/` or `commoncode/`")
        }
        paste0("jheem_analyses `", substr(checkout$commit, 1, 8), "`", branch, changes)
    }
    jheem2 <- record$packages$jheem2
    jheem2.text <- if (is.null(jheem2)) "jheem2 not installed" else
        paste0("jheem2 ", jheem2$version,
               if (!is.null(jheem2$remote_sha)) paste0(" (`", substr(jheem2$remote_sha, 1, 8), "`)") else "")
    paste0("**Uploaded from:** ", where, "; R ", record$r_version, "; ", jheem2.text)
}
