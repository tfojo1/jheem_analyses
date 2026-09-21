## ==========================================================================
## STATIC TIER  |  every .R file under applications/SHIELD
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   Every SHIELD .R file parses, every literal source() target exists, no file
##   contains pasted diff or merge-conflict markers, and no tracked .R file has
##   been deleted from the working tree.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=static-parse
##
## WHY IT MATTERS
##   Every SHIELD .R file must be syntactically valid, and every source() target
##   must exist.
##
##   This tier needs no data manager and no jheem2. It is the cheapest possible
##   guard, and it is the one that would have caught:
##     - the literal `+` diff markers pasted into make.mv.spline.prior()
##     - analysis/doxy_summary_aug.R being truncated mid-expression
##     - test/shield_doxy_int_reprex.R sourcing a path that does not exist
##     - shield_specification.R sourcing R/shield_locations_of_interest.R after
##       that file was deleted
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

test_that("every SHIELD .R file parses", {
    files <- shield_r_files(include.scratch = TRUE)
    expect_gt(length(files), 50)

    failures <- vapply(files, function(f) {
        err <- tryCatch({ parse(f); NA_character_ },
                        error = function(e) conditionMessage(e))
        err
    }, character(1))

    broken <- failures[!is.na(failures)]
    expect_equal(
        length(broken), 0,
        info = paste0("files that do not parse:\n",
                      paste0("  ", shield_rel(names(broken)), ": ",
                             gsub("\n", " ", broken), collapse = "\n"))
    )
})

test_that("no source file contains diff markers or merge conflict markers", {
    ## `make.mv.spline.prior()` was "fixed" by pasting three lines straight out
    ## of a diff view, leaving a literal leading `+` on each. R parses that as
    ## unary plus, so the function threw `could not find function "+<-"` at run
    ## time while still parsing cleanly. Parsing alone cannot catch this.
    files <- shield_r_files(include.scratch = FALSE)

    offenders <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        ## a code line that begins with +, ++ or +++ followed by an identifier
        diff.marker <- grepl("^\\s*\\+{1,3}\\s*[A-Za-z._]", lines) &
            !grepl("^\\s*#", lines)
        conflict <- grepl("^(<<<<<<<|=======|>>>>>>>)", lines)
        hits <- which(diff.marker | conflict)
        if (length(hits)) {
            offenders[[shield_rel(f)]] <- paste0(hits, ": ", trimws(lines[hits]))
        }
    }

    expect_equal(
        length(offenders), 0,
        info = paste0("diff/conflict markers found:\n",
                      paste(names(offenders), vapply(offenders, paste, character(1),
                                                     collapse = " | "),
                            sep = " -> ", collapse = "\n"))
    )
})

test_that("every literal source() target exists", {
    files <- shield_r_files(include.scratch = FALSE)

    missing <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        code <- lines[!grepl("^\\s*#", lines)]
        ## only literal single-argument paths; anything built with paste0() or
        ## file.path() is out of scope for a static check
        m <- regmatches(code, regexpr("source\\(\\s*['\"][^'\"]+['\"]", code))
        if (!length(m)) next
        targets <- gsub("^source\\(\\s*['\"]|['\"]$", "", m)

        for (t in targets) {
            ## SHIELD source() paths are written against three different roots
            ## depending on the file: the repo's parent ("../jheem_analyses/..."),
            ## the repo root, and the SHIELD directory. Accept any of them.
            candidates <- c(
                file.path(REPO.ROOT, sub("^\\.\\./jheem_analyses/", "", t)),
                file.path(REPO.ROOT, t),
                file.path(SHIELD.DIR, t),
                file.path(dirname(f), t)
            )
            if (!any(file.exists(candidates))) {
                missing[[length(missing) + 1]] <- paste0(shield_rel(f), " -> ", t)
            }
        }
    }

    expect_equal(
        length(missing), 0,
        info = paste0("source() targets that do not exist:\n",
                      paste0("  ", unlist(missing), collapse = "\n"))
    )
})

test_that("no tracked SHIELD file is missing from the working tree", {
    ## A deleted-but-tracked file is invisible to `parse()` (there is nothing to
    ## parse) and invisible to a normal run until something sources it.
    skip_if(nchar(Sys.which("git")) == 0, "git not on PATH")

    status <- suppressWarnings(system2(
        "git", c("-C", shQuote(REPO.ROOT), "status", "--porcelain", "--",
                 "applications/SHIELD"),
        stdout = TRUE, stderr = FALSE))

    deleted <- grep("^ ?D ", status, value = TRUE)
    deleted <- deleted[grepl("\\.[Rr]$", deleted)]

    expect_equal(
        length(deleted), 0,
        info = paste0("tracked .R files deleted from the working tree:\n",
                      paste0("  ", deleted, collapse = "\n"))
    )
})
