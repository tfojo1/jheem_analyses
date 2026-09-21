## ==========================================================================
## STATIC TIER  |  the source tree as a whole
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   Reproducibility and safety properties: no committed credentials, no
##   hardcoded developer paths, seeded Monte Carlo draws, no tracked scratch
##   files, and no mutating git commands run just by sourcing the model.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=static-hygiene
##
## WHY IT MATTERS
##   Reproducibility and safety properties of the source tree itself.
##
##   Each of these encodes a failure that has already happened in this codebase at
##   least once, and that no amount of running the model would surface.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

test_that("no credentials or pre-signed URLs are committed", {
    files <- shield_r_files(include.scratch = TRUE)

    patterns <- c(
        "pre-signed S3 URL"      = "X-Amz-Signature=|X-Amz-Credential=",
        "AWS access key"         = "AKIA[0-9A-Z]{16}",
        "GitHub token"           = "gh[pousr]_[A-Za-z0-9]{30,}",
        "private key block"      = "BEGIN (RSA |OPENSSH |EC )?PRIVATE KEY"
    )

    hits <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        for (label in names(patterns)) {
            idx <- grep(patterns[[label]], lines)
            if (length(idx)) {
                hits[[length(hits) + 1]] <- paste0(shield_rel(f), ":", idx[1], " (", label, ")")
            }
        }
    }

    expect_equal(
        length(hits), 0,
        info = paste0("possible secrets in source:\n",
                      paste0("  ", unlist(hits), collapse = "\n"))
    )
})

test_that("no model code hardcodes a developer's absolute path", {
    ## Paths like /Users/<someone>/... or Q:/... only work on one machine.
    ## ../jheem_analyses/ is a separate problem, checked below.
    files <- shield_r_files(include.scratch = FALSE)

    hits <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        code <- !grepl("^\\s*#", lines)
        idx <- which(code & grepl("['\"](/Users/|/home/|[A-Z]:/|/Volumes/)", lines))
        ## commoncode/file_paths.R legitimately defines the mount points; SHIELD
        ## model code should not.
        if (length(idx)) {
            hits[[length(hits) + 1]] <- paste0(shield_rel(f), ": lines ",
                                               paste(idx, collapse = ", "))
        }
    }

    expect_equal(
        length(hits), 0,
        info = paste0("absolute paths in SHIELD model code:\n",
                      paste0("  ", unlist(hits), collapse = "\n"))
    )
})

test_that("Monte Carlo draws in the intervention pipeline are seeded", {
    ## intervention_definitions.R draws 1,000 doxy-PEP efficacy values with
    ## rlnorm() and no seed, so every process that sources the file - every
    ## parallel intervention job - uses a different efficacy distribution, and
    ## no run can be reproduced.
    files <- c(
        file.path(SHIELD.DIR, "intervention/intervention_definitions.R"),
        file.path(SHIELD.DIR, "intervention/doxy_effectiveness.R")
    )
    files <- files[file.exists(files)]
    skip_if(length(files) == 0)

    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        code <- lines[!grepl("^\\s*#", lines)]
        draws <- grep("\\b(rlnorm|rnorm|runif|rbeta|rgamma|sample)\\s*\\(", code)
        if (!length(draws)) next

        expect_true(
            any(grepl("set\\.seed\\s*\\(", code)),
            info = paste0(shield_rel(f), " draws random numbers (lines ",
                          paste(draws, collapse = ", "),
                          ") but never calls set.seed(); intervention runs are ",
                          "not reproducible and parallel jobs disagree with ",
                          "each other.")
        )
    }
})

test_that("the repo does not track large binary scratch files", {
    ## .RDataTmp (67 MB), analysis/backups/ and 'untitled folder/' were all
    ## committed. Big blobs make every clone and every CI checkout slower.
    skip_if(nchar(Sys.which("git")) == 0, "git not on PATH")

    tracked <- suppressWarnings(system2(
        "git", c("-C", shQuote(REPO.ROOT), "ls-files"),
        stdout = TRUE, stderr = FALSE))
    tracked <- tracked[nzchar(tracked)]
    skip_if(length(tracked) == 0, "git ls-files returned nothing")

    suspicious <- tracked[grepl("\\.RDataTmp$|\\.Rhistory$|/backups/|untitled folder/|\\.DS_Store$",
                                tracked)]

    expect_equal(
        length(suspicious), 0,
        info = paste0("scratch/backup files tracked in git:\n",
                      paste0("  ", head(suspicious, 20), collapse = "\n"))
    )
})

test_that("sourcing the model never mutates a git repository", {
    ## shield_source_code.R may `git pull` jheem_analyses and ../jheem2 when it
    ## is sourced. With up to 20 chains racing, an ungated pull is both
    ## destructive (git collides on index.lock) and non-deterministic (chains
    ## can end up on different commits). The pulls must be gated off in
    ## non-interactive runs: PULL.GIT.UPDATES <- interactive(), or FALSE.
    f <- file.path(SHIELD.DIR, "shield_source_code.R")
    skip_if_not(file.exists(f))

    lines <- readLines(f, warn = FALSE)
    code <- lines[!grepl("^\\s*#", lines)]

    mutating <- grep("reset\\s*\",?\\s*\"--hard|\"pull\"|checkout", code, value = TRUE)
    gated <- any(grepl("PULL\\.GIT\\.UPDATES\\s*(<-|=)\\s*(F|FALSE|interactive\\(\\))", code)) ||
        any(grepl("Sys\\.getenv\\(.*(FETCH|SYNC|CI)", code))

    expect_true(
        length(mutating) == 0 || gated,
        info = paste0(
            "shield_source_code.R performs mutating git operations that are not ",
            "gated off by default or by an environment variable:\n  ",
            paste(trimws(mutating), collapse = "\n  "),
            "\nThe test suite works around this with its own bootstrap; a real ",
            "run has no such protection.")
    )
})
