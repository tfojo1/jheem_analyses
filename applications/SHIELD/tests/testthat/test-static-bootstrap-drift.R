## ==========================================================================
## STATIC TIER  |  tests/shield_test_bootstrap.R vs shield_source_code.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the test bootstrap still loads the same files production does, so the
##   integration tier cannot drift into testing a different model.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=bootstrap-drift
##
## WHY IT MATTERS
##   The test bootstrap deliberately re-implements the loading half of
##   shield_source_code.R. That duplication is only safe if it is checked: if
##   someone adds a file to shield_source_code.R and not to the bootstrap, the
##   integration tier would keep passing while testing a different model than the
##   one that actually runs.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

## production_shield_files ----
## Extract the SHIELD-relative files that shield_source_code.R sources in its
## `for (f in c(...))` loop.
production_shield_files <- function() {
    f <- file.path(SHIELD.DIR, "shield_source_code.R")
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")

    block <- regmatches(src, regexpr("for\\s*\\(\\s*f\\s+in\\s+c\\((?s).*?\\)\\)", src, perl = TRUE))
    if (!length(block)) return(character(0))
    unique(unlist(regmatches(block, gregexpr('"[^"]+\\.R"', block)))) |>
        gsub(pattern = '"', replacement = "")
}

## production_commoncode_files ----
## Files sourced by shield_source_code.R from commoncode/.
production_commoncode_files <- function() {
    f <- file.path(SHIELD.DIR, "shield_source_code.R")
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    m <- unlist(regmatches(lines, gregexpr('"commoncode/[^"]+\\.R"', lines)))
    unique(gsub('"', "", m))
}

test_that("the bootstrap sources every SHIELD file the real loader sources", {
    production <- production_shield_files()
    skip_if(length(production) == 0,
            "could not find the file loop in shield_source_code.R")

    ours <- unique(c(SHIELD.TEST.STANDALONE.FILES,
                     SHIELD.TEST.MANAGER.DEPENDENT.FILES))

    ## Files the real loader sources but the bootstrap does not. Files that no
    ## longer exist are a separate failure, reported by test-static-parse.R, so
    ## they are excluded here to keep the two diagnostics distinct.
    production <- production[file.exists(file.path(SHIELD.DIR, production))]

    expect_equal(
        setdiff(production, ours), character(0),
        info = paste0(
            "shield_source_code.R sources these SHIELD files but the test ",
            "bootstrap does not; the integration tier is testing a different ",
            "environment than production:\n  ",
            paste(setdiff(production, ours), collapse = "\n  "))
    )
})

test_that("the bootstrap sources every commoncode file the real loader sources", {
    production <- production_commoncode_files()
    skip_if(length(production) == 0)

    boot <- readLines(file.path(SHIELD.TEST.ENV$repo.root, "applications/SHIELD",
                                "tests/shield_test_bootstrap.R"), warn = FALSE)
    ours <- unique(gsub('"', "", unlist(regmatches(
        boot, gregexpr('"commoncode/[^"]+\\.R"', boot)))))

    production <- production[file.exists(file.path(REPO.ROOT, production))]

    expect_equal(
        setdiff(production, ours), character(0),
        info = paste0("commoncode files missing from the test bootstrap:\n  ",
                      paste(setdiff(production, ours), collapse = "\n  "))
    )
})

test_that("shield_specification.R still loads its environment the way the bootstrap assumes", {
    ## The bootstrap builds the specification by stripping the single line that
    ## sources shield_source_code.R. If that line moves, changes shape, or is
    ## joined by a second environment loader, the strip silently stops working
    ## and the tests start running git commands.
    f <- file.path(SHIELD.DIR, "shield_specification.R")
    lines <- readLines(f, warn = FALSE)
    code <- lines[!grepl("^\\s*#", lines)]

    loader <- grep("source\\(.*shield_source_code\\.R", code)
    expect_length(loader, 1)
})

test_that("the bootstrap pins a manager rather than following the promoted one", {
    ## A test that silently changes its input whenever a manager is promoted is
    ## not a regression test.
    expect_true(
        !is.null(SHIELD.TEST.MANAGER.TAG) ||
            nzchar(Sys.getenv("JHEEM_SYPHILIS_MANAGER_TAG")),
        info = "SHIELD.TEST.MANAGER.TAG must be pinned, or set explicitly in the environment"
    )
})
