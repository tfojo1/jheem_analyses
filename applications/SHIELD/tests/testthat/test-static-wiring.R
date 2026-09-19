## ==========================================================================
## STATIC TIER  |  shield_calib_register.R, launchers/*.sh, intervention/*.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   Cross-file agreement: the calibration codes the launchers run are ones the
##   register defines, the interventions the runner asks for are ones the
##   definitions build, and the sampling blocks name real prior parameters.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=static-wiring
##
## WHY IT MATTERS
##   Cross-file wiring: the codes one file produces must be the codes another file
##   asks for. These mismatches are invisible to `parse()` and only surface as a
##   failed cluster job hours later.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

## --- helpers ------------------------------------------------------------------

## Calibration codes that shield_calib_register.R actually registers (commented
## out registrations do not count - that is the whole point).
registered_calibration_codes <- function() {
    f <- file.path(SHIELD.DIR, "shield_calib_register.R")
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    m <- regmatches(lines, regexpr("register\\.calibration\\.info\\(\\s*['\"][^'\"]+['\"]", lines))
    unique(gsub("^register\\.calibration\\.info\\(\\s*['\"]|['\"]$", "", m))
}

## Calibration codes referenced by the shell launchers, excluding comments and
## usage examples.
launcher_calibration_codes <- function() {
    files <- list.files(file.path(SHIELD.DIR, "launchers"), pattern = "\\.sh$",
                        full.names = TRUE)
    out <- list()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        keep <- !grepl("^\\s*#", lines)
        m <- regmatches(lines[keep], gregexpr("calib\\.[A-Za-z0-9._]+", lines[keep]))
        codes <- unique(unlist(m))
        if (length(codes)) out[[basename(f)]] <- codes
    }
    out
}

test_that("every calibration code a launcher runs is registered", {
    ## launch_run_chains.sh ran calib.9.10.stage3.az while the register's latest
    ## was calib.9.11.stage3.az - a one-character difference that costs a whole
    ## cluster run.
    registered <- registered_calibration_codes()
    expect_gt(length(registered), 0)

    referenced <- launcher_calibration_codes()
    unregistered <- lapply(referenced, setdiff, y = registered)
    unregistered <- unregistered[lengths(unregistered) > 0]

    expect_equal(
        length(unregistered), 0,
        info = paste0("launchers referencing unregistered calibration codes:\n",
                      paste(names(unregistered),
                            vapply(unregistered, paste, character(1), collapse = ", "),
                            sep = " -> ", collapse = "\n"))
    )
})

test_that("every calibration code's preceding code is also registered", {
    f <- file.path(SHIELD.DIR, "shield_calib_register.R")
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]

    registered <- registered_calibration_codes()
    m <- regmatches(lines, regexpr("preceding\\.calibration\\.codes\\s*=\\s*['\"][^'\"]+['\"]", lines))
    preceding <- unique(gsub("^preceding\\.calibration\\.codes\\s*=\\s*['\"]|['\"]$", "", m))

    expect_equal(
        setdiff(preceding, registered), character(0),
        info = "a calibration stage names a predecessor that is not registered"
    )
})

test_that("the intervention runner asks only for interventions the definitions build", {
    ## intervention_definitions.R builds doxy.cov.5 .. doxy.cov.50 in steps of 5;
    ## intervention_run.R asks for doxy.cov.10 .. doxy.cov.100 in steps of 10.
    ## With stop.for.errors = FALSE the run simply produces nothing for the
    ## missing half and reports success.
    defs <- file.path(SHIELD.DIR, "intervention/intervention_definitions.R")
    runner <- file.path(SHIELD.DIR, "intervention/intervention_run.R")
    skip_if_not(file.exists(defs) && file.exists(runner))

    ## Built codes: read the seq() that drives the loop rather than guessing.
    def.lines <- readLines(defs, warn = FALSE)
    seq.line <- grep("for\\s*\\(\\s*coverage\\s+in\\s+seq\\(", def.lines, value = TRUE)
    expect_length(seq.line, 1)
    seq.args <- as.numeric(strsplit(
        gsub(".*seq\\(([^)]*)\\).*", "\\1", seq.line[1]), "\\s*,\\s*")[[1]])
    built <- paste0("doxy.cov.", seq(seq.args[1], seq.args[2], seq.args[3]))

    ## Requested codes: the names of INTERVENTION.LABELS.
    run.lines <- readLines(runner, warn = FALSE)
    run.lines <- run.lines[!grepl("^\\s*#", run.lines)]
    requested <- unique(unlist(regmatches(
        run.lines, gregexpr("doxy\\.cov\\.[0-9]+", run.lines))))

    expect_equal(
        setdiff(requested, built), character(0),
        info = paste0("interventions requested by intervention_run.R but never ",
                      "created by intervention_definitions.R: ",
                      paste(setdiff(requested, built), collapse = ", "),
                      "\n  built:     ", paste(built, collapse = ", "),
                      "\n  requested: ", paste(requested, collapse = ", "))
    )
})

test_that("the intervention runner does not swallow failed runs", {
    ## `stop.for.errors = FALSE` plus no completeness check means a run that
    ## produced nothing exits 0. If that flag is deliberate, the runner must at
    ## least verify afterwards that every requested intervention produced a
    ## simset.
    runner <- file.path(SHIELD.DIR, "intervention/intervention_run.R")
    skip_if_not(file.exists(runner))
    src <- paste(readLines(runner, warn = FALSE), collapse = "\n")

    tolerant <- grepl("stop\\.for\\.errors\\s*=\\s*(F|FALSE)", src)
    has.check <- grepl("stopifnot|completeness|all\\(.*%in%|verify", src)

    expect_true(
        !tolerant || has.check,
        info = paste("intervention_run.R sets stop.for.errors = FALSE and never",
                     "checks that the requested interventions actually ran.")
    )
})

test_that("the sampling blocks refer only to parameters in the full prior", {
    ## A block naming a parameter that is not in the prior silently drops that
    ## parameter from the sampler.
    skip_unless_stage("has.shield.helpers")

    prior.names <- SHIELD.FULL.PARAMETERS.PRIOR@var.names
    block.names <- unique(unlist(SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS))

    expect_equal(
        setdiff(block.names, prior.names), character(0),
        info = "sampling blocks name parameters absent from SHIELD.FULL.PARAMETERS.PRIOR"
    )
})

test_that("every prior parameter is proposed by at least one sampling block", {
    ## A parameter in the prior but in no block is never proposed: it stays at
    ## its initial value for the whole chain while still being reported as
    ## "calibrated".
    ##
    ## Note that blocks are allowed to OVERLAP, and here they deliberately do -
    ## the aging multipliers are grouped in sliding windows of adjacent age
    ## brackets (aging.black.group1 covers ages 14 and 19, group2 covers 19 and
    ## 24, and so on), so each interior bracket appears in two blocks. That is a
    ## standard blocked-Metropolis design, not a duplication bug.
    skip_unless_stage("has.shield.helpers")

    prior.names <- SHIELD.FULL.PARAMETERS.PRIOR@var.names
    block.names <- unlist(SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS)

    expect_equal(
        setdiff(prior.names, block.names), character(0),
        info = "prior parameters that no sampling block ever proposes"
    )
})

test_that("no sampling block is empty or degenerate", {
    skip_unless_stage("has.shield.helpers")

    blocks <- SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS

    expect_gt(length(blocks), 0)
    empty <- names(blocks)[lengths(blocks) == 0]
    expect_equal(empty, character(0),
                 info = paste("empty sampling blocks:", paste(empty, collapse = ", ")))

    ## a block that names the same parameter twice proposes a degenerate move
    repeated <- names(blocks)[vapply(blocks, function(b) anyDuplicated(b) > 0, logical(1))]
    expect_equal(repeated, character(0),
                 info = paste("blocks naming the same parameter twice:",
                              paste(repeated, collapse = ", ")))
})
