## =============================================================================
## shield_test_bootstrap.R
## -----------------------------------------------------------------------------
## Loads a SHIELD environment for the test suite.
##
## This deliberately does NOT reuse shield_source_code.R, because that file
##   * runs `git pull` on jheem_analyses and ../jheem2 (interactive sessions),
##   * stops if ../jheem2 is not on the dev branch, and
##   * downloads the surveillance manager over the network.
## None of that is acceptable inside a test run: a test must not mutate the
## developer's working tree, and it must be reproducible offline.
##
## Instead this file performs the same *loading* steps with the side effects
## removed. test-static-bootstrap-drift.R asserts that the set of files sourced
## here still matches the set sourced by shield_source_code.R, so this file
## cannot silently fall out of date.
##
## Everything here is tier-aware: it never stops. Each stage records whether it
## succeeded in the SHIELD.TEST.ENV list, and tests skip on what is missing.
## =============================================================================

SHIELD.TEST.ENV <- new.env(parent = emptyenv())

## --- Locate the repositories --------------------------------------------------
## The SHIELD code is full of literal "../jheem_analyses/..." paths, so the
## working directory must be a directory whose parent contains jheem_analyses.
## The repo root itself satisfies that, so we always run from there.

## shield.test.find.repo.root ----
shield.test.find.repo.root <- function(start = getwd()) {
    d <- normalizePath(start, mustWork = FALSE)
    while (TRUE) {
        if (dir.exists(file.path(d, "applications", "SHIELD")) &&
            dir.exists(file.path(d, "commoncode"))) {
            return(d)
        }
        parent <- dirname(d)
        if (identical(parent, d)) {
            stop("Could not locate the jheem_analyses repo root above ", start)
        }
        d <- parent
    }
}

SHIELD.TEST.ENV$repo.root  <- shield.test.find.repo.root()
SHIELD.TEST.ENV$shield.dir <- file.path(SHIELD.TEST.ENV$repo.root, "applications", "SHIELD")
SHIELD.TEST.ENV$jheem2.dir <- file.path(dirname(SHIELD.TEST.ENV$repo.root), "jheem2")

setwd(SHIELD.TEST.ENV$repo.root)

## The literal relative paths only resolve if the repo directory is actually
## named "jheem_analyses". Say so loudly rather than failing 40 files later.
if (!dir.exists("../jheem_analyses")) {
    stop("SHIELD code uses hardcoded '../jheem_analyses/' paths, but that does not ",
         "resolve from ", SHIELD.TEST.ENV$repo.root, ". The checkout directory ",
         "must be named 'jheem_analyses'.")
}

## --- Stage helper -------------------------------------------------------------
## Run a stage, remember whether it worked, never abort the suite.

## shield.test.stage ----
shield.test.stage <- function(name, expr, quiet = TRUE) {
    ## Warnings are recorded, never treated as failures. They are muffled with
    ## withCallingHandlers, which resumes the stage, rather than tryCatch, which
    ## would unwind it half-done.
    warnings.seen <- character()
    result <- tryCatch(
        withCallingHandlers({
            if (quiet) {
                invisible(utils::capture.output(force(expr), type = "output"))
            } else {
                force(expr)
            }
            list(ok = TRUE, message = NA_character_)
        }, warning = function(w) {
            warnings.seen <<- c(warnings.seen, conditionMessage(w))
            invokeRestart("muffleWarning")
        }),
        error = function(e) list(ok = FALSE, message = conditionMessage(e))
    )
    SHIELD.TEST.ENV[[paste0(name, ".warnings")]] <- warnings.seen
    SHIELD.TEST.ENV[[name]] <- result$ok
    SHIELD.TEST.ENV[[paste0(name, ".message")]] <- result$message
    if (!result$ok) {
        message("[bootstrap] stage '", name, "' unavailable: ", result$message)
    }
    invisible(result$ok)
}

## =============================================================================
## Tier 1: packages
## =============================================================================

shield.test.stage("has.packages", {
    suppressMessages({
        library(locations)
        library(distributions)
    })
})

## =============================================================================
## Tier 2: jheem2
## -----------------------------------------------------------------------------
## Prefer the installed package. Fall back to the local clone only if the
## package is absent - and source it as-is, without touching git.
## =============================================================================

## Honour the repo's own switch, exactly as shield_source_code.R does. This
## matters: the two sources are NOT interchangeable. The installed jheem2
## 1.12.0 has create.custom.likelihood.instructions(name, compute.function,
## get.data.function, verbose) while the dev clone adds `weights`, and
## shield_likelihoods.R passes `weights`. Testing against the package would fail
## to source the likelihoods at all, for a reason that has nothing to do with
## SHIELD.
shield.test.stage("has.jheem2", {
    use.package <- tryCatch({
        source("use_jheem2_package_setting.R", local = TRUE)
        isTRUE(USE.JHEEM2.PACKAGE)
    }, error = function(e) NA)

    clone.entry <- file.path(SHIELD.TEST.ENV$jheem2.dir,
                             "R/tests/source_jheem2_package.R")
    have.clone <- file.exists(clone.entry)
    have.package <- requireNamespace("jheem2", quietly = TRUE)

    ## The repo setting wins when it can be satisfied; otherwise take whatever
    ## is available and record which, so a failure can be attributed.
    use.clone <- if (isTRUE(use.package)) !have.package else have.clone

    ## Sourcing the clone compiles its C++ with Rcpp::sourceCpp(), which needs a
    ## working toolchain. If that fails, fall back to the installed package and
    ## record why - test-integration-jheem2-api.R then reports the consequence
    ## (some SHIELD code calls arguments only the dev clone has) instead of
    ## letting it surface as an unrelated error.
    clone.ok <- FALSE
    if (use.clone && have.clone) {
        ## A failed clone source leaves a half-populated global environment
        ## behind - enough definitions to shadow the package but not enough to
        ## work. Snapshot the globals first so the failure can be undone.
        before <- ls(globalenv(), all.names = TRUE)
        clone.ok <- tryCatch({
            source(clone.entry)
            TRUE
        }, error = function(e) {
            SHIELD.TEST.ENV$jheem2.clone.error <- conditionMessage(e)
            FALSE
        })
        if (!clone.ok) {
            added <- setdiff(ls(globalenv(), all.names = TRUE), before)
            if (length(added)) rm(list = added, envir = globalenv())
        }
    }

    if (clone.ok) {
        SHIELD.TEST.ENV$jheem2.source <- "clone"
        SHIELD.TEST.ENV$jheem2.version <- tryCatch(
            system2("git", c("-C", shQuote(SHIELD.TEST.ENV$jheem2.dir),
                             "rev-parse", "--short", "HEAD"),
                    stdout = TRUE, stderr = FALSE)[1],
            error = function(e) NA_character_)
    } else if (have.package) {
        suppressMessages(library(jheem2))
        SHIELD.TEST.ENV$jheem2.source <- "package"
        SHIELD.TEST.ENV$jheem2.version <- as.character(utils::packageVersion("jheem2"))
    } else {
        stop("jheem2 is neither installed nor present as a clone at ",
             SHIELD.TEST.ENV$jheem2.dir)
    }
    SHIELD.TEST.ENV$use.jheem2.package.setting <- use.package
})

## =============================================================================
## Tier 3: standalone SHIELD files
## -----------------------------------------------------------------------------
## These define functions only - no data manager, no cache, no network.
## The fast test tier runs against exactly these.
## =============================================================================

SHIELD.TEST.STANDALONE.FILES <- c(
    "R/shield_multivariate_spline_prior.R",
    "R/shield_inputManager_pairing.R",
    "R/shield_inputManager_helpers.R",
    "R/shield_ontology_mappings.R",
    "intervention/doxy_effectiveness.R",
    "shield_base_parameters.R"
)

shield.test.stage("has.standalone", {
    for (f in SHIELD.TEST.STANDALONE.FILES) {
        source(file.path(SHIELD.TEST.ENV$shield.dir, f))
    }
})

## =============================================================================
## Tier 4: commoncode + cached data managers
## -----------------------------------------------------------------------------
## Offline only. If the managers are not in the local cache, the integration
## tiers skip rather than reaching over the network mid-test.
## =============================================================================

shield.test.stage("has.commoncode", {
    source("commoncode/cache_manager.R")
    clear.all.managers()
    source("commoncode/target_populations.R")
    source("commoncode/age_mappings.R")
    source("commoncode/cache_object_for_version_functions.R")
    source("commoncode/logitnorm_helpers.R")
    source("commoncode/file_paths.R")
    source("commoncode/locations_of_interest.R")

    ## file_paths.R leaves ROOT.DIR as a RELATIVE path ("../../files") on a
    ## laptop, so everything the jheem root points at moves whenever the working
    ## directory does - and testthat runs with the test directory as its working
    ## directory. Resolve it once, against the repo root, and fall back to a
    ## session-scoped temp directory when the real archive is not present, so a
    ## test run never writes simulation output somewhere unexpected.
    resolved.root <- if (grepl("^(/|[A-Za-z]:)", ROOT.DIR)) {
        ROOT.DIR
    } else {
        file.path(SHIELD.TEST.ENV$repo.root, ROOT.DIR)
    }
    if (!dir.exists(resolved.root)) {
        resolved.root <- file.path(tempdir(), "shield-test-jheem-root")
        dir.create(resolved.root, recursive = TRUE, showWarnings = FALSE)
        SHIELD.TEST.ENV$root.dir.is.temporary <- TRUE
    }
    resolved.root <- normalizePath(resolved.root, mustWork = TRUE)
    assign("ROOT.DIR", resolved.root, envir = globalenv())
    SHIELD.TEST.ENV$root.dir <- resolved.root

    set.jheem.root.directory(resolved.root)
})

shield.test.stage("has.mobility", {
    load(file.path(JHEEM.CACHE.DIR, "google_mobility_data.Rdata"), envir = globalenv())
})

shield.test.stage("has.census.manager", {
    if (!exists("CENSUS.MANAGER", envir = globalenv())) {
        assign("CENSUS.MANAGER",
               load.data.manager.from.cache("census.manager.rdata",
                                            set.as.default = FALSE,
                                            offline = TRUE),
               envir = globalenv())
    }
})

## Which syphilis manager the tests run against.
##
## Production (shield_source_code.R) leaves SYPHILIS.MANAGER.RELEASE.TAG NULL,
## which resolves to whatever release is currently promoted. That is the right
## behaviour for a real run and the wrong behaviour for a test: the suite would
## silently change what it is testing whenever a manager is promoted, and an
## offline resolution falls back to the untagged file in cached/, which is a
## *different* file from the tagged copies under cached/data-managers/.
##
## So the tests pin a tag by default and let CI override it. Running the suite
## against a candidate manager before promoting it is the point of
## test-integration-manager-contract.R.
SHIELD.TEST.MANAGER.TAG <- local({
    from.env <- trimws(Sys.getenv("JHEEM_SYPHILIS_MANAGER_TAG"))
    if (nzchar(from.env)) {
        if (identical(from.env, "latest")) NULL else from.env
    } else {
        "syphilis-manager-v2026.07.27"
    }
})

shield.test.stage("has.surveillance.manager", {
    if (!exists("SURVEILLANCE.MANAGER", envir = globalenv())) {
        assign("SURVEILLANCE.MANAGER",
               load.data.manager.from.cache("syphilis.manager.rdata",
                                            set.as.default = TRUE,
                                            offline = TRUE,
                                            release.tag = SHIELD.TEST.MANAGER.TAG),
               envir = globalenv())
    }
    SHIELD.TEST.ENV$manager.tag <- SHIELD.TEST.MANAGER.TAG
})

## =============================================================================
## Tier 5: manager-dependent SHIELD helpers + global constants
## -----------------------------------------------------------------------------
## Mirrors sections 7-8 of shield_source_code.R.
## =============================================================================

SHIELD.TEST.MANAGER.DEPENDENT.FILES <- c(
    "shield_calib_parameters.R",
    "shield_base_parameters.R",
    "R/shield_ontology_mappings.R",
    "R/shield_specification_helpers.R",
    "R/shield_inputManager_pairing.R",
    "R/shield_inputManager_helpers.R",
    "R/shield_inputManager_covid.R"
)

shield.test.stage("has.shield.helpers", {
    if (!isTRUE(SHIELD.TEST.ENV$has.surveillance.manager)) {
        stop("surveillance manager not loaded")
    }
    for (f in SHIELD.TEST.MANAGER.DEPENDENT.FILES) {
        source(file.path(SHIELD.TEST.ENV$shield.dir, f))
    }
    assign("PAIRING.INPUT.MANAGER",
           create.pairing.manager(dir = file.path(SHIELD.TEST.ENV$shield.dir,
                                                  "data_files/pairing")),
           envir = globalenv())

    ## Global constants (section 8 of shield_source_code.R)
    assign("CENSUS.AGES", as.character(sort(
        parse.age.strata.names(CENSUS.MANAGER$ontologies$census$age)$lower)),
        envir = globalenv())

    assign("DEFAULT.START.YEAR",            1970, envir = globalenv())
    assign("DEFAULT.FIX.STRATA.YEAR",       2010, envir = globalenv())
    assign("DEFAULT.POPULATION.YEARS",      2010, envir = globalenv())
    assign("DEFAULT.AGING.START.YEAR",      2005, envir = globalenv())
    assign("DEFAULT.MIGRATION.START.YEAR",  2005, envir = globalenv())
    assign("DEFAULT.MORTALITY.RATE.YEARS",  c("2001-2010", "2011-2020"), envir = globalenv())
    assign("DEFAULT.FERTILITY.START.YEARS", 2005, envir = globalenv())
    assign("DEFAULT.FERTILITY.RATE.YEARS",  2007:2023, envir = globalenv())
    assign("FERTILE.AGES", c("15-19 years", "20-24 years", "25-29 years",
                             "30-34 years", "35-39 years", "40-44 years"),
           envir = globalenv())
    assign("NON.FERTILE.AGES", c("0-14 years", "45-49 years", "50-54 years",
                                 "55-64 years", "65+ years"), envir = globalenv())
    assign("SEXUAL.ACTIVITY.AGES", c("15-19 years", "20-24 years", "25-29 years",
                                     "30-34 years", "35-39 years", "40-44 years",
                                     "45-49 years", "50-54 years", "55-64 years"),
           envir = globalenv())
    assign("DEFAULT.STI.SCREENING.START.YEAR", 1980, envir = globalenv())
    assign("DEFAULT.PRENATAL.CARE.START.YEAR", 1980, envir = globalenv())
    assign("DEFAULT.TRANSMISSION.START.YEAR",  1980, envir = globalenv())
    assign("DEFAULT.HIV.TESTING.START.YEAR",   2010, envir = globalenv())

    ## Read by get.sexual.availability() and by get.age.mixing.proportions(),
    ## which floors every partner-age bracket at the debut age. Without it the
    ## specification does not build and the whole integration tier skips.
    assign("DEFAULT.SEXUAL.DEBUT.AGE",         13,   envir = globalenv())
})

## =============================================================================
## Tier 6: the specification itself
## -----------------------------------------------------------------------------
## Built lazily, on first request, because it is the expensive step.
## =============================================================================

SHIELD.TEST.ENV$specification.attempted <- FALSE

## shield.test.source.without.env.loader ----
## shield_specification.R line 11 sources shield_source_code.R, which is the
## file that runs git. The bootstrap has already supplied everything that file
## provides, so we source the specification with that one line removed.
## test-static-bootstrap-drift.R checks that this is still the only source() of
## an environment-loading file in shield_specification.R.
shield.test.source.without.env.loader <- function(path) {
    lines <- readLines(path, warn = FALSE)
    drop <- grepl("^\\s*source\\(.*shield_source_code\\.R", lines)
    if (!any(drop)) {
        warning("shield_specification.R no longer sources shield_source_code.R; ",
                "the bootstrap's skip logic may be stale")
    }
    lines[drop] <- paste0("## [test bootstrap removed] ", lines[drop])
    eval(parse(text = paste(lines, collapse = "\n")), envir = globalenv())
}

## shield.test.specification ----
shield.test.specification <- function() {
    ## The specification and everything it calls resolve files through the
    ## literal prefix "../jheem_analyses/", so they only work from the repo
    ## root. Guarantee that here rather than relying on every caller.
    old.wd <- setwd(SHIELD.TEST.ENV$repo.root)
    on.exit(setwd(old.wd), add = TRUE)

    if (!SHIELD.TEST.ENV$specification.attempted) {
        SHIELD.TEST.ENV$specification.attempted <- TRUE
        shield.test.stage("has.specification", {
            if (!isTRUE(SHIELD.TEST.ENV$has.shield.helpers)) {
                stop("SHIELD helpers not loaded")
            }
            shield.test.source.without.env.loader(
                file.path(SHIELD.TEST.ENV$shield.dir, "shield_specification.R"))
        })
    }
    if (isTRUE(SHIELD.TEST.ENV$has.specification)) {
        get("SHIELD.SPECIFICATION", envir = globalenv())
    } else {
        NULL
    }
}

## =============================================================================
## Tier 7: the likelihoods
## -----------------------------------------------------------------------------
## Also lazy: sourcing shield_likelihoods.R fits error terms and reads the
## national series, so it is not free.
## =============================================================================

SHIELD.TEST.ENV$likelihoods.attempted <- FALSE

## shield.test.likelihoods ----
shield.test.likelihoods <- function() {
    old.wd <- setwd(SHIELD.TEST.ENV$repo.root)
    on.exit(setwd(old.wd), add = TRUE)

    if (!SHIELD.TEST.ENV$likelihoods.attempted) {
        SHIELD.TEST.ENV$likelihoods.attempted <- TRUE
        shield.test.stage("has.likelihoods", {
            if (is.null(shield.test.specification()))
                stop("the specification is not available")
            source(file.path(SHIELD.TEST.ENV$shield.dir, "shield_likelihoods.R"))
        })
    }
    isTRUE(SHIELD.TEST.ENV$has.likelihoods)
}

## =============================================================================
## Reporting
## =============================================================================

## shield.test.tier.summary ----
shield.test.tier.summary <- function() {
    stages <- c("has.packages", "has.jheem2", "has.standalone", "has.commoncode",
                "has.mobility", "has.census.manager", "has.surveillance.manager",
                "has.shield.helpers")
    data.frame(
        stage     = stages,
        available = vapply(stages, function(s) isTRUE(SHIELD.TEST.ENV[[s]]), logical(1)),
        row.names = NULL
    )
}
