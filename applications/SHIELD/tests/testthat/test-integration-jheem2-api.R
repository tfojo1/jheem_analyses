## ==========================================================================
## INTEGRATION TIER  |  the jheem2 <-> SHIELD API contract
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the jheem2 actually loaded provides every function SHIELD calls, with
##   every argument SHIELD passes, and is the source USE.JHEEM2.PACKAGE asks for.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=jheem2-api
##
## WHY IT MATTERS
##   SHIELD is not a package and declares no dependency on a jheem2 version, yet
##   it calls jheem2 functions with arguments that only exist on some of them.
##
##   USE.JHEEM2.PACKAGE selects between the installed package and a local clone of
##   the `dev` branch, and those two are not interchangeable. Today, for example,
##   the installed jheem2 1.12.0 has
##       create.custom.likelihood.instructions(name, compute.function,
##                                             get.data.function, verbose)
##   while the dev clone adds `weights`, and shield_likelihoods.R passes
##   `weights = 1`. Against the package, sourcing the likelihoods dies with
##   "unused argument (weights = 1)" - a message that points at nothing useful.
##
##   These tests turn that into a named, actionable failure, and let the
##   likelihood tests skip with a reason instead of erroring.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.jheem2")

## jheem2 entry points SHIELD calls, and the arguments it passes to them.
REQUIRED.JHEEM2.API <- list(
    "create.jheem.specification"         = c("version", "start.year"),
    "create.jheem.engine"                = c("version", "location", "end.year"),
    "register.model.quantity"            = c("name", "value"),
    "create.basic.likelihood.instructions" = c("outcome.for.sim", "outcome.for.data",
                                               "dimensions", "levels.of.stratification",
                                               "from.year", "to.year",
                                               "error.variance.type",
                                               "error.variance.term"),
    "create.custom.likelihood.instructions" = c("name", "compute.function",
                                                "get.data.function", "weights"),
    "join.likelihood.instructions"       = character(0),
    "register.calibration.info"          = c("likelihood.instructions", "data.manager"),
    "create.intervention"                = c("parameters", "code"),
    "create.intervention.effect"         = c("quantity.name", "effect.values",
                                             "start.time", "times", "scale",
                                             "apply.effects.as"),
    "create.target.population"           = c("name"),
    "get.null.intervention"              = character(0),
    "create.simset.collection"           = c("version", "calibration.code",
                                             "locations", "interventions", "n.sim"),
    "get.specification.metadata"         = c("version", "location"),
    "get.simulation.metadata"            = c("version", "location")
)

test_that("every jheem2 function SHIELD calls exists", {
    missing <- names(REQUIRED.JHEEM2.API)[
        !vapply(names(REQUIRED.JHEEM2.API), exists, logical(1), mode = "function")]

    expect_equal(
        missing, character(0),
        info = paste0("jheem2 (", SHIELD.TEST.ENV$jheem2.source, " ",
                      SHIELD.TEST.ENV$jheem2.version,
                      ") is missing functions SHIELD calls: ",
                      paste(missing, collapse = ", "))
    )
})

test_that("every jheem2 function accepts the arguments SHIELD passes it", {
    ## This is the check that names the real problem. A missing argument here
    ## means some SHIELD file will fail to source, usually far from the cause.
    problems <- character(0)

    for (fn.name in names(REQUIRED.JHEEM2.API)) {
        if (!exists(fn.name, mode = "function")) next
        needed <- REQUIRED.JHEEM2.API[[fn.name]]
        if (!length(needed)) next

        have <- names(formals(get(fn.name, mode = "function")))
        if ("..." %in% have) next          # anything goes

        missing <- setdiff(needed, have)
        if (length(missing)) {
            problems <- c(problems, paste0(
                fn.name, "() does not accept: ", paste(missing, collapse = ", "),
                " (accepts: ", paste(have, collapse = ", "), ")"))
        }
    }

    expect_equal(
        problems, character(0),
        info = paste0(
            "the loaded jheem2 (", SHIELD.TEST.ENV$jheem2.source, " ",
            SHIELD.TEST.ENV$jheem2.version,
            ") does not match what SHIELD calls:\n  ",
            paste(problems, collapse = "\n  "),
            "\n\nUSE.JHEEM2.PACKAGE is ",
            SHIELD.TEST.ENV$use.jheem2.package.setting,
            ". If the clone was wanted but the package was used, the clone ",
            "failed to source: ",
            SHIELD.TEST.ENV$jheem2.clone.error %||% "(no error recorded)")
    )
})

test_that("the jheem2 actually loaded is the one the repo asks for", {
    ## use_jheem2_package_setting.R sets USE.JHEEM2.PACKAGE. If it says FALSE
    ## and we ended up on the package anyway, every downstream result came from
    ## a different jheem2 than production runs.
    setting <- SHIELD.TEST.ENV$use.jheem2.package.setting
    skip_if(is.na(setting), "could not read use_jheem2_package_setting.R")

    wanted <- if (isTRUE(setting)) "package" else "clone"
    expect_equal(
        SHIELD.TEST.ENV$jheem2.source, wanted,
        info = paste0(
            "USE.JHEEM2.PACKAGE is ", setting, ", so the model expects the ",
            wanted, ", but the tests loaded the ",
            SHIELD.TEST.ENV$jheem2.source, ". Reason: ",
            SHIELD.TEST.ENV$jheem2.clone.error %||% "(none recorded)",
            "\nResults from this run reflect a different jheem2 than a real run.")
    )
})
