# Tech2Check engine test.
#
# Confirms the spec sources, the engine builds, and a simulation is produced
# end-to-end at the primary location (Baltimore, C.12580). Exits 0 on success,
# 1 on test failure.
#
# Each check_* function is shaped as a future testthat block. Promotion is
# mechanical: wrap the body in test_that("description", { expect_true(...) }).
#
# Usage (from jheem_analyses root):
#   Rscript applications/tech2check/tech2check_engine_test.R

source('applications/tech2check/tech2check_specification.R')

VERSION  <- 'tech2check'
LOCATION <- 'C.12580'  # Baltimore MSA
END.YEAR <- 2030


#-- Checks --#

check_sim_produced <- function(sim) {
    ok <- inherits(sim, "jheem.simulation.set")
    list(ok = ok,
         message = if (ok)
             sprintf("OK: sim produced (class=%s)",
                     paste(class(sim), collapse = "/"))
         else
             sprintf("FAIL: not a jheem.simulation.set (class: %s)",
                     paste(class(sim), collapse = "/")))
}


#-- Main flow --#

if (!interactive()) {
    suppressMessages(library(jheem2))

    # EHE's global.trate has an improper Loguniform(0, Inf) prior, so
    # get.medians() leaves an NA the engine rejects. 0.09 matches
    # jheem2/R/tests/ENGINE_test.R. Real scientific output needs a
    # calibrated value, not a patch.
    params <- suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
    params['global.trate'] <- 0.09

    sim <- tryCatch(
        create.jheem.engine(VERSION, LOCATION, end.year = END.YEAR)$run(params),
        error = function(e) e
    )
    if (inherits(sim, "error")) {
        cat(sprintf("BUILD ERROR: %s\n", conditionMessage(sim)))
        quit(status = 1)
    }

    result <- check_sim_produced(sim)
    cat(result$message, "\n", sep = "")
    quit(status = if (isTRUE(result$ok)) 0 else 1)
}
