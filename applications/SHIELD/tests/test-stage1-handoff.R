source("applications/SHIELD/R/shield_handoff_checks.R")
previous <- c(rate.a = 1.6, rate.b = 1.4)
stopifnot(shield.handoff.require.transfer(previous, rev(previous)) == 2L)
bad <- list(c(rate.a = 1.6), c(rate.a = 1.6, rate.b = Inf),
            c(rate.a = 1.6, rate.a = 1.4), unname(previous),
            c(rate.a = 1.6, rate.b = 1.4000000000000004))
for (values in bad) {
    stopifnot(inherits(try(shield.handoff.require.transfer(previous, values),
                           silent = TRUE), "try-error"))
}
cat("Stage-1 predecessor transfer checks passed\n")
