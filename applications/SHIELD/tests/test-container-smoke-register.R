# Validate test calibration names against the installed engine's real path
# validator without constructing a model, manager, likelihood, or MCMC cache.
suppressPackageStartupMessages(library(jheem2))
validate <- utils::getFromNamespace("validate.calibration.code", "jheem2")
definitions <- list()
scope <- new.env(parent = baseenv())
scope$lik.inst.stage0 <- scope$SURVEILLANCE.MANAGER <- NULL
scope$register.calibration.info <- function(code, ...) {
    validate(code, error.prefix = "Container test registration: ")
    definitions[[code]] <<- list(...)
}
sys.source("applications/SHIELD/shield_calib_register_container_smoke.R", envir = scope)
stopifnot(identical(names(definitions), c("container.smoke.stage0", "container.smoke.stage1",
                                         "container.smoke.replay")),
          definitions[["container.smoke.replay"]]$n.iter == 8,
          definitions[["container.smoke.replay"]]$thin == 1)
cat("Container test registrations passed the engine path validator\n")
