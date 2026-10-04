# Validate test calibration names against the installed engine's real path
# validator without constructing a model, manager, likelihood, or MCMC cache.
suppressPackageStartupMessages(library(jheem2))
validate <- utils::getFromNamespace("validate.calibration.code", "jheem2")
definitions <- list()
scope <- new.env(parent = baseenv())
scope$lik.inst.stage0 <- scope$SURVEILLANCE.MANAGER <- NULL
templates <- lapply(c("stage0", "stage1"), function(stage) list(
    code = paste0("calib.10.1.", stage), likelihood.instructions = stage,
    data.manager = "same manager", parameter.names = c("rate.a", "rate.b"),
    parameter.aliases = list(alias = c("a", "b")),
    solver.metadata = list(method = "unchanged"), end.year = 2030,
    fixed.initial.parameter.values = c(rate.a = 1.6),
    preceding.calibration.codes = if (stage == "stage0") character() else "calib.10.1.stage0",
    n.iter = 20000, thin = 80, n.burn = 0, n.chains = 1,
    is.preliminary = TRUE, max.run.time.seconds = 30, description = "production"))
names(templates) <- c("calib.10.1.stage0", "calib.10.1.stage1")
original.templates <- unserialize(serialize(templates, NULL))
scope$shield.recorded.jheem2.function <- function(name) {
    stopifnot(identical(name, "get.calibration.info"))
    function(code) templates[[code]]
}
scope$register.calibration.info <- function(code, ...) {
    validate(code, error.prefix = "Container test registration: ")
    definitions[[code]] <<- list(...)
}
sys.source("applications/SHIELD/shield_calib_register_container_smoke.R", envir = scope)
stopifnot(identical(names(definitions), c("container.smoke.stage0", "container.smoke.stage1",
                                         "container.smoke.replay", "container.actual.stage0",
                                         "container.actual.stage1")),
          definitions[["container.smoke.replay"]]$n.iter == 8,
          definitions[["container.smoke.replay"]]$thin == 1)
allowed.changes <- c("code", "preceding.calibration.codes", "n.iter", "n.burn", "thin", "description")
for (stage in c("stage0", "stage1")) {
    original <- templates[[paste0("calib.10.1.", stage)]]
    actual <- definitions[[paste0("container.actual.", stage)]]
    unchanged <- setdiff(names(original), allowed.changes)
    stopifnot(identical(actual[unchanged], original[unchanged]),
              identical(actual$n.iter, 2L), identical(actual$thin, 1L),
              identical(actual$n.burn, 0L),
              identical(actual$preceding.calibration.codes,
                        if (stage == "stage0") character() else "container.actual.stage0"))
}
stopifnot(identical(templates, original.templates))
templates[["calib.10.1.stage1"]]$n.chains <- 4L
stopifnot(inherits(try(sys.source("applications/SHIELD/shield_calib_register_container_smoke.R",
                               envir = scope), silent = TRUE), "try-error"))
templates <- original.templates
templates[["calib.10.1.stage1"]]$preceding.calibration.codes <- "different.stage0"
stopifnot(inherits(try(sys.source("applications/SHIELD/shield_calib_register_container_smoke.R",
                               envir = scope), silent = TRUE), "try-error"))
cat("Container test registrations passed the engine path validator\n")
cat("Actual handoff fixtures preserve scientific fields and reject registration drift\n")
