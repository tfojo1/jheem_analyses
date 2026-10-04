# Opt-in calibration used to validate the container execution and checkpoint
# contract. It exercises the real SHIELD model and stage-0 likelihood but is
# too small to produce scientifically meaningful posterior samples.
#
# Loaded only by a recorded run with SHIELD_ENABLE_CONTAINER_SMOKE=true. Run it
# with SHIELD_CACHE_FREQUENCY=1 so the first chunk is a durable checkpoint.
register.calibration.info(
    "container.smoke.stage0",
    likelihood.instructions = lik.inst.stage0,
    data.manager = SURVEILLANCE.MANAGER,
    end.year = 2030,
    fixed.initial.parameter.values = c(
        "global.transmission.rate.msm" = 1.6,
        "global.transmission.rate.het" = 1.6
    ),
    parameter.names = c(
        "global.transmission.rate.msm",
        "global.transmission.rate.het"
    ),
    # Two chunks are the minimum that can prove interruption after one durable
    # checkpoint and continuation through a distinct resumed iteration.
    n.iter = 2,
    thin = 1,
    is.preliminary = TRUE,
    max.run.time.seconds = 30,
    description = "Container checkpoint/resume canary; not for scientific inference"
)

# Starts from container.smoke.stage0's recorded outputs in the same output tree,
# as a later stage does from its preceding stage. It validates stage chaining
# and its lineage record, not stage-1 science, so it reuses the stage-0
# likelihood and parameters.
register.calibration.info(
    "container.smoke.stage1",
    preceding.calibration.codes = "container.smoke.stage0",
    likelihood.instructions = lik.inst.stage0,
    data.manager = SURVEILLANCE.MANAGER,
    end.year = 2030,
    parameter.names = c(
        "global.transmission.rate.msm",
        "global.transmission.rate.het"
    ),
    n.iter = 2,
    thin = 1,
    is.preliminary = TRUE,
    max.run.time.seconds = 30,
    description = "Container stage-chaining canary; not for scientific inference"
)

# Four two-iteration chunks let the replay check compare two fresh processes
# and a run resumed after both its first and second checkpoints. Uses the same
# model, likelihood, and sampling setup as stage0; only the test length differs.
register.calibration.info(
    "container.smoke.replay",
    likelihood.instructions = lik.inst.stage0,
    data.manager = SURVEILLANCE.MANAGER,
    end.year = 2030,
    fixed.initial.parameter.values = c(
        "global.transmission.rate.msm" = 1.6,
        "global.transmission.rate.het" = 1.6
    ),
    parameter.names = c(
        "global.transmission.rate.msm",
        "global.transmission.rate.het"
    ),
    n.iter = 8,
    thin = 1,
    is.preliminary = TRUE,
    max.run.time.seconds = 30,
    description = "Seed and checkpoint replay check; not for scientific inference"
)

# Exercise the current production registrations' actual stage-0 -> stage-1
# handoff, without editing them or maintaining a second scientific definition.
# Only test identity, predecessor identity, length/thinning, and description
# change. Two draws are an operational check, not a calibration result.
for (handoff.stage in c("stage0", "stage1")) {
    handoff.template <- shield.recorded.jheem2.function("get.calibration.info")(
        paste0("calib.10.1.", handoff.stage))
    if (!identical(as.integer(handoff.template$n.chains), 1L) ||
        !isTRUE(handoff.template$is.preliminary)) {
        stop("Stage-1 handoff fixture requires a single-chain preliminary registration")
    }
    handoff.expected.parent <- if (handoff.stage == "stage0") character()
                              else "calib.10.1.stage0"
    if (!identical(handoff.template$preceding.calibration.codes, handoff.expected.parent)) {
        stop("Stage-1 handoff fixture's production predecessor changed; review the fixture")
    }
    handoff.template$code <- paste0("container.actual.", handoff.stage)
    handoff.template$preceding.calibration.codes <- if (handoff.stage == "stage0") character()
                                                   else "container.actual.stage0"
    handoff.template$n.iter <- 2L
    handoff.template$n.burn <- 0L
    handoff.template$thin <- 1L
    handoff.template$description <- "Actual stage-1 handoff canary; not for scientific inference"
    do.call(register.calibration.info, handoff.template)
}
rm(handoff.stage, handoff.template, handoff.expected.parent)
