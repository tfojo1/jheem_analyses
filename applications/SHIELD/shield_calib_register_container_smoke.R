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
