# Opt-in calibration used to validate the container execution and checkpoint
# contract. It deliberately exercises the real SHIELD model and likelihood but
# is too small to produce scientifically meaningful posterior samples.
register.calibration.info(
    "container.smoke.stage0",
    likelihood.instructions = lik.inst.stage0.2021,
    data.manager = SURVEILLANCE.MANAGER,
    end.year = 2030,
    fixed.initial.parameter.values = c(
        "global.transmission.rate.msm" = 2.3,
        "global.transmission.rate.het" = 2.3
    ),
    parameter.names = c(
        "global.transmission.rate.msm",
        "global.transmission.rate.het"
    ),
    n.iter = 6,
    n.burn = 0,
    thin = 1,
    n.chains = 1,
    is.preliminary = TRUE,
    max.run.time.seconds = 30,
    description = "Container checkpoint/resume canary; not for scientific inference"
)
