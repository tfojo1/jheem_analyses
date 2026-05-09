cat("*** Registering SHIELD calibration ***\n")
#
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
N_ITER=15000
#
# this lets us tune the transmission multipliers for MSM and het in each timepoint together 
par.aliases.transmission = list(
    trate.0 = c("transmission.rate.multiplier.msm1970","transmission.rate.multiplier.heterosexual1970"),
    trate.1 = c("transmission.rate.multiplier.msm1990","transmission.rate.multiplier.heterosexual1990"),
    trate.2 = c("transmission.rate.multiplier.msm1995","transmission.rate.multiplier.heterosexual1995"),
    trate.3 = c("transmission.rate.multiplier.msm2000", "transmission.rate.multiplier.heterosexual2000"),
    trate.4 = c("transmission.rate.multiplier.msm2010", "transmission.rate.multiplier.heterosexual2010"),
    trate.5 = c("transmission.rate.multiplier.msm2017", "transmission.rate.multiplier.heterosexual2017")
)

# ***** STAGE0 ***** ----
# MAKE SURE FOR NEW STAGE 0 TO USE 15000 AND TO KEEP IT CONSISTENT ACROSS STAGES

# Attempting a full top-to-bottom calibration
register.calibration.info("calib.4.24.stage0.az",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Attempting a calibration after increasing msm.fraction.pairing.female to 30%
register.calibration.info("calib.5.7.stage0.pk",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# *** OLD (has to be kept here because the registration has to happen in order)----
# Attempting a full top-to-bottom calibration
register.calibration.info('calib.4.24.stage1.az',
                          preceding.calibration.codes = 'calib.4.24.stage0.az',
                          likelihood.instructions = lik.inst.stage1, # likelihood has different meaning now
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


# ***** NEW STAGE1 ***** ----


register.calibration.info('calib.5.7.stage1.test',
                          preceding.calibration.codes = 'calib.4.24.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.5.4.stage1.az',
                          preceding.calibration.codes = 'calib.4.24.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Attempting a calibration after increasing msm.fraction.pairing.female to 30%
register.calibration.info('calib.5.7.stage1.pk',
                          preceding.calibration.codes = 'calib.5.7.stage0.pk',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# *** OLD (has to be kept here because the registration has to happen in order)----
# # Attempting a top-to-bottom calibration. Here, uses 8x on future penalty likelihood.
register.calibration.info('calib.4.24.stage2.az',
                          preceding.calibration.codes = 'calib.4.24.stage1.az',
                          likelihood.instructions = lik.inst.stage1, # likelihood replaced so that this code still runs. Was stage 2
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# ***** STAGE2 (formerly STAGE3) ***** ----
# overall 1/2, pop 1/32
register.calibration.info("calib.5.8.stg2.2.p32",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.32x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# overall 1/2, pop 1/2
register.calibration.info("calib.5.6.stage3.2.p2",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.2x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/2, pop 1/4
register.calibration.info("calib.5.6.stage3.2.p4",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.4x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/2, pop 1/8
register.calibration.info("calib.5.6.stage3.2.p8",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.8x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/2, pop 1/16
register.calibration.info("calib.5.6.stage3.2.p16",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.16x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/2, pop 1/32
register.calibration.info("calib.5.6.stage3.2.p32",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.32x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/4, pop 1/2
register.calibration.info("calib.5.6.stage3.4.p2",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.4x.pop.2x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/4, pop 1/4
register.calibration.info("calib.5.6.stage3.4.p4",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.4x.pop.4x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/4, pop 1/8
register.calibration.info("calib.5.6.stage3.4.p8",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.4x.pop.8x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/4, pop 1/16
register.calibration.info("calib.5.6.stage3.4.p16",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.4x.pop.16x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/4, pop 1/32
register.calibration.info("calib.5.6.stage3.4.p32",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.2x.pop.32x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/8, pop 1/2
register.calibration.info("calib.5.6.stage3.8.p2",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.8x.pop.2x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# overall 1/8, pop 1/4
register.calibration.info("calib.5.6.stage3.8.p4",
                          preceding.calibration.codes = 'calib.5.4.stage1.az',
                          likelihood.instructions = lik.inst.stage3.8x.pop.4x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# STAGE 3 (formerly would have been STAGE 4) ----
# overall 1/2, pop 1/32
register.calibration.info("calib.5.8.stg3.2.p32",
                          preceding.calibration.codes = 'calib.5.8.stg2.2.p32',
                          likelihood.instructions = lik.inst.stage3.2x.pop.32x,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = F, n.chains=4, max.run.time.seconds = 30, description = "NA"
)

# register.calibration.info("calib.5.1.stage3.p2",
#                           preceding.calibration.codes = 'calib.4.24.stage2.az',
#                           likelihood.instructions = lik.inst.stage3.pop.2x,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names,
#                               "global.transmission.rate"
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.5.1.stage3.p4",
#                           preceding.calibration.codes = 'calib.4.24.stage2.az',
#                           likelihood.instructions = lik.inst.stage3.pop.4x,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names,
#                               "global.transmission.rate"
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.5.1.stage3.p8",
#                           preceding.calibration.codes = 'calib.4.24.stage2.az',
#                           likelihood.instructions = lik.inst.stage3.pop.8x,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names,
#                               "global.transmission.rate"
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )

# All outcomes and likelihoods, and sex-specific prop. tested, with 1/8th overall weight
register.calibration.info("calib.4.30.stage3.8th",
                          preceding.calibration.codes = 'calib.4.24.stage2.az',
                          likelihood.instructions = lik.inst.stage3,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2035,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names,
                              "global.transmission.rate"
                              ),
                          n.iter = 10000, thin = 50,n.chains = 1, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
)

# # Attempting a top-to-bottom calibration. Here will use weight 1. Hope to end with 100 simulations.
# register.calibration.info('calib.4.24.stage3.az',
#                           preceding.calibration.codes = 'calib.4.24.stage2.az',
#                           likelihood.instructions = lik.inst.stage3,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2035,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                                               POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names),
#                           n.iter = 10000, thin = 50, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
# )



