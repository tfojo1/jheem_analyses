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


# ***** STAGE1 *****  ----
# Attempting a full top-to-bottom calibration
register.calibration.info('calib.4.24.stage1.az',
                          preceding.calibration.codes = 'calib.4.24.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



# ***** STAGE2 ***** ----
# Attempting a new stage1 and 2 calibration by upweighting sex-stratified HIV testing data
register.calibration.info('calib.4.29.stage2.pk',
                          preceding.calibration.codes = 'calib.4.24.stage1.az',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# # Attempting a top-to-bottom calibration. Here, uses 8x on future penalty likelihood.
register.calibration.info('calib.4.24.stage2.az',
                          preceding.calibration.codes = 'calib.4.24.stage1.az',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


# ***** STAGE3 ***** ----

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