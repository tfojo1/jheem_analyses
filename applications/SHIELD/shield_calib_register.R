cat("*** Registering SHIELD calibration ***\n")
#
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
#
# this lets us tune the transmission multipliers for MSM and het in each timepoint together 
# par.aliases.transmission = list(
#     trate.0 = c("transmission.rate.multiplier.msm1970","transmission.rate.multiplier.heterosexual1970"),
#     trate.1 = c("transmission.rate.multiplier.msm1990","transmission.rate.multiplier.heterosexual1990"),
#     trate.2 = c("transmission.rate.multiplier.msm1995","transmission.rate.multiplier.heterosexual1995"),
#     trate.3 = c("transmission.rate.multiplier.msm2000", "transmission.rate.multiplier.heterosexual2000"),
#     trate.4 = c("transmission.rate.multiplier.msm2010", "transmission.rate.multiplier.heterosexual2010"),
#     trate.5 = c("transmission.rate.multiplier.msm2017", "transmission.rate.multiplier.heterosexual2017")
# )
par.aliases.transmission = list(
    trate.0 = c("transmission.rate.multiplier.msm1970","transmission.rate.multiplier.heterosexual1970"),
    trate.1 = c("transmission.rate.multiplier.msm1990","transmission.rate.multiplier.heterosexual1990"),
    trate.2 = c("transmission.rate.multiplier.msm1995","transmission.rate.multiplier.heterosexual1995"),
    trate.3 = c("transmission.rate.multiplier.msm2000", "transmission.rate.multiplier.heterosexual2000"),
    trate.4 = c("transmission.rate.multiplier.msm2010", "transmission.rate.multiplier.heterosexual2010"),
    trate.5 = c("transmission.rate.multiplier.msm2022", "transmission.rate.multiplier.heterosexual2022")
)


register.calibration.info("calib.v2.11.25.s0",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate.msm"=1.6,
                                                             "global.transmission.rate.het"=1.6),
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate.msm",
                                              "global.transmission.rate.het",
                                              "ratio.of.undiagnosed.to.diagnosed.1970",
                                              "prp.infections.among.msm.1970"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info('calib.v2.11.25.s1',
                          preceding.calibration.codes = 'calib.v2.11.25.s0',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# -----
# PAPER1 VERSION1 
#10.15 - V1
# Using the new manager
# fixing historical penalty likelihood issue
# fixing the seed infections in 1970: they were created by rate for msm, and were too small
#  register.calibration.info("calib.10.15.stage0",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=1.6,
#                                                              "global.transmission.rate.het"=1.6),
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het",
#                                               "ratio.of.undiagnosed.to.diagnosed.1970",
#                                               "prp.infections.among.msm.1970"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# register.calibration.info('calib.10.15.stage1',
#                           preceding.calibration.codes = 'calib.10.15.stage0',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.10.15.stage2",
#                           preceding.calibration.codes = 'calib.10.15.stage1',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               SHIELD.FULL.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.10.15.stage3",
#                           preceding.calibration.codes = 'calib.10.15.stage2',
#                           likelihood.instructions = lik.inst.stage3,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               SHIELD.FULL.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 20000, thin = 50, is.preliminary = F, 
#                           n.burn = 0, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# # changed the from.to:1970 for statified diagnosis data+ autoregressive=1
# register.calibration.info('calib.10.19.stage1',
#                           preceding.calibration.codes = 'calib.10.15.stage0',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# #changed a bunch of things:
# # equlize.weight.by.year=F for startified diagnosis to fit them better in recent years
# # onthology mapping fix for MSM: likelihood couldnt see the male diagnosis data from cdc.pdf.reports before this
# # lowring the transmission and screening priors for 15-17 year olds
# register.calibration.info('calib.10.20.stage1',
#                           preceding.calibration.codes = 'calib.10.15.stage0',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# register.calibration.info("calib.10.20.stage2",
#                           preceding.calibration.codes = 'calib.10.20.stage1',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               SHIELD.FULL.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.10.20.stage3",
#                           preceding.calibration.codes = 'calib.10.20.stage2',
#                           likelihood.instructions = lik.inst.stage3,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               SHIELD.FULL.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 20000, thin = 50, is.preliminary = F, 
#                           n.burn = 0, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.10.20.stage4",
#                           preceding.calibration.codes = 'calib.10.20.stage2',
#                           likelihood.instructions = lik.inst.stage3,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               SHIELD.FULL.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 250000, thin = 50, is.preliminary = F, 
#                           n.burn = 0, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
