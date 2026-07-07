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

# 6.29 (removed contact tracing; changed global trates to 3.1; added prop male diag among msm nested prop likelihood)
register.calibration.info("calib.6.29.stage0.az",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.1,
                                                             "global.transmission.rate.het"=3.1),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate.msm",
                                              "global.transmission.rate.het"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 7.2  ----
# (There was a 6.29 but the stage 1 LL diagnosis stratified likelihood was -Inf for most, so now stage1 liks are only 0- and 1-way stratified)
register.calibration.info('calib.7.2.stage1.az',
                          preceding.calibration.codes = 'calib.6.29.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info("calib.7.2.stage2.az",
                          preceding.calibration.codes = 'calib.7.2.stage1.az',
                          likelihood.instructions = lik.inst.stage23,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info("calib.7.2.stage3.az",
                          preceding.calibration.codes = 'calib.7.2.stage2.az',
                          likelihood.instructions = lik.inst.stage23,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
)
# 7.6 ----
# stage 2 and 3 repeated 
register.calibration.info("calib.7.6.stage2.az",
                          preceding.calibration.codes = 'calib.7.2.stage1.az',
                          likelihood.instructions = lik.inst.stage23,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info("calib.7.6.stage3.az",
                          preceding.calibration.codes = 'calib.7.6.stage2.az',
                          likelihood.instructions = lik.inst.stage23,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(
                              TRANSMISSION.PARAMETERS.PRIOR@var.names,
                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
)

# 
# 
# # 6.5 (several changes) ----
# register.calibration.info("calib.6.5.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # stage1
# register.calibration.info('calib.6.5.stage1.az',
#                           preceding.calibration.codes = 'calib.6.5.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # stage2
# register.calibration.info("calib.6.5.stage2.az",
#                           preceding.calibration.codes = 'calib.6.5.stage1.az',
#                           likelihood.instructions = lik.inst.stage23,
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
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # stage3
# register.calibration.info("calib.6.5.stage3.az",
#                           preceding.calibration.codes = 'calib.6.5.stage2.az',
#                           likelihood.instructions = lik.inst.stage23,
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
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.8: seperating sti.screening slope for msm/het ----
# # STAGE0
# register.calibration.info('calib.6.8.stage1.az',
#                           preceding.calibration.codes = 'calib.6.5.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # STAGE1
# register.calibration.info("calib.6.8.stage2.az",
#                           preceding.calibration.codes = 'calib.6.8.stage1.az',
#                           likelihood.instructions = lik.inst.stage23,
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
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.9: lowering overall weight for stage2-3 likelihood lowered to 1/4 from 1/2 ----
# register.calibration.info("calib.6.9.stage2.az",
#                           preceding.calibration.codes = 'calib.6.8.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.fourth,
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
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.12: setting msm trans prior in 2000 to log(1) same as het ----
# register.calibration.info("calib.6.12.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.9,
#                                                              "global.transmission.rate.het"=3.9),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info('calib.6.12.stage1.az',
#                           preceding.calibration.codes = 'calib.6.12.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.12.stage2.az",
#                           preceding.calibration.codes = 'calib.6.12.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.12.stage3.az",
#                           preceding.calibration.codes = 'calib.6.12.stage2.az',
#                           likelihood.instructions = lik.inst.stage23.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.12: with penalty---- 
# # Same as above but with the new penalty likelihood
# # This STAGE 0 is exactly identical to "calib.6.12.stage0.az"
# register.calibration.info("calib.6.12.stg0.penalty",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.9,
#                                                              "global.transmission.rate.het"=3.9),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info('calib.6.12.stg1.penalty',
#                           preceding.calibration.codes = 'calib.6.12.stg0.penalty',
#                           likelihood.instructions = lik.inst.stage1.plus.penalty,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.12.stg2.penalty",
#                           preceding.calibration.codes = 'calib.6.12.stg1.penalty',
#                           likelihood.instructions = lik.inst.stage23.plus.penalty.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.12.stg3.penalty",
#                           preceding.calibration.codes = 'calib.6.12.stg2.penalty',
#                           likelihood.instructions = lik.inst.stage23.plus.penalty.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.16: adding covid reductions to sti.screenings ----
# register.calibration.info("calib.6.16.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.9,
#                                                              "global.transmission.rate.het"=3.9),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info('calib.6.16.stage1.az',
#                           preceding.calibration.codes = 'calib.6.16.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.16.stage2.az",
#                           preceding.calibration.codes = 'calib.6.16.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.16.stage3.az",
#                           preceding.calibration.codes = 'calib.6.16.stage2.az',
#                           likelihood.instructions = lik.inst.stage23.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# 
# # 6.16 with penalty ----
# # Same as above but with the new penalty likelihood
# # This STAGE 0 is exactly identical to "calib.6.16.stage0.az"
# register.calibration.info("calib.6.16.stg0.penalty",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.9,
#                                                              "global.transmission.rate.het"=3.9),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info('calib.6.16.stg1.penalty',
#                           preceding.calibration.codes = 'calib.6.16.stg0.penalty',
#                           likelihood.instructions = lik.inst.stage23.plus.penalty.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.16.stg2.penalty",
#                           preceding.calibration.codes = 'calib.6.16.stg1.penalty',
#                           likelihood.instructions = lik.inst.stage23.plus.penalty.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.16.stg3.penalty",
#                           preceding.calibration.codes = 'calib.6.16.stg2.penalty',
#                           likelihood.instructions = lik.inst.stage23.plus.penalty.fourth,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# 
# 
# 
# # 6.19: changing sti.screening.rate to linear spline function ----
# register.calibration.info("calib.6.19.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate.msm"=3.9,
#                                                              "global.transmission.rate.het"=3.9),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate.msm",
#                                               "global.transmission.rate.het"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info('calib.6.19.stage1.az',
#                           preceding.calibration.codes = 'calib.6.19.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.19.stage2.az",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.fourth, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.19.stage3.az",
#                           preceding.calibration.codes = 'calib.6.19.stage2.az',
#                           likelihood.instructions = lik.inst.stage23.eight,#w=1/8
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 10000, thin = 50, is.preliminary = F, n.chains = 4, max.run.time.seconds = 30, description = "NA"
# )
# # 6.23: trying different weights for a stage 2 off of 6.19.stage1 ----
# register.calibration.info("calib.6.23.stg2.8.pop.4",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.8x.pop.4x, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.23.stg2.8.pop.8",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.8x.pop.8x, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.23.stg2.16.pop.2",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.16x.pop.2x, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.23.stg2.16.pop.4",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.16x.pop.4x, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.6.23.stg2.32.pop.2",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.32x.pop.2x, #w=1/4
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # 6.25: built on 6.19 again, but with 8 & 4 weights and new data ----
# register.calibration.info("calib.6.25.stage2.az",
#                           preceding.calibration.codes = 'calib.6.19.stage1.az',
#                           likelihood.instructions = lik.inst.stage23.8x.pop.4x,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(
#                               TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names,
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names
#                           ),
#                           n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # ************************************----
# # Trying out global trates ----
# for (gtr in c(3.6, 3.7, 3.8, 3.9)) {
#     register.calibration.info(paste0("calib.6.12.stg0.", gtr),
#                               likelihood.instructions = lik.inst.stage0,
#                               data.manager = SURVEILLANCE.MANAGER,
#                               end.year = 2030,
#                               fixed.initial.parameter.values = c("global.transmission.rate"=gtr),  
#                               parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                                   AGING.PARAMETERS.PRIOR@var.names,
#                                                   "global.transmission.rate"),
#                               parameter.aliases = par.aliases.transmission,
#                               n.iter = 1000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
#     )
# }
