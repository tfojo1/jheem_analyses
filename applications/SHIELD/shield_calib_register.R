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

# STAGE0 ----
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

# repeating stage0 for Phoenix again, AGAIN. With only 10,000 iterations to finish a bit quicker.
register.calibration.info("calib.4.6.stage0.az",
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

# running calibration for all cities 
register.calibration.info("calib.4.13.stage0.pk",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# now sampling the misclassification fractions in stages 1 and 2.
register.calibration.info("calib.4.8.stage0.az",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# repeating stage0 for Phoenix again
register.calibration.info("calib.4.6.stage0.pk",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Repeating after fixing the fertility likelihood
register.calibration.info("calib.4.3.stage0.pk",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Repeating after the new revisions to symptomatic testing
# register.calibration.info("calib.3.30.stage0.pk",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.3),  
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )

# # Built on calib.3.17.stage0.az, making global trate = 2.3. Expecting some cities' diagnoses to go too high since it was 2.05 before.
# register.calibration.info("calib.3.22.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.3), # median is 2.2
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# # Has 1/32 weight AND correlation between strata = 0 for population
# register.calibration.info("calib.3.17.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.05), # median is 2.2
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# # Has 1/32 weight
# register.calibration.info("calib.3.13.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.05), # median is 2.2
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# register.calibration.info("calib.3.10.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.05), # median is 2.2
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# register.calibration.info("calib.2.19.stage0.az",
#                           likelihood.instructions = lik.inst.stage0,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           fixed.initial.parameter.values = c("global.transmission.rate"=2.05), # median is 2.2
#                           parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                               AGING.PARAMETERS.PRIOR@var.names,
#                                               "global.transmission.rate"),
#                           parameter.aliases = par.aliases.transmission,
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# STAGE1  ----
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

# now sampling the misclassification fractions in stages 1 and 2.
register.calibration.info('calib.4.13.stage1.pk',
                          preceding.calibration.codes = 'calib.4.13.stage0.pk',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# now sampling the misclassification fractions in stages 1 and 2.
register.calibration.info('calib.4.8.stage1.az',
                          preceding.calibration.codes = 'calib.4.8.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.4.3.stage1.pk',
                          preceding.calibration.codes = 'calib.4.3.stage0.pk',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Repeating after the new revisions to symptomatic testing
# fixed an error with rr.prp.sym. scale for het male
# register.calibration.info('calib.4.2.stage1.pk',
#                           preceding.calibration.codes = 'calib.3.30.stage0.pk',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# Repeating after the new revisions to symptomatic testing
# register.calibration.info('calib.3.30.stage1.pk',
#                           preceding.calibration.codes = 'calib.3.30.stage0.pk',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# 
# register.calibration.info('calib.3.2.stage1.az',
#                           preceding.calibration.codes = 'calib.2.19.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.3.12.stage1.az",
#                           preceding.calibration.codes = 'calib.2.19.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
#                           
# )
# register.calibration.info("calib.3.16.stage1.az",
#                           preceding.calibration.codes = 'calib.2.19.stage0.az',
#                           likelihood.instructions = lik.inst.stage1,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
#                           
# )
# STAGE2 ----
# Attempting a top-to-bottom calibration. Here, uses 8x on future penalty likelihood.
register.calibration.info('calib.4.24.stage2.az',
                          preceding.calibration.codes = 'calib.4.24.stage1.az',
                          likelihood.instructions = lik.inst.stage2.wFC,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# No future pen likelihood for now, to get the 7 workable for Doxy
register.calibration.info('calib.4.22.stage2.WO.fut',
                          preceding.calibration.codes = 'calib.4.8.stage1.az',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Like 4.21.stage2.az, but with 8x weight for future change lik
register.calibration.info('calib.4.22.stage2.az',
                          preceding.calibration.codes = 'calib.4.8.stage1.az',
                          likelihood.instructions = lik.inst.stage2.wFC,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Trying the new future change penalty likelihood for Houston and NYC to compare with the 4/8 calib
register.calibration.info('calib.4.21.stage2.az',
                          preceding.calibration.codes = 'calib.4.8.stage1.az',
                          likelihood.instructions = lik.inst.stage2.wFC,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# now sampling the misclassification fractions in stages 1 and 2.
register.calibration.info('calib.4.13.stage2.pk',
                          preceding.calibration.codes = 'calib.4.13.stage1.pk',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# now sampling the misclassification fractions in stages 1 and 2.
register.calibration.info('calib.4.8.stage2.az',
                          preceding.calibration.codes = 'calib.4.8.stage1.az',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.4.6.stage2.az',
                          preceding.calibration.codes = 'calib.4.3.stage1.pk',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.4.3.stage2.pk',
                          preceding.calibration.codes = 'calib.4.3.stage1.pk',
                          likelihood.instructions = lik.inst.stage2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Repeating after the new revisions to symptomatic testing
# fixed an error with rr.prp.sym. scale for het male
# register.calibration.info('calib.4.2.stage2.pk',
#                           preceding.calibration.codes = 'calib.4.2.stage1.pk',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# # Repeating after the new revisions to symptomatic testing
# register.calibration.info('calib.3.30.stage2.pk',
#                           preceding.calibration.codes = 'calib.3.30.stage1.pk',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names,
#                                               TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# @andrew: how come you didnt have the TRANS.BY.AGE.SAMPLING.PRIOR in stage2 before? 

# register.calibration.info('calib.3.12.stage2.az',
#                           preceding.calibration.codes = 'calib.3.12.stage1.az',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
# )
# register.calibration.info("calib.3.23.stage2.az",
#                           preceding.calibration.codes = 'calib.3.16.stage1.az',
#                           likelihood.instructions = lik.inst.stage2,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                               STI.TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
#                           
# )
# STAGE3 ----
# Attempting a top-to-bottom calibration. Here will use weight 1. Hope to end with 100 simulations.
register.calibration.info('calib.4.24.stage3.az',
                          preceding.calibration.codes = 'calib.4.24.stage2.az',
                          likelihood.instructions = lik.inst.stage3,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2035,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 10000, thin = 50, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info('calib.4.24.stage3.CTRL',
                          preceding.calibration.codes = 'calib.4.24.stage2.az',
                          likelihood.instructions = lik.inst.stage3.CTRL,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2035,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 10000, thin = 50, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info('calib.4.24.stage3.hf',
                          preceding.calibration.codes = 'calib.4.24.stage2.az',
                          likelihood.instructions = lik.inst.stage3.half,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2035,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 10000, thin = 50, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
)
register.calibration.info('calib.4.24.stage3.qtr',
                          preceding.calibration.codes = 'calib.4.24.stage2.az',
                          likelihood.instructions = lik.inst.stage3.quarter,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2035,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 10000, thin = 50, is.preliminary = F, max.run.time.seconds = 30, description = "NA"
)