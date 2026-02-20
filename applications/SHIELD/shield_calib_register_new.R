# New Andrew calibration

cat("*** Registering SHIELD calibration ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
N_ITER=20000

# this lets us tune the transmission multipliers for MSM and het in each timepoint together 
par.aliases.transmission = list(
    trate.0 = c("transmission.rate.multiplier.msm1970","transmission.rate.multiplier.heterosexual1970"),
    trate.1 = c("transmission.rate.multiplier.msm1990","transmission.rate.multiplier.heterosexual1990"),
    trate.2 = c("transmission.rate.multiplier.msm1995","transmission.rate.multiplier.heterosexual1995"),
    trate.3 = c("transmission.rate.multiplier.msm2000", "transmission.rate.multiplier.heterosexual2000"),
    trate.4 = c("transmission.rate.multiplier.msm2010", "transmission.rate.multiplier.heterosexual2010"),
    trate.5 = c("transmission.rate.multiplier.msm2017", "transmission.rate.multiplier.heterosexual2017")
)

# STAGE 0 Demographics thin 50
register.calibration.info("calib.2.19.stage0.az",
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          fixed.initial.parameter.values = c("global.transmission.rate"=2.05), # median is 2.2
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#STAGE1
register.calibration.info('calib.2.19.stage1.az',
                          preceding.calibration.codes = 'calib.2.19.stage0.az',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N_ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)