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
for (rate in c(20,22)) {
    print(paste0("calib.5.11.stg0.gtr.", rate))
    register.calibration.info(paste0("calib.5.11.stg0.gtr.", rate),
                              likelihood.instructions = lik.inst.stage0,
                              data.manager = SURVEILLANCE.MANAGER,
                              end.year = 2030,
                              fixed.initial.parameter.values = c("global.transmission.rate"=rate/10),  
                              parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                                  AGING.PARAMETERS.PRIOR@var.names,
                                                  "global.transmission.rate"),
                              parameter.aliases = par.aliases.transmission,
                              n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
    )
}
# for (rate in seq(1.6, 2.6, by=0.2)) {
#     print(paste0("calib.5.7.stg0.gtr.", round(rate*10,0)))
#     register.calibration.info(paste0("calib.5.7.stg0.gtr.", round(rate*10,0)),
#                               likelihood.instructions = lik.inst.stage0,
#                               data.manager = SURVEILLANCE.MANAGER,
#                               end.year = 2030,
#                               fixed.initial.parameter.values = c("global.transmission.rate"=rate),  
#                               parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
#                                                   AGING.PARAMETERS.PRIOR@var.names,
#                                                   "global.transmission.rate"),
#                               parameter.aliases = par.aliases.transmission,
#                               n.iter = 5000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
#     )
# }
#  
# STAGE 1
register.calibration.info('calib.5.12.stage1.pk',
                          preceding.calibration.codes = 'calib.5.11.stg0.gtr.20',
                          likelihood.instructions = lik.inst.stage1,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              STI.TESTING.PARAMETERS.PRIOR@var.names,
                                              TRANS.BY.AGE.SAMPLING.PRIOR@var.names),
                          n.iter = 15000, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# STAGE 2
register.calibration.info("calib.5.12.stage2.pk",
                          preceding.calibration.codes = 'calib.5.12.stage1.pk',
                          likelihood.instructions = lik.inst.stage23,
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