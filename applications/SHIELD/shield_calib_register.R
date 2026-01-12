cat("*** Running Shiled_register_calibration.R ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

N.ITER=15000
# shield.solver = create.solver.metadata(rtol = 0.001, atol=0.03) #rtol,atol
# default.solver= create.solver.metadata()

#parameter set for demographic & diagnosis calibration
param.names.all<-c(
    POPULATION.PARAMETERS.PRIOR@var.names,
    AGING.PARAMETERS.PRIOR@var.names,
    TRANSMISSION.PARAMETERS.PRIOR@var.names,
    TESTING.PARAMETERS.PRIOR@var.names,
    AGE.PARAMETERS.PRIOR@var.names
)


par.aliases.transmission = list(
    
    trate.0 = c("transmission.rate.multiplier.msm1970","transmission.rate.multiplier.heterosexual1970"),
    trate.1 = c("transmission.rate.multiplier.msm1990","transmission.rate.multiplier.heterosexual1990"),
    trate.2 = c("transmission.rate.multiplier.msm1995","transmission.rate.multiplier.heterosexual1995"),
    trate.3 = c("transmission.rate.multiplier.msm2000", "transmission.rate.multiplier.heterosexual2000"),
    trate.4 = c("transmission.rate.multiplier.msm2010", "transmission.rate.multiplier.heterosexual2010"),
    trate.5 = c("transmission.rate.multiplier.msm2020", "transmission.rate.multiplier.heterosexual2020")
    
)


############################Demographic

register.calibration.info('calib.demog.MIA.09.24', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.demog.NYC.09.24', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.demog.BLT.09.24', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.demog.ATL.09.24', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)








############################
register.calibration.info('calib.MIA.10.08.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.08.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.08.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.08.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
#########################

register.calibration.info('calib.MIA.10.21.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.21.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.21.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.21.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################

register.calibration.info('calib.MIA.10.23.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.23.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.23.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.23.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


#########################

register.calibration.info('calib.MIA.10.29.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.29.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.29.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.29.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


###########################

register.calibration.info('calib.MIA.10.30.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.30.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.30.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.30.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.6),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)




###########################

register.calibration.info('calib.MIA.10.31.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.31.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.31.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.31.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

###########################

register.calibration.info('calib.MIA.10.31.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.31.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.31.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.31.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

###########################

register.calibration.info('calib.MIA.10.31.demog.3', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.10.31.demog.3', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.31.demog.3', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.10.31.demog.3', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
###########################

register.calibration.info('calib.MIA.11.04.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.ATL.11.04.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.04.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.BLT.11.04.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
###################

register.calibration.info('calib.11.10.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.10.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.11.20.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Has 1/32 demog and 1/8 diag, age 19 sampled alone
register.calibration.info('calib.11.21.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# Has updated age mix prior
register.calibration.info('calib.11.21.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# ###### with penalty and stratified diagnoses (no congenital added)
####

#############################################
register.calibration.info('calib.MIA.10.06.2way.2',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.06.2way.2',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.06.2way.2',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 60, description = "NA"
)


register.calibration.info('calib.BLT.10.06.2way.2',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#############################################
register.calibration.info('calib.MIA.10.06.2way.3',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.06.2way.3',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.06.2way.3',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 60, description = "NA"
)


register.calibration.info('calib.BLT.10.06.2way.3',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
#####################################################

register.calibration.info('calib.MIA.10.08.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.08.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.08.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.08.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.08.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.08.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.08.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.08.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
##########################################

register.calibration.info('calib.MIA.10.08.2way.2',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.08.2way.2',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.08.2way.2',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.08.2way.2',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

##########################################

register.calibration.info('calib.MIA.10.10.2way.1',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.10.2way.1',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.10.2way.1',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.10.2way.1',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


##########################################

register.calibration.info('calib.MIA.10.16.2way.1',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.16.2way.1',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.16.2way.1',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.16.2way.1',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

##########################################

register.calibration.info('calib.MIA.10.17.2way.1',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.17.2way.1',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.17.2way.1',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.17.2way.1',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
##########################################

register.calibration.info('calib.MIA.10.20.2way.1',
                          preceding.calibration.codes = 'calib.demog.MIA.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.678068),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.20.2way.1',
                          preceding.calibration.codes = 'calib.demog.ATL.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.727956),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.20.2way.1',
                          preceding.calibration.codes = 'calib.demog.NYC.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.676484),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.20.2way.1',
                          preceding.calibration.codes = 'calib.demog.BLT.09.24',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.7811071),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

###############################################

register.calibration.info('calib.MIA.10.22.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.23.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 6.751841),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.22.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.23.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 6.966474),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.22.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.23.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=6.699208),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.22.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.23.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=7.155683),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

###########################################


register.calibration.info('calib.MIA.10.24.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.751841),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.24.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.966474),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.24.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=6.699208),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.24.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=7.155683),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

############################################

register.calibration.info('calib.MIA.10.28.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.751841),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.28.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.966474),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.28.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=6.699208),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.28.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=7.155683),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

##########################################


register.calibration.info('calib.MIA.10.28.2way.2',
                          preceding.calibration.codes = 'calib.MIA.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.751841),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.28.2way.2',
                          preceding.calibration.codes = 'calib.ATL.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.966474),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.28.2way.2',
                          preceding.calibration.codes = 'calib.NYC.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=6.699208),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.28.2way.2',
                          preceding.calibration.codes = 'calib.BLT.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=7.155683),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#######################################

register.calibration.info('calib.MIA.10.28.2way.3',
                          preceding.calibration.codes = 'calib.MIA.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.751841),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.28.2way.3',
                          preceding.calibration.codes = 'calib.ATL.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 6.966474),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.28.2way.3',
                          preceding.calibration.codes = 'calib.NYC.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=6.699208),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.28.2way.3',
                          preceding.calibration.codes = 'calib.BLT.10.22.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=7.155683),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

###############################################

register.calibration.info('calib.MIA.10.30.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.29.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 2.200608),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.30.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.29.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 2.275271),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.30.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.29.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.648726),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.30.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.29.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=2.331478),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


###############################################

register.calibration.info('calib.MIA.10.31.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.30.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.10.31.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.30.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.10.31.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.30.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.10.31.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.30.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


###############################################

register.calibration.info('calib.MIA.11.01.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.31.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.01.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.31.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.01.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.31.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.01.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.31.2way.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=2.2),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################################

register.calibration.info('calib.MIA.11.02.2way.1',
                          preceding.calibration.codes = 'calib.MIA.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 6.770918 ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.02.2way.1',
                          preceding.calibration.codes = 'calib.ATL.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 6.997742),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.02.2way.1',
                          preceding.calibration.codes = 'calib.NYC.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=6.177395),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.02.2way.1',
                          preceding.calibration.codes = 'calib.BLT.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=5.456728),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################################

register.calibration.info('calib.MIA.11.03.2way.2',
                          preceding.calibration.codes = 'calib.MIA.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.03.2way.2',
                          preceding.calibration.codes = 'calib.ATL.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.03.2way.2',
                          preceding.calibration.codes = 'calib.NYC.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.03.2way.2',
                          preceding.calibration.codes = 'calib.BLT.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


#########################################

register.calibration.info('calib.MIA.11.05.2way.1',
                          preceding.calibration.codes = 'calib.MIA.11.04.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.05.2way.1',
                          preceding.calibration.codes = 'calib.ATL.11.04.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.05.2way.1',
                          preceding.calibration.codes = 'calib.NYC.11.04.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.05.2way.1',
                          preceding.calibration.codes = 'calib.BLT.11.04.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)







#########################################################



register.calibration.info('calib.MIA.11.05.2way.2',
                          preceding.calibration.codes = 'calib.MIA.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.05.2way.2',
                          preceding.calibration.codes = 'calib.ATL.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.05.2way.2',
                          preceding.calibration.codes = 'calib.NYC.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.05.2way.2',
                          preceding.calibration.codes = 'calib.BLT.10.31.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################################################

register.calibration.info('calib.MIA.11.05.2way.3',
                          preceding.calibration.codes = 'calib.MIA.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.05.2way.3',
                          preceding.calibration.codes = 'calib.ATL.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.05.2way.3',
                          preceding.calibration.codes = 'calib.NYC.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.05.2way.3',
                          preceding.calibration.codes = 'calib.BLT.10.31.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


#########################################################

register.calibration.info('calib.MIA.11.05.2way.4',
                          preceding.calibration.codes = 'calib.MIA.10.31.demog.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.05.2way.4',
                          preceding.calibration.codes = 'calib.ATL.10.31.demog.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.05.2way.4',
                          preceding.calibration.codes = 'calib.NYC.10.31.demog.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.05.2way.4',
                          preceding.calibration.codes = 'calib.BLT.10.31.demog.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


#########################################

register.calibration.info('calib.MIA.11.06.2way.1',
                          preceding.calibration.codes = 'calib.MIA.11.05.2way.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.06.2way.1',
                          preceding.calibration.codes = 'calib.ATL.11.05.2way.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.06.2way.1',
                          preceding.calibration.codes = 'calib.NYC.11.05.2way.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.06.2way.1',
                          preceding.calibration.codes = 'calib.BLT.11.05.2way.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)







#########################################################



register.calibration.info('calib.MIA.11.06.2way.2',
                          preceding.calibration.codes = 'calib.MIA.11.05.2way.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.06.2way.2',
                          preceding.calibration.codes = 'calib.ATL.11.05.2way.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.06.2way.2',
                          preceding.calibration.codes = 'calib.NYC.11.05.2way.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.06.2way.2',
                          preceding.calibration.codes = 'calib.BLT.11.05.2way.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################################################

register.calibration.info('calib.MIA.11.06.2way.3',
                          preceding.calibration.codes = 'calib.MIA.11.05.2way.4',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.ATL.11.06.2way.3',
                          preceding.calibration.codes = 'calib.ATL.11.05.2way.4',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790231),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.NYC.11.06.2way.3',
                          preceding.calibration.codes = 'calib.NYC.11.05.2way.4',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.769388),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.BLT.11.06.2way.3',
                          preceding.calibration.codes = 'calib.BLT.11.05.2way.4',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate=1.869432),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#########################################################

register.calibration.info('calib.11.11.stage1.1',
                          preceding.calibration.codes = 'calib.11.10.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.11.stage1.2',
                          preceding.calibration.codes = 'calib.11.10.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.12.stage2.1',
                          preceding.calibration.codes = 'calib.11.11.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          #fixed.initial.parameter.values = c(global.transmission.rate= 1.790618),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.12.stage2.2',
                          preceding.calibration.codes = 'calib.11.11.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.11.13.stage2.1',
                          preceding.calibration.codes = 'calib.11.11.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.13.stage2.2',
                          preceding.calibration.codes = 'calib.11.11.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.14.stage2.1',
                          preceding.calibration.codes = 'calib.11.11.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.14.stage2.2',
                          preceding.calibration.codes = 'calib.11.11.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.11.18.stage2.1',
                          preceding.calibration.codes = 'calib.11.11.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.18.stage2.2',
                          preceding.calibration.codes = 'calib.11.11.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names),
                          
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



register.calibration.info('calib.11.19.stage1.1',
                          preceding.calibration.codes = 'calib.11.10.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.19.stage1.2',
                          preceding.calibration.codes = 'calib.11.10.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.11.20.stage2.1',
                          preceding.calibration.codes = 'calib.11.19.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.20.stage2.2',
                          preceding.calibration.codes = 'calib.11.19.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 1/4 weight
register.calibration.info('calib.11.20.stage1.1',
                          preceding.calibration.codes = 'calib.11.10.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# 1/4 weight
register.calibration.info('calib.11.20.stage1.2',
                          preceding.calibration.codes = 'calib.11.10.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
# 1/8 weight
register.calibration.info('calib.11.20.stage1.3',
                          preceding.calibration.codes = 'calib.11.10.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 1/16 weight
register.calibration.info('calib.11.20.stage1.4',
                          preceding.calibration.codes = 'calib.11.10.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 1/8 weight
register.calibration.info('calib.11.21.stage2.3',
                          preceding.calibration.codes = 'calib.11.20.stage1.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



# 1/8 weight, age mixing not sampled and age19 sampled seperately


register.calibration.info('calib.11.22.stage1.1',
                          preceding.calibration.codes = 'calib.11.21.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 1/8 weight, age mixing (0.5) not sampled and age19 sampled seperately

register.calibration.info('calib.11.22.stage1.2',
                          preceding.calibration.codes = 'calib.11.21.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



# 1/8 weight, age mixing not sampled until stage2 and age19 sampled seperately


register.calibration.info('calib.11.24.stage2.1',
                          preceding.calibration.codes = 'calib.11.22.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# 1/8 weight, age mixing (0.5) not sampled and age19 sampled seperately

register.calibration.info('calib.11.24.stage2.2',
                          preceding.calibration.codes = 'calib.11.22.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


######################AGE 19 in vs out of MVN ##################################



#### Demog (stage 0)
# Within MVN
register.calibration.info('calib.11.25.demog.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

# Outside MVN
register.calibration.info('calib.11.25.demog.2', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


#### Stage 1

register.calibration.info('calib.11.27.stage1.1',
                          preceding.calibration.codes = 'calib.11.25.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.27.stage1.2',
                          preceding.calibration.codes = 'calib.11.25.demog.2',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#### Stage 2


register.calibration.info('calib.11.28.stage2.1',
                          preceding.calibration.codes = 'calib.11.27.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.11.28.stage2.2',
                          preceding.calibration.codes = 'calib.11.27.stage1.2',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)
######################AGE.SD.Sampled in stage 1 vs stage 2######################


#### Demog (stage 0)

# control is 'calib.11.25.demog.1'

# sampled in stage 1:

register.calibration.info('calib.11.25.demog.3', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#### Stage 1
# control is 'calib.11.25.stage.1.1'
# sampled in stage 1:

register.calibration.info('calib.11.27.stage1.3',
                          preceding.calibration.codes = 'calib.11.25.demog.3',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.12.1.stage1.1',
                          preceding.calibration.codes = 'calib.11.25.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.12.1.stage1.3',
                          preceding.calibration.codes = 'calib.11.25.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              "age.mixing.sd.mult"
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)




#### Stage 2

register.calibration.info('calib.11.28.stage2.3',
                          preceding.calibration.codes = 'calib.11.27.stage1.3',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


########### Rerun

register.calibration.info('calib.12.1.stage2.test',
                          preceding.calibration.codes = 'calib.11.27.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.12.2.stage2.1',
                          preceding.calibration.codes = 'calib.12.1.stage1.1',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


register.calibration.info('calib.12.2.stage2.3',
                          preceding.calibration.codes = 'calib.12.1.stage1.3',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

######################### AGE SD MULT prior shift  #############################


#### Demog (stage 0)

register.calibration.info('calib.11.25.demog.4', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#### Stage 1
register.calibration.info('calib.11.27.stage1.4',
                          preceding.calibration.codes = 'calib.11.25.demog.4',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



#### Stage 2


register.calibration.info('calib.11.28.stage2.4',
                          preceding.calibration.codes = 'calib.11.27.stage1.4',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)


######################### SD term in Stratified LH  ############################



#### Demog (stage 0) No need to run

#### Stage 1
register.calibration.info('calib.11.27.stage1.5',
                          preceding.calibration.codes = 'calib.11.25.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.SD.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)



#### Stage 2


register.calibration.info('calib.11.28.stage2.5',
                          preceding.calibration.codes = 'calib.11.27.stage1.5',
                          likelihood.instructions = lik.inst.diag.strata.stage2.SD.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)







###################### Changing stage 1 objectives ##############################


#### Demog (stage 0) No need to run


#### Stage 1: 2 way sex race vs 1 way sex race age

register.calibration.info('calib.11.27.stage1.6',
                          preceding.calibration.codes = 'calib.11.25.demog.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.1way.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#### Stage 2: 2 way sex race age

register.calibration.info('calib.11.28.stage2.6',
                          preceding.calibration.codes = 'calib.11.27.stage1.6',
                          likelihood.instructions = lik.inst.diag.strata.stage2.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

######################## Checking replication of fit for CROI

register.calibration.info('calib.01.08.stage0.1', 
                          likelihood.instructions = lik.inst.demog.TD.2,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

register.calibration.info('calib.01.09.stage1.1',
                          preceding.calibration.codes = 'calib.01.08.stage0.1',
                          likelihood.instructions = lik.inst.diag.strata.stage1.no.demog.w.future.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                              TESTING.PARAMETERS.PRIOR@var.names,
                                              AGE.PARAMETERS.PRIOR@var.names
                          ),
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)




# LOG SUMMARY -----
# <calib.07.10.pk*> adding EL specific knots in 1990,95,2000,2010,2020 for screening to see if it works
#0-likelihoods Totals, all with w=1/8 weight
#1-likelihoods Totals, all with w=1/8 weight except for EL (w=1/4)
#2-likelihoods Totals, all with w=1/8 weight except for EL (w=1/2)
#3-likelihoods Totals, all with w=1/8 weight except for EL (w=1)

# <calib.07.09.pk*> running with different weights for EL
#0-likelihoods Totals, all with w=1/8 weight
#1-likelihoods Totals, all with w=1/8 weight except for EL (w=1/4)
#2-likelihoods Totals, all with w=1/8 weight except for EL (w=1/2)
#3-likelihoods Totals, all with w=1/8 weight except for EL (w=1) >>> Best fit

# <calib.diagnosis.07.08.pk>
# increasing the weight of EL diagnosis to see if it works
# <calib.diagnosis.07.08.pk1> using total likelihoods, all with w=1/8 except for EL
#1-likelihoods Totals, all with w=1/8 weight except for EL (w=1/4)
#2-likelihoods Totals, all with w=1/8 weight except for EL (w=1/2)
#3-likelihoods Totals, all with w=1/8 weight except for EL (w=1)


# <calib.diagnosis.07.07.pk1>
# rerunning *4 from yesterday after revising the HIV likelihood to use the "TOTALS" only.
# <<calib.diagnosis.07.07.pk1>> total likelihoods, using the previous calibration as starting point
# <<calib.diagnosis.07.07.pk2>> stratified likelihoods, using the previous calibration as starting point
# <<calib.diagnosis.07.07.pk11>> total likelihoods, using the demog calibration as starting point
# <<calib.diagnosis.07.07.pk22>> stratified likelihoods, using the demog calibration as starting point
# The starting point from demographic wasnt good, dismissing *11, *22
# stratified diagnosis data are problematic (dismissing *2)
# <calib.diagnosis.07.06.pk1>
#adding additional knots to symptomatic testing to align with transmission: 1970,90,95,2000,2010,2020
# >>> this is a good fit, and it captures the tails of late diagnosis well
# <calib.diagnosis.07.06.pk2>
#revising knots in sti.screening function to be the same : 1970,90,95,2000,2010,2020
# <calib.diagnosis.07.06.pk3> #revising the sym.testing spline function to use a knot.link=logit, and use link=identity. 
# >>> using identity link for probabilities doesnt make sense. Ignore this run
# <calib.diagnosis.07.06.pk4> #revising the sym.testing spline function to use a knot.link=logit, and use link=logit 
# >>> this is exactly the same as *2. why? #'@Todd? 


# <calib.07.03.pk1> ----
# repeating Ryan's run with the last sim as starting point
# EL infectiousness = ON; Relapse=ON, PS screening=ON ; contact tracing=ON


# <calib.diagnosis.07.02.pk1> ----
# repeating calibration from yesterday with weight 1/8, running another one with proceeding=calib.diagnosis.07.01.pk1 

# <calib.diag.07.02.pk[2...*] > using the demographic calibration as the starting point (calib.diagnosis.06.30.pk1)
# <calib.diag.07.02.pk2> # EL transmissibility = ON
# <calib.diag.07.02.pk3> # EL transmissibility = ON; Relapse=ON
# <calib.diag.07.02.pk4> # EL transmissibility = ON; Relapse=ON, PS screening=ON [Range of 0.13-1.9]
# <calib.diag.07.02.pk5> # EL transmissibility = ON; Relapse=ON, PS screening=ON ; contact tracing=ON
# <calib.diag.07.02.pk6> same as before, adding a contact.tracing parameter to calibration


# <calib.diagnosis.07.01.pk1> ----
# w=1/8
# revised the prior for EL and LL sti screening multipliers
# change the diagnosis likelihood to use "autoregressive.1" correlation instead of compound symmetry
# >>> this one works great

# <calib.diagnosis.07.01.pk2> # downweighting likelihoods to w=1/32 to make sure it mixes well
# >>> this one didnt work as well

# <calib.diagnosis.06.30.pk1> ----
# changing the initial number infected in 1970
# adding new transmission multiplier in 1990
# calibrating to PS total, EL total, Late total, HIV tests
# only changing Transmission and Testing Parameters
# excluding sti.screening.multiplier.ps from calibration
# >>> The initial peak pre-1990 is gone. Simulations have a hard time catching up with EL diagnoses.
# >>> Manual try suggested high levels of screening for EL and LL is needed

# <calib.diagnosis.06.30.pk1> 
#  

# ## TEST for Nick:
# register.calibration.info('pop.demog.test', 
#                           likelihood.instructions = likelihood.instructions.demographics,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,  
#                           parameter.names = c(
#                               POPULATION.PARAMETERS.PRIOR@var.names,
#                               AGING.PARAMETERS.PRIOR@var.names 
#                           ), 
#                           n.iter = 10,
#                           thin = 1, 
#                           is.preliminary = T, 
#                           max.run.time.seconds = 30, 
#                           description = "A quick run to get population parameters in the general vicinity",
#                           solver.metadata = solver
# )
cat("*** Shiled_register_calibration.R completed!***\n")


# fixed.initial.parameter.values = c(global.transmission.rate=3), #change the initial value for a parameter
# is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
# max.run.time.seconds = 30,  # If solving the differential equations takes longer than this, it returns a ‘degenerate’ sim where all outcomes are NA and the likelihood evaluates to -Inf.
# get.contained.locations('US', 'CBSA')

#LOG -----
# simsets are saved in: /Volumes/jheem$/shield/pop.demog.1.Rdata

#######
#05.13: <syphilis.diagnoses.5>  calibrating to total diagnosis by stage again to make sure we can hit the one peak in 1995
# >> 51%, parameters were mixing well, but couldnt fit the peak in 1995

#05.14: <syphilis.diagnoses.6.RF> add the weight param to likelihood and set to 0.5 to weaken the likelihoods 

#05.15: <syphilis.diagnoses.7.pk> reducing atol to 0.1, using 0.5 weight to loosen the likelihoods # >> the chain is not mixing well.
#05.15: <syphilis.diagnoses.7.pk1> reverting changes in rtol/atol and weight to check that model performs as expected# >> model is working well and parameters are mixing at 40% completion. 
#05.15: <syphilis.diagnoses.7.RF> reducing atol to 0.1, using 0.8 weight to loosen the likelihoods

#05.15: <syphilis.diagnoses.8.RF> using 0.8 weight to loosen the likelihoods. atol restored to default >> running
# 05.16: <syphilis.diagnoses.8.pk> using rtol=0.01, atol=0.1, and Total.weight=0.8; starting from priors:
# ROCKFISH >> 83%complete, params not mixing at all (could this be an issue with starting priors? Ryan's chain is mixing well)

# 5.19: <syphilis.diag.9.pk.***> Total.weight=0.8:  starting from pop.demog.8's model, trying diagnosis likelihoods one a time
# >> at 70% complete, we are getting closer to the peark and it's mixing well. 
# >>> seems like the chain need more time
# >>> we can help the priors to have a better strarting point
# >>> to speeup the model, we can change the start year to 1970

# 5.20: <syphilis.10.pk.psTotal> Total.weight=0.8:  starting from pop.demog.8's model, trying ps.total diagnosis with dynamic weights 
# >> ongoing 

# 5.21: <syphilis.11.pk.psTotal> same settings, revising the prior for transmission in 1995 & 2000, reducing number of outputs to speed up the model
# >> in progress

# 5.23: <syphilis.11.rf.psElTotal> now includes likelihood for PS and EL stages

# 5.27: <syphilis.12.rf.psElTotal> Includes testing parameters for calibration

# 5.30: <syphilis.13.rf.psElTotal> adding likelihood for HIV testing

# 6.02: <syphilis.14.rf.psElTotal> added scenond transition route for latent 

# 6.03: <syphilis.15.rf.psElTotal> reverted to 5.30 model, modified to have multiplier for ps screening 

# 6.04: <syphilis.16.rf.psElTotal> included  multiplier for el screening

# 6.06: <syphilis.6.06.rf.psElTotal> added 1970s calibratable params, changed default year to 1970s

# 6.09: revising the symp.testing logic. Now it occurs during the stages. 
# 6.09: <calib.demog.06.09.pk>: running a demographic calibration to fit the targets with 1970 start date
# >> this run worked well. will be using this simset as the starting one for next calibrations

#6.09 <calib.diagnosis.06.09.pk>: running a calibration with demographic, total ps, total EL and hiv test likelihoods
# >> we still cant generate the peak in 1997 for the EL

#6.10 <calib.diagnosis.06.10.rf>: Added tunable screening sti knot values for 1990 and 2000 

#6.11 <calib.diagnosis.06.11.2.rf>: 1) set contact tracing prp reached to 0
# 2) set prenatal care to zero
# 3) Diagnosis LH weight set to 0.8
# 4) Removed likelihood for demographics

#6.12 <calib.diagnosis.06.12.rf>: 1) fixed values for ps and el screening multi
# 2) removed max knot values in sti screening functional form

#06.17:  <calib.diag.06.17.pk> using a weight of 1/8
# calibrating to total ps; total EL diagnosis and hiv tests targets
# downweighting the likelihood
# 1) removed relapse =0 #2) removed infectiousness for EL stage =0 #3) removed screening for PS (muliplier set to 0) #4) took out prenatal care #5) took out contact tracing
##### calib.diag.06.17.pk1# changing the weight to 1/16

#06.19:  <calib.diag.06.19.pk> using a weight of 1/8
# calibrating to total ps; total EL diagnosis and hiv tests targets
# downweighting the likelihood
# 1) removed relapse =0 #2) removed infectiousness for EL stage =0 #3) removed screening for PS (muliplier set to 0) #4) took out prenatal care #5) took out contact tracing


# # a temporary local calibration to test the HIV numbers 
# register.calibration.info(code = "calib.07.02.temp",
#                           preceding.calibration.codes = "calib.diagnosis.07.01.pk1", #calibrated diagnosis model
#                           likelihood.instructions = likelihood.instructions.syphilis.diag.total.no.demog, # PS total, EL total, Late total, HIV tests
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030,
#                           parameter.names = 
#                               c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
#                                 TESTING.PARAMETERS.PRIOR@var.names),
#                           n.iter = 10, ###!!! MAke sure to
#                           thin =1,###!!!
#                           is.preliminary = T,
#                           max.run.time.seconds = 30,
#                           description = "A quick run to get syphilis parameters in the general vicinity")