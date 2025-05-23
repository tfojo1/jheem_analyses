cat("*** Running Shiled_register_calibration.R ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

N.ITER=15000
solver = create.solver.metadata(rtol = 0.001, atol=0.05) #rtol,atol
# solver = create.solver.metadata() #default solver
param.names.trans.demog<-c(POPULATION.PARAMETERS.PRIOR@var.names,
                           AGING.PARAMETERS.PRIOR@var.names,
                           TRANSMISSION.PARAMETERS.PRIOR@var.names)


# Calibrating to demographic and syphilis diagnoses targets
register.calibration.info('pop.demog.8', 
                          likelihood.instructions = likelihood.instructions.demographics,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names 
                          ), 
                          n.iter = N.ITER,
                          thin = 50, 
                          is.preliminary = T, 
                          max.run.time.seconds = 30, 
                          description = "A quick run to get population parameters in the general vicinity",
                          solver.metadata = solver
)
## TEST for Nick:
register.calibration.info('pop.demog.test', 
                          likelihood.instructions = likelihood.instructions.demographics,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names 
                          ), 
                          n.iter = 10,
                          thin = 1, 
                          is.preliminary = T, 
                          max.run.time.seconds = 30, 
                          description = "A quick run to get population parameters in the general vicinity",
                          solver.metadata = solver
)
#############
register.calibration.info('syphilis.diagnoses.8.RF',
                          preceding.calibration.codes = 'pop.demog.8',
                          likelihood.instructions = likelihood.instructions.syphilis.diagnoses.all.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficient way is to run it to the last year of data; but it's also helpful to review projections for start
                          param.names.trans.demog,
                          n.iter = N.ITER,
                          thin = 50,
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity",
                          solver.metadata = solver
)

register.calibration.info('syphilis.diagnoses.8.pk',
                          # preceding.calibration.codes = 'pop.demog.8',
                          likelihood.instructions = likelihood.instructions.syphilis.diagnoses.all.totals,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficient way is to run it to the last year of data; but it's also helpful to review projections for start
                          param.names.trans.demog,
                          n.iter = N.ITER,
                          thin = 50,
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity",
                          solver.metadata = solver
)
##############
# Trying diagnosis targets one at a time
calibnames=c("psElTotal","all.totals")
lapply(calibnames,function(x){
register.calibration.info(code = paste0('syphilis.11.pk.',x), 
                          preceding.calibration.codes = "pop.demog.8",
                          likelihood.instructions = get(paste0("likelihood.instructions.syphilis.diagnoses.",x)),  
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          param.names.trans.demog, 
                          n.iter = N.ITER,
                          thin = 50, 
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity",
                          solver.metadata = solver
)
})


# Trying diagnosis targets one at a time
calibnames=c("psElTotal","psLlTotal","all.totals")
lapply(calibnames,function(x){
    register.calibration.info(code = paste0('syphilis.11.rf.',x), 
                              preceding.calibration.codes = "pop.demog.8",
                              likelihood.instructions = get(paste0("likelihood.instructions.syphilis.diagnoses.",x)),  
                              data.manager = SURVEILLANCE.MANAGER,
                              end.year = 2030, 
                              param.names.trans.demog, 
                              n.iter = N.ITER,
                              thin = 50, 
                              is.preliminary = T,
                              max.run.time.seconds = 30,
                              description = "A quick run to get syphilis parameters in the general vicinity",
                              solver.metadata = solver
    )
})


cat("*** Shiled_register_calibration.R completed!***\n")


# fixed.initial.parameter.values = c(global.transmission.rate=3), #change the initial value for a parameter
# is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
# max.run.time.seconds = 30,  # If solving the differential equations takes longer than this, it returns a ‘degenerate’ sim where all outcomes are NA and the likelihood evaluates to -Inf.
# get.contained.locations('US', 'CBSA')

#LOG -----
# simsets are saved in: /Volumes/jheem$/shield/pop.demog.1.Rdata
#04.14: PK: I removed initial infected population to speed up the sims #check base_params prp.of.initial.population.infected.syphilis=0
#04.18: Ryan added immigration parameters
#04.23: <pop.demog.1> we found discrepancies in the population size, Zoe is working on a fix
#04.25: <pop.demog.wEmigration> same chain run with 6 parameters by Ryan
#04.29: <pop.demog.2> NYC. Zoe has update the survillance data. we have a bad fit to older agegroups, and the fertility in Baltimore. 
#04.29: <pop.demog.3> Baltimore, added race.age interaction parameters for fertility.rates (18 params)
#04.29: <pop.demog.4> Baltimore, added race.age.time interaction parameters for fertility.rates (32 params)
#04.30: <pop.demog.3.pk> same run as <pop.demog.3>  
#05.01: <pop.demog.5> Baltimore, rolled back to 9 params for fert., added fert. calbration start at 2005 and population variance function added
#05.02: <pop.demog.6> Baltimore, rolled back to 18 params for fert.
#05.05: <pop.demog.7> Baltimore, rolled back to 9 params for fert., Todd fixed issue with JHEEM model
#05.07: <pop.demog.8> same as pop.demog.7
#05.08: <syphilis.diagnoses.1> same as pop.demog.8 with syphilis
#05.08: <syphilis.diagnoses.2> same as syphilis.diagnoses.1, using total by stage
#05.09: <syphilis.diagnoses.3.pk> adding more transmission params for msm, het and by race, calibrating to total diagnosis by stage
#05.12: <syphilis.diagnoses.4.pk> adding time4 transmission params for msm, het, setting likelihoods to total and marginals for syphilis diagnosis
#>> couldnt fit well, we are going to work on the total diagnosis first before moving to marginals
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

