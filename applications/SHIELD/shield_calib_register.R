cat("*** Running Shiled_register_calibration.R ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

N.ITER=15000
solver = create.solver.metadata(rtol = 0.001, atol=0.05) #rtol,atol
# solver = create.solver.metadata() #default solver


param.names.demog<-c(POPULATION.PARAMETERS.PRIOR@var.names,
                           AGING.PARAMETERS.PRIOR@var.names
                          )

param.names.all<-c(POPULATION.PARAMETERS.PRIOR@var.names,
                   AGING.PARAMETERS.PRIOR@var.names,
                   TRANSMISSION.PARAMETERS.PRIOR@var.names,
                   TESTING.PARAMETERS.PRIOR@var.names)

# Calibrating to demographic and syphilis diagnoses targets
register.calibration.info('calib.demog.06.09.pk', 
                          likelihood.instructions = likelihood.instructions.demographics,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = param.names.demog, 
                          n.iter = N.ITER,
                          thin = 50, 
                          is.preliminary = T, 
                          max.run.time.seconds = 30, 
                          description = "A quick run to get population parameters in the general vicinity",
                          solver.metadata = solver
)
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
#############
# calibrating to demographics, total ps and total EL syphilis diagnosis targets and hiv tests 
# register.calibration.info(code = "calib.diagnosis.06.09.pk", 
#                           #preceding.calibration.codes = "pop.demog.8",
#                           likelihood.instructions = likelihood.instructions.syphilis.diagnoses.psElTotal,  
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = 2030, 
#                           param.names.trans.demog, 
#                           n.iter = N.ITER,
#                           thin = 50, 
#                           is.preliminary = T,
#                           max.run.time.seconds = 30,
#                           description = "A quick run to get syphilis parameters in the general vicinity",
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

# 6.09: <calib.demog.06.09.pk>: running a demographic calibration to fit the targets with 1970 start date
# <syphilis.6.09.rf.psElTotal> revising the symp.testing logic. Now it occurs during the stages 