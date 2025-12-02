cat("*** Running Shiled_register_calibration.R ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

N.ITER=15000
# shield.solver = create.solver.metadata(rtol = 0.001, atol=0.03) #rtol,atol
# default.solver= create.solver.metadata()

#STAGE0: The demographic stage
register.calibration.info('calib.12.02.stage0', 
                          likelihood.instructions = lik.inst.stage0,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  
                          parameter.names = c(POPULATION.PARAMETERS.PRIOR@var.names,
                                              AGING.PARAMETERS.PRIOR@var.names,
                                              "global.transmission.rate"),
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
)

#STAGE1: 
register.calibration.info('calib.12.02.stage1',
                              preceding.calibration.codes = 'calib.12.02.stage0',
                              likelihood.instructions = lik.inst.stage1,
                              data.manager = SURVEILLANCE.MANAGER,
                              end.year = 2030,
                              parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                                  TESTING.PARAMETERS.PRIOR@var.names),
                              n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
    )

#STAGE2
register.calibration.info('calib.12.02.stage2',
                              preceding.calibration.codes = 'calib.12.02.stage1',
                              likelihood.instructions = lik.inst.stage2,
                              data.manager = SURVEILLANCE.MANAGER,
                              end.year = 2030,
                              parameter.names = c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                                  TESTING.PARAMETERS.PRIOR@var.names,
                                                  AGE.TRANS.TEST.PARAMETERS.PRIOR@var.names,
                                                  "age.mixing.sd.mult"
                                                  ),
                              n.iter = N.ITER, thin = 50, is.preliminary = T, max.run.time.seconds = 30, description = "NA"
    ) 


     
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