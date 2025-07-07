cat("*** Running Shiled_register_calibration.R ***\n")
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

N.ITER=15000
# shield.solver = create.solver.metadata(rtol = 0.001, atol=0.03) #rtol,atol
# default.solver= create.solver.metadata()

#parameter set for demographic calibration
param.names.demog<-c(POPULATION.PARAMETERS.PRIOR@var.names,
                     AGING.PARAMETERS.PRIOR@var.names
)

#parameter set for diagnosis calibration
param.names.diag<-c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                    TESTING.PARAMETERS.PRIOR@var.names)

#parameter set for demographic & diagnosis calibration
param.names.all<-c(
    POPULATION.PARAMETERS.PRIOR@var.names,
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
                          description = "A quick run to get population parameters in the general vicinity"
                          # solver.metadata = shield.solver
)

#############
# calibrating to total ps; total EL diagnosis and hiv tests targets
# downweighting the likelihood
# removed relapse =0 
# removed infectiousness for EL stage =0
# removed screening for PS (muliplier set to 0)
# took out prenatal care
# took out contact tracing
register.calibration.info(code = "calib.diagnosis.06.30.pk1",
                          preceding.calibration.codes = "calib.demog.06.09.pk", #calibrated demographic model
                          likelihood.instructions = likelihood.instructions.syphilis.diag.total.no.demog, # PS total, EL total, Late total, HIV tests
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = 
                              c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N.ITER,
                          thin = 50,
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity"
                          # solver.metadata = shield.solver
)

register.calibration.info(code = "calib.07.02.rf",
                          preceding.calibration.codes = "calib.demog.06.09.pk",  
                          likelihood.instructions = likelihood.instructions.syphilis.diag.total.no.demog, # PS total, EL total, Late total, HIV tests
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = 
                              c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N.ITER,
                          thin = 50,
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity"
)
# calib.diagnosis.07.06.pk*
for (i in (1:4)){
    register.calibration.info(code = paste0("calib.diagnosis.07.06.pk",i),
                              preceding.calibration.codes = "calib.demog.06.09.pk", #calibrated demographic model
                              likelihood.instructions = likelihood.instructions.syphilis.diag.total.no.demog, # PS total, EL total, Late total, HIV tests statified
                              data.manager = SURVEILLANCE.MANAGER,
                              end.year = 2030,
                              parameter.names = 
                                  c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                    TESTING.PARAMETERS.PRIOR@var.names),
                              n.iter = N.ITER,
                              thin = 50,
                              is.preliminary = T,
                              max.run.time.seconds = 30,
                              description = "A quick run to get syphilis parameters in the general vicinity"
                              # solver.metadata = shield.solver
    )
}

#calib.diagnosis.07.07.pk*
i=1
register.calibration.info(code = paste0("calib.diagnosis.07.07.pk",i),
                          preceding.calibration.codes = "calib.diagnosis.07.06.pk4", #calibrated diagnosis model
                          likelihood.instructions = lik.inst.diag.total.no.demog, # Total Diag, PS total, EL total, Late total, HIV tests Total
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,
                          parameter.names = 
                              c(TRANSMISSION.PARAMETERS.PRIOR@var.names,
                                TESTING.PARAMETERS.PRIOR@var.names),
                          n.iter = N.ITER,
                          thin = 50,
                          is.preliminary = T,
                          max.run.time.seconds = 30,
                          description = "A quick run to get syphilis parameters in the general vicinity"
                          # solver.metadata = shield.solver
)


# LOG SUMMARY -----
# <calib.diagnosis.07.07.pk1>
# rerunning *4 from yesterday after revising the HIV likelihood to use the "TOTALS" only.


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