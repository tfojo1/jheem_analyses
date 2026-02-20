source("applications/SHIELD/shield_base_parameters.R")
source("applications/SHIELD/inputs/input_estimate_sexual_activity_by_age.R")

#Notes:
# We use this distribution to model uncertainty in changes to event odds.
# We assume the odds ratio is most likely between half and double the prior odds.
# Lognormal.Distribution(meanlog = log(1), sdlog = log(2)/2), # 95% CI= (0.5, 1.9) 

# When using a log-linear or logit-linear model, we can express changes in terms of odds ratios
# for both the intercept and the slope.
# In practice, it is generally recommended to apply this formulation to the intercept.
# We avoid sampling slopes across many dimensions at once, because small changes in the slope
# can lead to very large increases in the odds ratio.
# ************************************************************************************************************************
# Helpul command: #get.intervals(variable name): Get intervals (confidence/credible intervals) for the variables in a distribution
# HELPER FUNCTIONS ----

logit = function(p){
  log(p) - log(1-p)
}

# helper function to build multivariate normal distribution
make.mv.spline.prior = function(parameter, 
                                logmean00, logsd00, #mean and standard deviation for the parameter corresponding to the year 2000
                                logsd.delta95, logsd.delta90, logsd.delta70, #SD for past changes (delta) in the param values between knots to 2000
                                logsd.delta10, logsd.delta17 #SD for future changes (delta) in the param values between knots to 2000
){
  
  untransformed.mu = c(logmean00,0,0,0,0,0) #the initial expectation is that the change between years is zero
  #diagonal covariance matrix for the initial parameters. Initial parameters are assumed to be independent.
  #The variances for the changes (logsd.delta...) determine the expected magnitude of the annual changes.
  untransformed.sigma = diag(c(logsd00,
                               logsd.delta95, 
                               logsd.delta90, 
                               logsd.delta70, 
                               logsd.delta10, 
                               logsd.delta17))
  
  #trasformation matrix: transforming the initial, independent parameters (baseline value + deltas) to the 
  # final, correlated parameters (the value of the parameter at each of the six years)
  # This structure allows to model knot values by accumulating changes (deltas) from baseline year
  # P2000=baseline
  # P1995=P2000+deltas???  #'@Ryan: can you review notes to see how the knots depend on each other. 
  # Each delta is a log-change between successive knots.
  # We use an accumulation matrix M as a shortcut to find covariances
  M = rbind(
    c(1,1,1,1,0,0), # 1970 # log P_1970 = logsd00 + logsd.delta95 + logsd.delta90 + logsd.delta70
    c(1,1,1,0,0,0), # 1990 # log P_1990 = logsd00 + logsd.delta95 + logsd.delta90 
    c(1,1,0,0,0,0), # 1995 # log P_1995 = logsd00 + logsd.delta95 
    c(1,0,0,0,0,0), # 2000 # log P_2000 = logsd00 
    c(1,0,0,0,1,0), # 2010 # log P_2010 = logsd00 + logsd.delta10 
    c(1,0,0,0,1,1) # 2017 # log P_2020 = logsd00 + logsd.delta10 + logsd.delta20 
  ) # 2000, 1970, 1995, 1990, 2010, 2020
  
  mu = M %*% untransformed.mu # E[y] = M E[x]
  sigma = M  %*% untransformed.sigma %*% t(M) # Σ_y = M Σ_x Mᵀ 
  
  # Let:
  #   y_i = sum_k M[i,k] * x_k
  #   y_j = sum_l M[j,l] * x_l
  #
  # Then the covariance between y_i and y_j is:
  #   Cov(y_i, y_j) = sum_{k,l} M[i,k] * M[j,l] * Cov(x_k, x_l)
  #
  # Since Σ_x is diagonal (the x_k are independent),
  # all Cov(x_k, x_l) = 0 unless k = l. Therefore:
  #
  #   Cov(y_i, y_j) = sum_{k shared by i and j} M[i,k] * M[j,k] * σ_k^2
  
  #
  dist = Multivariate.Lognormal.Distribution(mu = mu, sigma = sigma, 
                                             var.names = paste0(parameter,c(1970,1990,1995,2000,2010,2017)))
  return(dist)
}

create.auto.regressive.covariance.matrix = function(correlation.coefficient,n,sd){
  delta = matrix(rep(1:n,n)-rep(1:n,each=n),nrow=n)
  corr.matrix = correlation.coefficient^abs(delta)
  corr.matrix*(sd^2)
}

#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

#***** PARAMETER PRIORS *****----
## POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
  # Fertility rates ----
  # (6 agegroups, 3 race, 2 knots)-> max 36 params
  # we start with 6 age, and 3 race, parameters applied to both knots -> 9 total
  # Race-level fertility rate multipliers
  black.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  hispanic.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  other.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  # Age-level fertility rate multipliers
  age15.19.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age20.24.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age25.29.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age30.34.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age35.39.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  age40.44.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
  
  # Mortality rates ----
  # By Race:
  black.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  # by Sex:
  male.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
  female.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)),
  
  # Immigration by race ----
  black.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.immigration.rate.multiplier.1= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  black.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.immigration.rate.multiplier.2= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
)
## AGING.PARAMETERS.PRIOR ----
AGING.PARAMETERS.PRIOR=join.distributions( 
  # By age, race, sex for 2 knots:
  age14.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.black.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.hispanic.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.other.aging.rate.multiplier.1=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  age14.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.black.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.hispanic.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  #
  age14.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age19.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age24.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age29.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age34.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age39.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age44.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age49.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age54.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  age64.other.aging.rate.multiplier.2=Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
)

## TRANSMISSION.PARAMETERS.PRIOR ----
TRANSMISSION.PARAMETERS.PRIOR=join.distributions( 
  ## Initial diagnosis 1970 by sex/risk group ----
  ps.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),#'@Ryan: why are we using these mu?
  ps.diagnoses.heterosexual.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  el.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),#'@Ryan: why are we using these mu
  el.diagnoses.heterosexual.multiplier.1970 = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
  
  #'@PKASAIE
  lu.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),#'@Ryan: why are we using these mu
  lu.diagnoses.heterosexual.multiplier.1970 = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
  
  
  ## Global transmission ----
  global.transmission.rate = Lognormal.Distribution(meanlog = log(2.2), sdlog = log(10)/2), #'@Ryan: why are we using these mu/sd?
  
  #12 independant params
  ## msm multipliers by time ----
  make.mv.spline.prior(parameter = "transmission.rate.multiplier.msm", #relative to heterosexuals
                       logmean00 = log(3),logsd00 = log(2), #reference year 2000 # 3X transmission assumption based on https://pmc.ncbi.nlm.nih.gov/articles/PMC11307151
                       #
                       logsd.delta95 = log(sqrt(1.5))/2, #'@Ryan: why are we using these values?
                       logsd.delta90 = log(sqrt(1.5))/2, 
                       logsd.delta70 = log(1.5^2)/2,
                       logsd.delta10 = log(1.5)/2, 
                       logsd.delta17 = log(1.5)/2 # change this to 2017
  ),
  
  ## heterosexual multipliers by time ----
  make.mv.spline.prior(parameter = "transmission.rate.multiplier.heterosexual", 
                       logmean00 = 0,logsd00 = log(2), ##'@Ryan: why are we using this SD?
                       logsd.delta95 = log(sqrt(1.5))/2, 
                       logsd.delta90 = log(sqrt(1.5))/2, 
                       logsd.delta70 = log(1.5^2)/2,
                       logsd.delta10 = log(1.5)/2, 
                       logsd.delta17 = log(1.5)/2 # change this to 2017
  ),
  
  ## race multipliers (msm and het seperatly) ----
  #'@Ryan: we have used sdlog= log(2)/2 everywhere else.why increasing here?
  transmission.rate.multiplier.black.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  transmission.rate.multiplier.black.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  
  transmission.rate.multiplier.hispanic.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  transmission.rate.multiplier.hispanic.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  
  transmission.rate.multiplier.other.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  transmission.rate.multiplier.other.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
  
  ## future change ----
  transmission.rate.future.change.mult = Normal.Distribution(mean = 0.75, sd=0.25, lower = 0), #@Ryan: please cite these numbers 
  
  ## Sexual Mixing by Age ----
  age.mixing.sd.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #the model is sensitive to this parameter-we reduce the sdlog=1/4*log(2)
  #directly used in specification helper function
  #to control the standard deviation of the contact matrix by age
  
  ## Sexual Mixing by Race ----
  #this is multiplied in the race mixing matrix
  black.black.sexual.multi = Lognormal.Distribution(meanlog = log(4), sdlog = log(2)), #Mu and SD are chosen empirically 
  hispanic.hispanic.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(2)),  
  other.other.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(2)),
  
  ## Sexual Mixing by Risk ----
  oe.female.pairings.with.msm = Lognormal.Distribution(meanlog = log(0.0895), sdlog = log(2)), #SD are chosen empirically '@Ryan: Mu?
  fraction.heterosexual_male.pairings.with.male = Logitnormal.Distribution(meanlogit = logit(0.004), sdlogit = log(2)),
  fraction.msm.pairings.with.female = Logitnormal.Distribution(meanlogit = logit(0.1187612), sdlogit = log(2)),
  
  # Proportion MSM ----#'@:Ryan: where are these mu/sd coming from? 
  black.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog =0, sdlog = 0.125*log(2)),
  hispanic.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.125*log(2)),
  other.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog =0, sdlog = 0.125*log(2)),
  
  # relapse & infectiousness for EL ---- '@:Ryan: where are these mu/sd coming from? 
  prop.early.latent.to.secondary=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2) ),# get.intervals(prop.early.latent.to.secondary)
  el.rel.secondary.transmissibility=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2) )
) 

## STI.TESTING.PARAMETERS.PRIOR ----
STI.TESTING.PARAMETERS.PRIOR=join.distributions( 
  ## Proportion Symptomatic (Testing) ----
  # Stratify intercept by race and sex 
  #'@Andrew: it doesnt make sense to me to seperate this by stage
  # 
  or.symptomatic.ps.msm = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,
  or.symptomatic.ps.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,
  or.symptomatic.ps.female = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,
  #
  or.symptomatic.ps.black = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,
  or.symptomatic.ps.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,
  or.symptomatic.ps.other = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2 ) ,

  # Changing the slope for everyone (We avoid sampling slopes across many dimensions at once, because small changes in the slope
  # can lead to very large increases in the odds ratio)
  or.slope.symptomatic.ps = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/10), 
  
  ## STI Screening ----
  # Stratify intercept by race and sex
  or.sti.screening.msm = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.sti.screening.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.sti.screening.female = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  #
  or.sti.screening.black = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.sti.screening.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.sti.screening.other = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # Changing the slope for everyone
  or.slope.sti.screening = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/10),
  
  
  ## Syphilis to HIV Testing Ratio ----
  # Stratify intercept by race and sex
  or.syphilis.to.hiv.testing.msm = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.syphilis.to.hiv.testing.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.syphilis.to.hiv.testing.female = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  #
  or.syphilis.to.hiv.testing.black = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.syphilis.to.hiv.testing.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  or.syphilis.to.hiv.testing.other = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  #
  or.slope.syphilis.to.hiv.testing = Lognormal.Distribution(meanlog = 0, sdlog = (log(1.25)/2)/10) # changed from 2 to make change slower
)

###--------------------------------------------------------------------------###
## TRANS.BY.AGE.SAMPLING.PRIOR ----
# changing transmission function by age
# we assume an auto regressive (AR) structure between agegroups 
# 1- Parameters:
#See estimate_sexual_activity_by_age for calculation of sexualActivity mean priors: 
#Helpers for sexual activity by age and risk
age_labels <- c("19","24","29","34","39","44","49","54","64","65")
n_ages <- length(age_labels) 
#see inputs/estimate_sexual_activity_by_age.R
# MSM_sexualActivity (male values in the survey)
# msm_sexualActivity_means <- ESTIAMTED IN input_estimate_sexual_activity_by_age.R 
msm_sexualActivity_meanlog <- log(msm_sexualActivity_means)
msm_sexualActivity_sdlog <- 0.5 * log(2)     
# Heterosexual_sexualActivity (these are female values)
# het_sexualActivity_means <- ESTIAMTED IN input_estimate_sexual_activity_by_age.R 
het_sexualActivity_meanlog <- log(het_sexualActivity_means)
het_sexualActivity_sdlog <- 0.5 * log(2)     
# AR parameters 
rho_age <- 0.7  # p: correlation between adjacent ages
# Covariance matrices
msm_sexualActivity_sigma <- create.auto.regressive.covariance.matrix(
  correlation.coefficient = rho_age,
  n = n_ages,
  sd = msm_sexualActivity_sdlog
)
het_sexualActivity_sigma <- create.auto.regressive.covariance.matrix(
  correlation.coefficient = rho_age,
  n = n_ages,
  sd = het_sexualActivity_sdlog
)
# 2- Define variable names 
msm_varnames <- paste0("transmission.rate.multiplier.age", age_labels, ".msm")
het_varnames <- paste0("transmission.rate.multiplier.age", age_labels, ".heterosexual")
# 3-Build the prior
TRANS.BY.AGE.SAMPLING.PRIOR=join.distributions(
  ## MSM Sexual activity ----
  #Transmission multiplier for age 0-14: we have a seperate parameter for this agegroup 
  transmission.rate.multiplier.age14.msm = Lognormal.Distribution(meanlog = log(0.01), #pure assumption: sexual activity relative to 20-24 (peak)
                                                                  sdlog = 0.5 * log(8)), #widened the prior to allow a wider search (emperically)
  
  # The other agegroups are tied together through a MVN distribution 
  TRANSMISSION.AGE.MSM.PRIOR <- Multivariate.Lognormal.Distribution( 
    mu = msm_sexualActivity_meanlog,
    sigma = msm_sexualActivity_sigma,
    var.names = msm_varnames
  ),
  
  ## HET Sexual activity ----
  transmission.rate.multiplier.age14.heterosexual = Lognormal.Distribution(meanlog = log(1e-2), sdlog = 0.5 * log(8)),
  TRANSMISSION.AGE.HET.PRIOR <- Multivariate.Lognormal.Distribution(
    mu = het_sexualActivity_meanlog,
    sigma = het_sexualActivity_sigma,
    var.names = het_varnames
  )
)
###--------------------------------------------------------------------------###
## //TESTING.BY.AGE.PARAMETERS.PRIOR ----
# A similar framework to model changes in STI screening by age #'Andrew: should we model this for sympomatic testing too?

# 1- Define the params
# testing_meanlog <- c(0, 0, 0, 0,0, 0, 0, 0, 0, 0)
# testing_sdlog <- 0.5 * log(2)
# testing_sigma <- create.auto.regressive.covariance.matrix(
#   correlation.coefficient = rho_age,
#   n = n_ages,
#   sd = testing_sdlog
# )
# 2-variable names
# screening_varnames <- paste0("sti.screening.multiplier.age", age_labels)
# symptomatic_varnames <- paste0("or.symptomatic.age", age_labels)
# 3-make the prior
# TESTING.BY.AGE.PARAMETERS.PRIOR=join.distributions( 
## STI.Screening 
# sti.screening.multiplier.age14 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
# SCREENING.AGE.PRIOR <- Multivariate.Lognormal.Distribution(
#   mu = testing_meanlog,
#   sigma = testing_sigma,
#   var.names = screening_varnames
# ),

## Symptomatic Testing 
# or.symptomatic.age14 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5 * log(2)),
# SYMPTOMATIC.PRIOR <- Multivariate.Lognormal.Distribution(
#   mu = testing_meanlog,
#   sigma = testing_sigma,
#   var.names = symptomatic_varnames
# )

## PRENATAL.PARAMETER.PRIOR ----
PRENATAL.PARAMETERS.PRIOR=join.distributions(
  # First‑trimester 
  first.trimester.intercept.mult      = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  first.trimester.slope.mult          = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/5), 
  
  # black.first.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # hispanic.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # other.first.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # age15.19.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age20.24.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age25.29.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age30.34.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age35.39.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age40.44.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  
  # Second‑trimester 
  second.trimester.intercept.mult     = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2), 
  second.trimester.slope.mult         = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/5),
  
  # black.second.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # hispanic.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # other.second.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # age15.19.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age20.24.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age25.29.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age30.34.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age35.39.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age40.44.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # Third‑trimester 
  third.trimester.intercept.mult      = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  third.trimester.slope.mult          = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/5)
  
  # black.third.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # hispanic.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # other.third.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # age15.19.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age20.24.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age25.29.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age30.34.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age35.39.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age40.44.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  
  # ## non fertile ages ##
  # # First‑trimester
  # age0.14.first.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age45.49.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age50.54.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age55.64.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age65.plus.first.trimester.odds.mult= Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # # Second‑trimester
  # age0.14.second.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age45.49.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age50.54.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age55.64.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age65.plus.second.trimester.odds.mult= Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # 
  # # Third‑trimester
  # age0.14.third.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age45.49.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age50.54.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age55.64.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
  # age65.plus.third.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2)
  
)

## DOXY-PEP.PARAMETERS.PRIOR ----
# 
# DOXYPEP.PARAMETERS.PRIOR = join.distributions(
#   
#   # Logistic slope for doxy coverage after 2022
#   # logit(coverage(t)) = doxy.coverage.slope * (t - 2022), t >= 2022
#   doxy.coverage.slope = Lognormal.Distribution(
#     meanlog = log(1), sdlog  = 0.5*log(2)),
#   
#   # Luetkemeyer et al. 2025 https://pubmed.ncbi.nlm.nih.gov/40147465/
#   # Relative risk under doxy-PEP (multiplicative reduction in acquisition)
#   doxy.rr = Lognormal.Distribution(
#     meanlog = log(0.20),   # prior mean RR ~0.20 (80% reduction)
#     sdlog  = 0.4570899     # chosen to match 95% CI [0.08 - 0.48]
#   )
# )

#### ----
#***** LINKING PARAMETERS TO FUNCTIONAL FORMS *****  -----
SHIELD.APPLY.PARAMETERS.FN = function(model.settings, parameters ){ 
  ages=model.settings$specification.metadata$dim.names$age
  sexes=model.settings$specification.metadata$dim.names$sex
  races=model.settings$specification.metadata$dim.names$race
  
  fertile.ages=model.settings$specification.metadata$dim.names$age[2:7]
  fertile.age.ranges= c("15.19","20.24","25.29","30.34","35.39","40.44") 
  #buckets of aging from:
  q=model.settings$specification.metadata$age.upper.bounds
  aging.from=q[1: (length(q)-1)]-1
  
  ## Aging Rates ----
  #10 (ages) * 3 (races) * 3 sexes= 90 for 2 knots = 180
  for(i in c(1,2)){ #spline with 2 knots
    for(age.index in 1:length(aging.from)) {
      for(race in races){
        # for(s in sexes){
        agegroup=ages[age.index]
        paramName=paste0("age",aging.from[age.index],".",race,".aging.rate.multiplier.",i)
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = "rate.general.aging",
                                                       alpha.name = paste0("time",i),
                                                       value = parameters[paramName],
                                                       applies.to.dimension.values =list(age = agegroup, race = race))
      }}}                                      
  
  ## Fertility rates by race to time1/time2 knots----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time1",
                                                 values = parameters[paste0(races, ".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time2",
                                                 values = parameters[paste0(races, ".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  # Fertility multipliers by age to time1/time2 knots ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time1",
                                                 values = parameters[paste0("age", fertile.age.ranges, ".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = "time2",
                                                 values = parameters[paste0("age", fertile.age.ranges, ".fertility.rate.multiplier")],
                                                 dimension = "age",
                                                 applies.to.dimension.values = fertile.ages)
  
  ## Immigration rate multipliers by race for time1/time2 knots ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.immigration",
                                                 alpha.name = "time.1",
                                                 values = parameters[paste0(races,".immigration.rate.multiplier.1")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.immigration",
                                                 alpha.name = "time.2",
                                                 values = parameters[paste0(races,".immigration.rate.multiplier.2")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
  ## Emigration coefficients  by race for time1/time2 knots ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.emigration",
                                                 alpha.name = "time.1",
                                                 values = 1/parameters[paste0(races,".immigration.rate.multiplier.1")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.emigration",
                                                 alpha.name = "time.2",
                                                 values = 1/parameters[paste0(races,".immigration.rate.multiplier.2")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by race ----
  races=model.settings$specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,".general.mortality.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by sex ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters["male.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('heterosexual_male','msm'))
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.general.mortality",
                                                 alpha.name = 'value',
                                                 values = parameters["female.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('female'))
  
  ## Proportion MSM by race ----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = 'prp.msm.of.male',
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races, '.proportion.msm.of.male.mult')],
                                                 applies.to.dimension.values = races,
                                                 dimension = 'race')
  
  
  ## Transmission ----
  #multipliers for msm rates in each knot:
  for(time in c("1970","1990","1995","2000","2010","2017")){   # change this to 2017 
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = time,
                                                   values = parameters[paste0("transmission.rate.multiplier.msm",time)],
                                                   dimension = 'all',
                                                   applies.to.dimension.values = 'all')
    #multipliers for heterosexual rates in each knot:
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.heterosexual",
                                                   alpha.name = time,
                                                   values = parameters[paste0("transmission.rate.multiplier.heterosexual",time)],
                                                   dimension = 'all',
                                                   applies.to.dimension.values = 'all')
    
    
    #race multipliers, shared for msm and heterosexuals: 
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = time,
                                                   values = parameters[c("transmission.rate.multiplier.black.msm",
                                                                         "transmission.rate.multiplier.hispanic.msm", 
                                                                         "transmission.rate.multiplier.other.msm")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic", "other"))
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.heterosexual",
                                                   alpha.name = time,
                                                   values = parameters[c("transmission.rate.multiplier.black.heterosexual",
                                                                         "transmission.rate.multiplier.hispanic.heterosexual", 
                                                                         "transmission.rate.multiplier.other.heterosexual")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic", "other"))
    
    
    #age multipliers:
    agegroups = c("14", "19","24", "29", "34", "39", "44", "49", "54", "64", "65")
    paramName.msm =paste0("transmission.rate.multiplier.age",agegroups, ".msm")
    paramName.heterosexual =paste0("transmission.rate.multiplier.age",agegroups, ".heterosexual")
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.msm",
                                                   alpha.name = time,
                                                   values = parameters[paramName.msm],
                                                   dimension = "age.to", #recipient
                                                   applies.to.dimension.values = ages)
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "transmission.rate.heterosexual",
                                                   alpha.name = time,
                                                   values = parameters[paramName.heterosexual],
                                                   dimension = "age.to", #recipient
                                                   applies.to.dimension.values = ages)
  }
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "transmission.rate.heterosexual",
                                                 alpha.name = "after.modifier",
                                                 values = parameters["transmission.rate.future.change.mult"],
                                                 applies.to.dimension.values = "all",
                                                 dimension = "all"
  )
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "transmission.rate.msm",
                                                 alpha.name = "after.modifier",
                                                 values = parameters["transmission.rate.future.change.mult"],
                                                 applies.to.dimension.values = "all",
                                                 dimension = "all"
  )
  
  
  ## Symptomatic Testing ----
  # changes in intercept by sex
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.primary",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.symptomatic.ps.", sexes)],
                                                 dimension = "sex", #recipient
                                                 applies.to.dimension.values = sexes)
  # changes in intercept by race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.primary",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.symptomatic.ps.", races)],
                                                 dimension = "race", #recipient
                                                 applies.to.dimension.values = races)
  # same changes for secondary:
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.secondary",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.symptomatic.ps.", sexes)],
                                                 dimension = "sex", #recipient
                                                 applies.to.dimension.values = sexes)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.secondary",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.symptomatic.ps.", races)],
                                                 dimension = "race", #recipient
                                                 applies.to.dimension.values = races)
  # changing the slope
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.primary",
                                                 alpha.name = "slope",
                                                 values = parameters["or.slope.symptomatic.ps"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "prp.symptomatic.secondary",
                                                 alpha.name = "slope",
                                                 values = parameters["or.slope.symptomatic.ps"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  ## STI Screening  ----
  # Change intercept and slope 
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.sti.screening.over.14",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.sti.screening.", races)],
                                                 dimension = "race", #recipient
                                                 applies.to.dimension.values = races) 
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.sti.screening.over.14",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.sti.screening.", sexes)],
                                                 dimension = "sex", #recipient
                                                 applies.to.dimension.values = sexes)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "rate.sti.screening.over.14",
                                                 alpha.name = "slope",
                                                 values = parameters["or.slope.sti.screening"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  
  ## Syphilis to HIV tests Ratio ----
  # Change intercept and slope
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "ratio.syphilis.screening.to.hiv.tests",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.syphilis.to.hiv.testing.", sexes)],
                                                 dimension = "sex", #recipient
                                                 applies.to.dimension.values = sexes)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "ratio.syphilis.screening.to.hiv.tests",
                                                 alpha.name = "intercept",
                                                 values = parameters[paste0("or.syphilis.to.hiv.testing.", races)],
                                                 dimension = "race", #recipient
                                                 applies.to.dimension.values = races)

  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "ratio.syphilis.screening.to.hiv.tests",
                                                 alpha.name = "slope",
                                                 values = parameters["or.slope.syphilis.to.hiv.testing"],
                                                 dimension = "all", #recipient
                                                 applies.to.dimension.values = "all")
  
  # Prenatal care ----
  trimesters <- list(
    list(element = "prp.prenatal.care.first.trimester",  prefix = "first.trimester"),
    list(element = "prp.prenatal.care.second.trimester.of.those.not.screened.first", prefix = "second.trimester"),
    list(element = "prp.prenatal.care.third.trimester.of.those.not.screened.first.second",  prefix = "third.trimester")
  )
  for (tr in trimesters) {
    elem <- tr$element
    pre  <- tr$prefix
    
    ## global OR 
    set.element.functional.form.main.effect.alphas(
      model.settings, 
      element.name = elem, 
      alpha.name = "intercept",
      values = parameters[paste0(pre, ".intercept.mult")],
      dimension = "all",
      applies.to.dimension.values = "all")
    
    ##  global slope 
    set.element.functional.form.main.effect.alphas(
      model.settings, 
      element.name = elem, 
      alpha.name = "slope",
      values = parameters[paste0(pre, ".slope.mult")],
      dimension = "all",
      applies.to.dimension.values = "all")
    
  }
  
  ## Doxy-PEP Coverage
  # set.element.functional.form.main.effect.alphas(
  #   model.settings,
  #   element.name = "doxy.coverage",
  #   alpha.name   = "slope",
  #   values       = parameters["doxy.coverage.slope"],
  #   dimension    = "all",
  #   applies.to.dimension.values = "all"
  # )
  
  
}



#### ----
#***** SAMPLING BLOCKS ***** ----
# classic mcmc samples one param at a time, adaptive mcms samples multiple params (1-5 per block)

## POPULATION.SAMPLING.BLOCKS ----
POPULATION.SAMPLING.BLOCKS = list(
  # Fertility ---- 
  fertility.rates.by.race = c(
    "black.fertility.rate.multiplier",
    "hispanic.fertility.rate.multiplier",
    "other.fertility.rate.multiplier"
  ),
  fertility.rates.by.age.1 = c(
    "age15.19.fertility.rate.multiplier",
    "age20.24.fertility.rate.multiplier",
    "age25.29.fertility.rate.multiplier"
  ),
  fertility.rates.by.age.2 = c(
    "age30.34.fertility.rate.multiplier",
    "age35.39.fertility.rate.multiplier",
    "age40.44.fertility.rate.multiplier"
  ),
  # Mortality ----
  mortality.rates.by.race=c("black.general.mortality.rate.multiplier",
                            "hispanic.general.mortality.rate.multiplier",
                            "other.general.mortality.rate.multiplier"),
  moratlity.rates.by.sex=c("male.general.mortality.rate.multiplier",
                           "female.general.mortality.rate.multiplier"),
  # Immigration ----
  black.immigration = c("black.immigration.rate.multiplier.1",
                        "black.immigration.rate.multiplier.2"),
  hispanic.immigration = c("hispanic.immigration.rate.multiplier.1",
                           "hispanic.immigration.rate.multiplier.2"),
  other.immigration = c("other.immigration.rate.multiplier.1",
                        "other.immigration.rate.multiplier.2")
)
## AGING.SAMPLING.BLOCKS ---- 
AGING.SAMPLING.BLOCKS = list(
  aging.black.group1=c(
    "age14.black.aging.rate.multiplier.1",
    "age14.black.aging.rate.multiplier.2",
    "age19.black.aging.rate.multiplier.1",
    "age19.black.aging.rate.multiplier.2"),
  aging.black.group2=c(
    "age19.black.aging.rate.multiplier.1",
    "age19.black.aging.rate.multiplier.2",
    "age24.black.aging.rate.multiplier.1",
    "age24.black.aging.rate.multiplier.2"  ),
  aging.black.group3=c(
    "age24.black.aging.rate.multiplier.1",
    "age24.black.aging.rate.multiplier.2" ,
    "age29.black.aging.rate.multiplier.1",
    "age29.black.aging.rate.multiplier.2"),
  aging.black.group4=c(
    "age29.black.aging.rate.multiplier.1",
    "age29.black.aging.rate.multiplier.2",
    "age34.black.aging.rate.multiplier.1",
    "age34.black.aging.rate.multiplier.2" ),
  aging.black.group5=c(
    "age34.black.aging.rate.multiplier.1",
    "age34.black.aging.rate.multiplier.2",
    "age39.black.aging.rate.multiplier.1",
    "age39.black.aging.rate.multiplier.2"),
  aging.black.group6=c(
    "age39.black.aging.rate.multiplier.1",
    "age39.black.aging.rate.multiplier.2",
    "age44.black.aging.rate.multiplier.1",
    "age44.black.aging.rate.multiplier.2"),
  aging.black.group7=c(
    "age44.black.aging.rate.multiplier.1",
    "age44.black.aging.rate.multiplier.2",
    "age49.black.aging.rate.multiplier.1",
    "age49.black.aging.rate.multiplier.2"),
  aging.black.group8=c(
    "age49.black.aging.rate.multiplier.1",
    "age49.black.aging.rate.multiplier.2",
    "age54.black.aging.rate.multiplier.1",
    "age54.black.aging.rate.multiplier.2"),
  aging.black.group9=c(
    "age64.black.aging.rate.multiplier.1",
    "age64.black.aging.rate.multiplier.2"),
  
  ###
  aging.hispanic.group1=c(
    "age14.hispanic.aging.rate.multiplier.1",
    "age14.hispanic.aging.rate.multiplier.2",
    "age19.hispanic.aging.rate.multiplier.1",
    "age19.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group2=c(
    "age19.hispanic.aging.rate.multiplier.1",
    "age19.hispanic.aging.rate.multiplier.2",
    "age24.hispanic.aging.rate.multiplier.1",
    "age24.hispanic.aging.rate.multiplier.2"  ),
  aging.hispanic.group3=c(
    "age24.hispanic.aging.rate.multiplier.1",
    "age24.hispanic.aging.rate.multiplier.2" ,
    "age29.hispanic.aging.rate.multiplier.1",
    "age29.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group4=c(
    "age29.hispanic.aging.rate.multiplier.1",
    "age29.hispanic.aging.rate.multiplier.2",
    "age34.hispanic.aging.rate.multiplier.1",
    "age34.hispanic.aging.rate.multiplier.2" ),
  aging.hispanic.group5=c(
    "age34.hispanic.aging.rate.multiplier.1",
    "age34.hispanic.aging.rate.multiplier.2",
    "age39.hispanic.aging.rate.multiplier.1",
    "age39.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group6=c(
    "age39.hispanic.aging.rate.multiplier.1",
    "age39.hispanic.aging.rate.multiplier.2",
    "age44.hispanic.aging.rate.multiplier.1",
    "age44.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group7=c(
    "age44.hispanic.aging.rate.multiplier.1",
    "age44.hispanic.aging.rate.multiplier.2",
    "age49.hispanic.aging.rate.multiplier.1",
    "age49.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group8=c(
    "age49.hispanic.aging.rate.multiplier.1",
    "age49.hispanic.aging.rate.multiplier.2",
    "age54.hispanic.aging.rate.multiplier.1",
    "age54.hispanic.aging.rate.multiplier.2"),
  aging.hispanic.group9=c(
    "age64.hispanic.aging.rate.multiplier.1",
    "age64.hispanic.aging.rate.multiplier.2"),
  
  ##
  aging.other.group1=c(
    "age14.other.aging.rate.multiplier.1",
    "age14.other.aging.rate.multiplier.2",
    "age19.other.aging.rate.multiplier.1",
    "age19.other.aging.rate.multiplier.2"),
  aging.other.group2=c(
    "age19.other.aging.rate.multiplier.1",
    "age19.other.aging.rate.multiplier.2",
    "age24.other.aging.rate.multiplier.1",
    "age24.other.aging.rate.multiplier.2"  ),
  aging.other.group3=c(
    "age24.other.aging.rate.multiplier.1",
    "age24.other.aging.rate.multiplier.2" ,
    "age29.other.aging.rate.multiplier.1",
    "age29.other.aging.rate.multiplier.2"),
  aging.other.group4=c(
    "age29.other.aging.rate.multiplier.1",
    "age29.other.aging.rate.multiplier.2",
    "age34.other.aging.rate.multiplier.1",
    "age34.other.aging.rate.multiplier.2" ),
  aging.other.group5=c(
    "age34.other.aging.rate.multiplier.1",
    "age34.other.aging.rate.multiplier.2",
    "age39.other.aging.rate.multiplier.1",
    "age39.other.aging.rate.multiplier.2"),
  aging.other.group6=c(
    "age39.other.aging.rate.multiplier.1",
    "age39.other.aging.rate.multiplier.2",
    "age44.other.aging.rate.multiplier.1",
    "age44.other.aging.rate.multiplier.2"),
  aging.other.group7=c(
    "age44.other.aging.rate.multiplier.1",
    "age44.other.aging.rate.multiplier.2",
    "age49.other.aging.rate.multiplier.1",
    "age49.other.aging.rate.multiplier.2"),
  aging.other.group8=c(
    "age49.other.aging.rate.multiplier.1",
    "age49.other.aging.rate.multiplier.2",
    "age54.other.aging.rate.multiplier.1",
    "age54.other.aging.rate.multiplier.2"),
  aging.other.group9=c(
    "age64.other.aging.rate.multiplier.1",
    "age64.other.aging.rate.multiplier.2")
)

## TRANSMISSION.SAMPLING.BLOCKS ----
TRANSMISSION.SAMPLING.BLOCKS = list(
  global.transmission.rate=c("global.transmission.rate"),
  #
  relapse=c("prop.early.latent.to.secondary"),
  #
  infectiousness=c("el.rel.secondary.transmissibility"),
  #
  initial.infections.msm=c(
    'ps.diagnoses.msm.multiplier.1970',
    'el.diagnoses.msm.multiplier.1970',
    'lu.diagnoses.msm.multiplier.1970'
  ),
  initial.infections.Heterosexual=c(
    'ps.diagnoses.heterosexual.multiplier.1970',
    'el.diagnoses.heterosexual.multiplier.1970',
    'lu.diagnoses.heterosexual.multiplier.1970'
  ),
  #
  msm.transmission.block1 = c(
    "transmission.rate.multiplier.msm1970",
    "transmission.rate.multiplier.msm1990",
    "transmission.rate.multiplier.msm1995"),
  msm.transmission.block2=c(
    "transmission.rate.multiplier.msm2000",
    "transmission.rate.multiplier.msm2010",
    "transmission.rate.multiplier.msm2017"#, # rename this and others to 2017
    #"transmission.rate.multiplier.msm2030"
  ),
  #
  het.transmission.block1 =c(
    "transmission.rate.multiplier.heterosexual1970",
    "transmission.rate.multiplier.heterosexual1990",
    "transmission.rate.multiplier.heterosexual1995"),
  het.transmission.block2=c(
    "transmission.rate.multiplier.heterosexual2000",
    "transmission.rate.multiplier.heterosexual2010",
    "transmission.rate.multiplier.heterosexual2017"#,
    #"transmission.rate.multiplier.heterosexual2030"
  ),
  #
  race.transmission.msm = c(
    "transmission.rate.multiplier.black.msm",
    "transmission.rate.multiplier.hispanic.msm",
    "transmission.rate.multiplier.other.msm"
  ),
  race.transmission.heterosexual = c(
    "transmission.rate.multiplier.black.heterosexual",
    "transmission.rate.multiplier.hispanic.heterosexual",
    "transmission.rate.multiplier.other.heterosexual"
  ),
  #
  future.change.trate=c("transmission.rate.future.change.mult"),
  #
  race.mixing.transmission= c("black.black.sexual.multi",
                              "hispanic.hispanic.sexual.multi", 
                              "other.other.sexual.multi"
  ),
  risk.mixing.transmission= c("oe.female.pairings.with.msm",
                              "fraction.heterosexual_male.pairings.with.male",
                              "fraction.msm.pairings.with.female"),
  #
  proportion.msm.of.male = c(
    'black.proportion.msm.of.male.mult',
    'hispanic.proportion.msm.of.male.mult',
    'other.proportion.msm.of.male.mult')
  
)

## STI.TESTING.SAMPLING.BLOCKS ----
STI.TESTING.SAMPLING.BLOCKS = list(
  prp.sympt.ps.sex.slope = c(
    "or.symptomatic.ps.msm",
    "or.symptomatic.ps.heterosexual_male",
    "or.symptomatic.ps.female",
    "or.slope.symptomatic.ps"
  ),
  #
  prp.sympt.ps.race = c(
    "or.symptomatic.ps.black",
    "or.symptomatic.ps.hispanic",
    "or.symptomatic.ps.other"
  ),
  #
  sti.testing.sex.slope = c(
    "or.sti.screening.msm",
    "or.sti.screening.heterosexual_male",
    "or.sti.screening.female",
    "or.slope.sti.screening"),
  #
  sti.testing.race= c(
    "or.sti.screening.black",
    "or.sti.screening.hispanic",
    "or.sti.screening.other"),
  #
  
  
  syphilis.to.hiv.testing.ratio.sex.slope<-c(
    "or.syphilis.to.hiv.testing.msm",
    "or.syphilis.to.hiv.testing.heterosexual_male",
    "or.syphilis.to.hiv.testing.female",
    "or.slope.syphilis.to.hiv.testing"
  ),
  #
  syphilis.to.hiv.testing.ratio.race<-c(
    "or.syphilis.to.hiv.testing.black",
    "or.syphilis.to.hiv.testing.hispanic",
    "or.syphilis.to.hiv.testing.other"
  )
  
)
## SD.MULT.SAMPLING.BLOCKS ----
SD.MULT.SAMPLING.BLOCKS = list(
  age.mixing.transmission=(
    "age.mixing.sd.mult"
  )
)
## AGE.TRANS.TEST.SAMPLING.BLOCKS ----
TRANS.BY.AGE.SAMPLING.BLOCKS = list(
  age.transmission.young<-c(
    "transmission.rate.multiplier.age14.msm",
    "transmission.rate.multiplier.age14.heterosexual"),
  #
  age.transmission.msm.1 = c(
    "transmission.rate.multiplier.age19.msm",
    "transmission.rate.multiplier.age24.msm",
    "transmission.rate.multiplier.age29.msm",
    "transmission.rate.multiplier.age34.msm"
  ),
  age.transmission.msm.2 = c(
    "transmission.rate.multiplier.age39.msm",
    "transmission.rate.multiplier.age44.msm",
    "transmission.rate.multiplier.age49.msm"
  ),
  age.transmission.msm.3 = c(
    "transmission.rate.multiplier.age54.msm",
    "transmission.rate.multiplier.age64.msm",
    "transmission.rate.multiplier.age65.msm"
  ),
  age.transmission.heterosexual.1 = c(
    "transmission.rate.multiplier.age19.heterosexual",
    "transmission.rate.multiplier.age24.heterosexual",
    "transmission.rate.multiplier.age29.heterosexual",
    "transmission.rate.multiplier.age34.heterosexual"
  ),
  age.transmission.heterosexual.2 = c(
    "transmission.rate.multiplier.age39.heterosexual",
    "transmission.rate.multiplier.age44.heterosexual",
    "transmission.rate.multiplier.age49.heterosexual"
  ),
  age.transmission.heterosexual.3 = c(
    "transmission.rate.multiplier.age54.heterosexual",
    "transmission.rate.multiplier.age64.heterosexual",
    "transmission.rate.multiplier.age65.heterosexual"
  )
)

## PRENATAL.SAMPLING.BLOCKS ----
PRENATAL.SAMPLING.BLOCKS=list(
  # First trimester
  pnc1.global = c("first.trimester.intercept.mult",
                  "first.trimester.slope.mult"),
  # pnc1.race   = c("black.first.trimester.odds.mult",
  #                 "hispanic.first.trimester.odds.mult",
  #                 "other.first.trimester.odds.mult"),
  # 
  # pnc1.age.A  = c("age15.19.first.trimester.odds.mult",
  #                 "age20.24.first.trimester.odds.mult",
  #                 "age25.29.first.trimester.odds.mult"),
  # 
  # pnc1.age.B  = c("age30.34.first.trimester.odds.mult",
  #                 "age35.39.first.trimester.odds.mult",
  #                 "age40.44.first.trimester.odds.mult"),
  # 
  # pnc1.age.C = c("age0.14.first.trimester.odds.mult",
  #                "age45.49.first.trimester.odds.mult",
  #                "age50.54.first.trimester.odds.mult",
  #                "age55.64.first.trimester.odds.mult",
  #                "age65.plus.first.trimester.odds.mult"),
  # 
  
  # Second trimester
  pnc2.global = c("second.trimester.intercept.mult",
                  "second.trimester.slope.mult"),
  
  # pnc2.race   = c("black.second.trimester.odds.mult",
  #                 "hispanic.second.trimester.odds.mult",
  #                 "other.second.trimester.odds.mult"),
  # 
  # pnc2.age.A  = c("age15.19.second.trimester.odds.mult",
  #                 "age20.24.second.trimester.odds.mult",
  #                 "age25.29.second.trimester.odds.mult"),
  # 
  # pnc2.age.B  = c("age30.34.second.trimester.odds.mult",
  #                 "age35.39.second.trimester.odds.mult",
  #                 "age40.44.second.trimester.odds.mult"),
  # 
  # pnc2.age.C = c("age0.14.second.trimester.odds.mult",
  #                "age45.49.second.trimester.odds.mult",
  #                "age50.54.second.trimester.odds.mult",
  #                "age55.64.second.trimester.odds.mult",
  #                "age65.plus.second.trimester.odds.mult"),
  
  # Third trimester
  pnc3.global = c("third.trimester.intercept.mult",
                  "third.trimester.slope.mult")
  
  # pnc3.race   = c("black.third.trimester.odds.mult",
  #                 "hispanic.third.trimester.odds.mult",
  #                 "other.third.trimester.odds.mult"),
  # 
  # pnc3.age.A  = c("age15.19.third.trimester.odds.mult",
  #                 "age20.24.third.trimester.odds.mult",
  #                 "age25.29.third.trimester.odds.mult"),
  # 
  # pnc3.age.B  = c("age30.34.third.trimester.odds.mult",
  #                 "age35.39.third.trimester.odds.mult",
  #                 "age40.44.third.trimester.odds.mult"),
  # 
  # pnc3.age.C = c("age0.14.third.trimester.odds.mult",
  #                "age45.49.third.trimester.odds.mult",
  #                "age50.54.third.trimester.odds.mult",
  #                "age55.64.third.trimester.odds.mult",
  #                "age65.plus.third.trimester.odds.mult")
)


## DOXY-PEP.SAMPLING.BLOCKS ----
# DOXYPEP.SAMPLING.BLOCKS = list(
#   doxy.block = c(
#     "doxy.coverage.slope",
#     "doxy.rr"
#   )
# )

#### ----
# SUMMARIZE ---- 
#these will be registered in the specification 
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  AGING.PARAMETERS.PRIOR,
  TRANSMISSION.PARAMETERS.PRIOR,
  STI.TESTING.PARAMETERS.PRIOR,
  TRANS.BY.AGE.SAMPLING.PRIOR,
  PRENATAL.PARAMETERS.PRIOR
)

SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS=c(
  POPULATION.SAMPLING.BLOCKS,
  AGING.SAMPLING.BLOCKS ,
  TRANSMISSION.SAMPLING.BLOCKS,
  STI.TESTING.SAMPLING.BLOCKS,
  TRANS.BY.AGE.SAMPLING.BLOCKS,
  PRENATAL.SAMPLING.BLOCKS,
  SD.MULT.SAMPLING.BLOCKS #we have seperated this for now.
  
)



# Notes: 
# x=Lognormal.Distribution(meanlog = log(1), sdlog = 0.5*log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,1) #penalty for drawing a point at 10 instead of 1
# 
# x=Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,3)
# 
# x=Lognormal.Distribution(meanlog = log(3), sdlog = log(2))
# get.intervals(x)
# calculate.density(x,10)/calculate.density(x,3)

cat('*** shield_calib_parameters.R completed! ***\n')


#length(SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS) = 85
#we roughly accept 1 in 4 parameter values
#average number of samples needed: 85*4=340


# if x is normally distributed with mu-0, and sd=1
# the 95% confidence interval is mu+/- 1.96*sd
# then log(x) has the confidence bound as +-log(1.96) = +-log(2)
# which means that we can estimate the sdlog as ".5 * log(2)"
# what does this mean for our paramter sampling in the log scale?
# if we exponentiate those bounds, we can estimate the interval for sampling: exp(log(2))=2. exp(-log(2))=.5
