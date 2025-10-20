source("applications/SHIELD/shield_base_parameters.R")

# Helpul command: #get.intervals(variable name): Get intervals (confidence/credible intervals) for the variables in a distribution

logit = function(p){
    log(p) - log(1-p)
}

# helper function to build multivariate normal distribution

# make.mv.spline.prior = function(parameter, logmean00, logsd00, 
#                                 logsd.delta95, logsd.delta90, logsd.delta70,
#                                 logsd.delta10, logsd.delta20, logsd.delta30){
#     
#     M = rbind(
#         c(1,1,1,1,0,0,0),
#         c(1,1,1,0,0,0,0),
#         c(1,1,0,0,0,0,0),
#         c(1,0,0,0,0,0,0),
#         c(1,0,0,0,1,0,0),
#         c(1,0,0,0,1,1,0),
#         c(1,0,0,0,1,1,1)
#     )
#     untransformed.mu = c(logmean00,0,0,0,0,0,0)
#     untransformed.sigma = diag(c(logsd00,
#                                  logsd.delta95, 
#                                  logsd.delta90, 
#                                  logsd.delta70, 
#                                  logsd.delta10, 
#                                  logsd.delta20,
#                                  logsd.delta30))
#     
#     mu = M %*% untransformed.mu
#     sigma = M  %*% untransformed.sigma %*% t(M)
#     
#     
#     dist = Multivariate.Lognormal.Distribution(mu = mu, sigma = sigma, var.names = paste0(parameter,c(1970,1990,1995,2000,2010,2020,2030)))
#     return(dist)
# }


make.mv.spline.prior = function(parameter, logmean00, logsd00, 
                                logsd.delta95, logsd.delta90, logsd.delta70,
                                logsd.delta10, logsd.delta20){
    
    M = rbind(
        c(1,1,1,1,0,0),
        c(1,1,1,0,0,0),
        c(1,1,0,0,0,0),
        c(1,0,0,0,0,0),
        c(1,0,0,0,1,0),
        c(1,0,0,0,1,1)
    )
    untransformed.mu = c(logmean00,0,0,0,0,0)
    untransformed.sigma = diag(c(logsd00,
                                 logsd.delta95, 
                                 logsd.delta90, 
                                 logsd.delta70, 
                                 logsd.delta10, 
                                 logsd.delta20))
    
    mu = M %*% untransformed.mu
    sigma = M  %*% untransformed.sigma %*% t(M)
    
    
    dist = Multivariate.Lognormal.Distribution(mu = mu, sigma = sigma, var.names = paste0(parameter,c(1970,1990,1995,2000,2010,2020)))
    return(dist)
}


#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

#1- PARAMETER PRIORS:----

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
    ## Initial diagnosis multipliers in 1970 by sex/risk group
    ps.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),
    ps.diagnoses.heterosexual_male.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # ps.diagnoses.black.multiplier.1970 = Lognormal.Distribution(meanlog = log(5), sdlog = 0.5*log(2)),
    # ps.diagnoses.hispanic.multiplier.1970 = Lognormal.Distribution(meanlog = log(2), sdlog = 0.5*log(2)),
    # ps.diagnoses.other.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    #ps.diagnoses.female.multiplier.1970          = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
    el.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),
    el.diagnoses.heterosexual_male.multiplier.1970 = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
    # el.diagnoses.black.multiplier.1970 = Lognormal.Distribution(meanlog = log(5), sdlog = 0.5*log(2)),
    # el.diagnoses.hispanic.multiplier.1970 = Lognormal.Distribution(meanlog = log(2), sdlog = 0.5*log(2)),
    # el.diagnoses.other.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    #el.diagnoses.female.multiplier.1970          = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
    
    lu.diagnoses.msm.multiplier.1970             = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)),
    lu.diagnoses.heterosexual_male.multiplier.1970 = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
    # lu.diagnoses.black.multiplier.1970 = Lognormal.Distribution(meanlog = log(5), sdlog = 0.5*log(2)),
    # lu.diagnoses.hispanic.multiplier.1970 = Lognormal.Distribution(meanlog = log(2), sdlog = 0.5*log(2)),
    # lu.diagnoses.other.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    #lu.diagnoses.female.multiplier.1970          = Lognormal.Distribution(meanlog = 0.0, sdlog = 0.5*log(2)),
    
    ## Transmission
    #global.transmission.rate = Lognormal.Distribution(meanlog = 0, sdlog = 100), #directly used in specification (will need sth uch larger) 
    #global.transmission.rate = Uniform.Distribution(min = 0, max = Inf),
    global.transmission.rate = Lognormal.Distribution(meanlog = log(1.5), sdlog = log(10)/2),
    
    #12 independant params
    # msm multipliers by time
    # transmission.rate.multiplier.msm1970 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),
    # transmission.rate.multiplier.msm1990 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),
    # transmission.rate.multiplier.msm1995 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),#1995 #increasing the peak value
    # transmission.rate.multiplier.msm2000 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)), 
    # transmission.rate.multiplier.msm2010 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)), 
    # transmission.rate.multiplier.msm2020 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),
    # transmission.rate.multiplier.msm2030 = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),
    # 
    make.mv.spline.prior(parameter = "transmission.rate.multiplier.msm", logmean00 = log(3),logsd00 = log(2), 
                         logsd.delta95 = log(sqrt(1.5))/2, 
                         logsd.delta90 = log(sqrt(1.5))/2, 
                         logsd.delta70 = log(1.5^2)/2,
                         logsd.delta10 = log(1.5)/2, 
                         logsd.delta20 = log(1.5)/2#, 
                         #logsd.delta30 = log(1.5)/2
    ),
    
    
    
    # heterosexual multipliers by time
    # transmission.rate.multiplier.heterosexual1970 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # transmission.rate.multiplier.heterosexual1990 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # transmission.rate.multiplier.heterosexual1995 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),#1995 #increasing the peak value
    # transmission.rate.multiplier.heterosexual2000 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)), 
    # transmission.rate.multiplier.heterosexual2010 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)), 
    # transmission.rate.multiplier.heterosexual2020 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)), 
    # transmission.rate.multiplier.heterosexual2030 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)), 
    # 
    
    make.mv.spline.prior(parameter = "transmission.rate.multiplier.heterosexual", logmean00 = 0,logsd00 = log(2)/2, 
                         logsd.delta95 = log(sqrt(1.5))/2, 
                         logsd.delta90 = log(sqrt(1.5))/2, 
                         logsd.delta70 = log(1.5^2)/2,
                         logsd.delta10 = log(1.5)/2, 
                         logsd.delta20 = log(1.5)/2#, 
                         #logsd.delta30 = log(1.5)/2
    ),
    
    
    
    
    ### race multipliers (msm and het seperate):
    transmission.rate.multiplier.black.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.black.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    transmission.rate.multiplier.hispanic.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.hispanic.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    transmission.rate.multiplier.other.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.other.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    ### future change
    transmission.rate.future.change.mult = Normal.Distribution(mean = 0.75, sd=0.25, lower = 0),
    
    ## Sexual Mixing by Age
    age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)), #directly used in specification helper function
    #to control the standard deviation of the contact matrix by age
    
    ## Sexual Mixing by Race
    black.black.sexual.multi = Lognormal.Distribution(meanlog = log(4), sdlog = log(4)/2),
    hispanic.hispanic.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(4)/2),
    other.other.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(4)/2),
    
    ## Sexual Mixing by Risk
    oe.female.pairings.with.msm = Lognormal.Distribution(meanlog = log(0.0895), sdlog = log(4)/2),
    fraction.heterosexual_male.pairings.with.male = Logitnormal.Distribution(meanlogit = logit(0.004), sdlogit = log(2)),
    fraction.msm.pairings.with.female = Logitnormal.Distribution(meanlogit = logit(0.1187612), sdlogit = log(2)),
    
    # Proportion MSM ----
    black.proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    hispanic.proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    other.proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    
    # relapse & infectiousness for EL
    prop.early.latent.to.secondary=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2) ),# get.intervals(prop.early.latent.to.secondary)
    el.rel.secondary.transmissibility=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2) )
) 

## TESTING.PARAMETERS.PRIOR ----
TESTING.PARAMETERS.PRIOR=join.distributions( 
    # Odd-Ratio of symptomatic testing (stage X sex and time) 
    
    # or.symptomatic.primary.black.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.black.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.black.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    # or.symptomatic.primary.hispanic.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.hispanic.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.hispanic.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    # or.symptomatic.primary.other.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.other.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # or.symptomatic.primary.other.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    # or.symptomatic.secondary.black.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.black.msm               = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.black.female            = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # 
    # or.symptomatic.secondary.hispanic.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.hispanic.msm               = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.hispanic.female            = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # 
    # or.symptomatic.secondary.other.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.other.msm               = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # or.symptomatic.secondary.other.female            = Lognormal.Distribution(meanlog = 0, sdlog = 0.25*log(2)),
    # 
    # 
    
    or.symptomatic.primary.msm = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.primary.heterosexual_male = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.primary.female = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    #
    or.symptomatic.secondary.msm = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.secondary.heterosexual_male = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.secondary.female= Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ),
    #
    or.symptomatic.primary.black = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.primary.hispanic = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    or.symptomatic.primary.other = Lognormal.Distribution(meanlog = log(1), sdlog = log(2) ) ,
    #
    or.symptomatic.secondary.black = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)) ,
    or.symptomatic.secondary.hispanic = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)) ,
    or.symptomatic.secondary.other= Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    #
    or.symptomatic.1970 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    or.symptomatic.1990 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    or.symptomatic.1995 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    or.symptomatic.2000 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    or.symptomatic.2010 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    or.symptomatic.2020 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    #or.symptomatic.2030 = Lognormal.Distribution(meanlog = log(1), sdlog = log(2)),
    
    # for HIV screening
    hiv.testing.or = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    hiv.testing.slope.or = Lognormal.Distribution(meanlog = 0, sdlog = (log(2)/2)/5),
    
    # STI screening knots multiplier (relative to HIV screening)
    # sti.screening.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.1990 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.1995 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.2000 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.2010 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.2020 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.2030 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    make.mv.spline.prior(parameter = "sti.screening.multiplier." ,logmean00 = 0,logsd00 = log(2), 
                         logsd.delta95 = log(sqrt(1.5))/2, 
                         logsd.delta90 = log(sqrt(1.5))/2, 
                         logsd.delta70 = log(1.5^2)/2,
                         logsd.delta10 = log(1.5)/2, 
                         logsd.delta20 = log(1.5)/2#, 
                         #logsd.delta30 = log(1.5)/2
    ),
    
    
    
    sti.screening.future.change.mult = Normal.Distribution(mean = 0.75, sd=0.25, lower = 0),
    
    # STI screening multiplier by stage (defined in specification-no linking needed here)
    sti.screening.multiplier.ps = Lognormal.Distribution(meanlog = log(.5), sdlog = log(2)), #get.intervals(sti.screening.multiplier.ps) #most values between 0.25-0.75
    sti.screening.multiplier.el = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)), #changing the prior to reflect higher freq of screening among syphilis-infected subgroups (highrisk)
    sti.screening.multiplier.ll = Lognormal.Distribution(meanlog = log(3), sdlog = log(2)),
    sti.screening.multiplier.tertiary = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    sti.screening.multiplier.cns = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    
    # STI screening multiplier by race
    
    sti.screening.multiplier.black = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    sti.screening.multiplier.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    sti.screening.multiplier.other = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    
    # STI screening multiplier by sex
    
    sti.screening.multiplier.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    sti.screening.multiplier.msm = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    sti.screening.multiplier.female = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    
    # sti.screening.multiplier.black.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.black.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.black.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    # sti.screening.multiplier.hispanic.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.hispanic.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.hispanic.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    # sti.screening.multiplier.other.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.other.msm               = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # sti.screening.multiplier.other.female            = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # 
    
    # Contact tracing
    # prop.index.cases.reached.for.contact.tracing = 0.8 [0.3, 0.98] #I chose the sdlogit to roughly create this range
    prop.index.cases.reached.for.contact.tracing=Logitnormal.Distribution(meanlogit = logit(.8), sdlogit = log(2)*1.7 )# get.intervals(prop.index.cases.reached.for.contact.tracing)
    #prop.index.cases.reached.for.contact.tracing = Logitnormal.Distribution(meanlogit = 0, sdlogit = log(3))
    
    
    # Time varying treatment rate
    
    # treatment.multiplier.1970 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.1990 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.1995 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.2000 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.2010 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.2020 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2),
    # treatment.multiplier.2030 = Lognormal.Distribution(meanlog = 0, sdlog = log(2)/2)
)


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



#### ----
#2- LINKING PARAMETERS TO FUNCTIONAL FORMS....  -----
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
    for(time in c("1970","1990","1995","2000","2010","2020")){    
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
    
    ## STI SCREENING  ----
    # Changing the intercept and slope for HIV tests
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "rate.testing.hiv.without.covid.over.14",
                                                   alpha.name = "intercept",
                                                   values = parameters["hiv.testing.or"],
                                                   dimension = "all", #recipient
                                                   applies.to.dimension.values = "all")
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "rate.testing.hiv.without.covid.over.14",
                                                   alpha.name = "slope",
                                                   values = parameters["hiv.testing.slope.or"],
                                                   dimension = "all", #recipient
                                                   applies.to.dimension.values = "all")
    
    # Changing the knot values for ratio of STI screening to HIV tests
    for(time in c("1970","1990","1995","2000","2010","2020")){
        
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "multiplier.syphilis.screening.to.hiv.tests",
                                                       alpha.name = time,
                                                       values = parameters[paste0("sti.screening.multiplier.",time)],
                                                       dimension = "all", #recipient
                                                       applies.to.dimension.values = "all")
        # for (r in races) for (s in sexes) {
        #     set.element.functional.form.interaction.alphas(
        #         model.settings,
        #         element.name = "multiplier.syphilis.screening.to.hiv.tests",
        #         alpha.name   = time,
        #         value        = parameters[paste0("sti.screening.multiplier.", r, ".", s)],
        #         applies.to.dimension.values = list(race = r, sex = s)
        #     )
        # }
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "multiplier.syphilis.screening.to.hiv.tests",
                                                       alpha.name = time,
                                                       values = parameters[paste0("sti.screening.multiplier.",races)],
                                                       dimension = "race", #recipient
                                                       applies.to.dimension.values = races)
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "multiplier.syphilis.screening.to.hiv.tests",
                                                       alpha.name = time,
                                                       values = parameters[paste0("sti.screening.multiplier.", sexes)],
                                                       dimension = "sex", #recipient
                                                       applies.to.dimension.values = sexes)
    }
    #Note: sti.screening.multiplier.*by stage are directly linked in the specification
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "multiplier.syphilis.screening.to.hiv.tests",
                                                   alpha.name = "after.modifier",
                                                   values = parameters["sti.screening.future.change.mult"],
                                                   applies.to.dimension.values = "all",
                                                   dimension = "all"
    )
    
    # Symptomatic Testing ----
    for(time in c("1970", "1990","1995","2000","2010","2020")){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.primary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.",time)],
                                                       dimension = "all", #recipient
                                                       applies.to.dimension.values = "all")
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.primary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.primary.", sexes)],
                                                       dimension = "sex", #recipient
                                                       applies.to.dimension.values = sexes)
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.primary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.primary.", races)],
                                                       dimension = "race", #recipient
                                                       applies.to.dimension.values = races)
        # # Primary symptomatic
        # for (r in races) for (s in sexes) {
        #     set.element.functional.form.interaction.alphas(
        #         model.settings,
        #         element.name = "prp.symptomatic.primary",
        #         alpha.name   = time,
        #         value        = parameters[paste0("or.symptomatic.primary.", r, ".", s)],
        #         applies.to.dimension.values = list(race = r, sex = s)
        #     )
        # }
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.secondary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.",time)],
                                                       dimension = "all", #recipient
                                                       applies.to.dimension.values = "all")
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.secondary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.secondary.", sexes)],
                                                       dimension = "sex", #recipient
                                                       applies.to.dimension.values = sexes)
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "prp.symptomatic.secondary",
                                                       alpha.name = time,
                                                       values = parameters[paste0("or.symptomatic.secondary.", races)],
                                                       dimension = "race", #recipient
                                                       applies.to.dimension.values = races)
        # 
        # # Secondary symptomatic
        # for (r in races) for (s in sexes) {
        #     set.element.functional.form.interaction.alphas(
        #         model.settings,
        #         element.name = "prp.symptomatic.secondary",
        #         alpha.name   = time,
        #         value        = parameters[paste0("or.symptomatic.secondary.", r, ".", s)],
        #         applies.to.dimension.values = list(race = r, sex = s)
        #     )
        # }
    }
    
    
    
    
    
    
    
    
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
        
        # ## 3. race‑specific OR
        # race.parms <- paste0(races, ".", pre, ".odds.mult")
        # set.element.functional.form.main.effect.alphas(
        #     model.settings, 
        #     element.name = elem, 
        #     alpha.name = "intercept",
        #     values = parameters[race.parms],
        #     dimension = "race",
        #     applies.to.dimension.values = races)
        # 
        # ## 4. age‑specific OR
        # age.parms <- paste0("age", ages, ".", pre, ".odds.mult")
        # set.element.functional.form.main.effect.alphas(
        #     model.settings,
        #     element.name = elem,
        #     alpha.name = "intercept",
        #     values = parameters[age.parms],
        #     dimension = "age",
        #     applies.to.dimension.values = ages)
    }
    
    
}



#### ----
#3- SAMPLING BLOCKS: ----
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
    initial.infections.msm=c(
        'ps.diagnoses.msm.multiplier.1970',
        'el.diagnoses.msm.multiplier.1970',
        'lu.diagnoses.msm.multiplier.1970'
    ),
    initial.infections.Heterosexual=c(
        'ps.diagnoses.heterosexual_male.multiplier.1970',
        'el.diagnoses.heterosexual_male.multiplier.1970',
        'lu.diagnoses.heterosexual_male.multiplier.1970'
    ),
    #' initial.infections.Females=c(
    #'     'ps.diagnoses.female.multiplier.1970',
    #'     'el.diagnoses.female.multiplier.1970'#,
    #'     'lu.diagnoses.female.multiplier.1970'
    #' ),
    # initial.infections.black=c(
    #     'ps.diagnoses.black.multiplier.1970',
    #     'el.diagnoses.black.multiplier.1970',
    #     'lu.diagnoses.black.multiplier.1970'
    # ),
    # initial.infections.hispanic=c(
    #     'ps.diagnoses.hispanic.multiplier.1970',
    #     'el.diagnoses.hispanic.multiplier.1970',
    #     'lu.diagnoses.hispanic.multiplier.1970'
    # ),
    # initial.infections.other=c(
    #     'ps.diagnoses.other.multiplier.1970',
    #     'el.diagnoses.other.multiplier.1970',
    #     'lu.diagnoses.other.multiplier.1970'
    # ),
    global.transmission.rate=c("global.transmission.rate"),
    #
    msm.transmission.block1 = c(
        "transmission.rate.multiplier.msm1970",
        "transmission.rate.multiplier.msm1990",
        "transmission.rate.multiplier.msm1995"),
    msm.transmission.block2=c(
        "transmission.rate.multiplier.msm2000",
        "transmission.rate.multiplier.msm2010",
        "transmission.rate.multiplier.msm2020"#,
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
        "transmission.rate.multiplier.heterosexual2020"#,
        #"transmission.rate.multiplier.heterosexual2030"
    ),
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
    future.change.trate=c("transmission.rate.future.change.mult"),
    age.mixing.transmission=(
        "age.mixing.sd.mult"
    ),
    race.mixing.transmission= c("black.black.sexual.multi",
                                "hispanic.hispanic.sexual.multi", 
                                "other.other.sexual.multi"
    ),
    risk.mixing.transmission= c("oe.female.pairings.with.msm",
                                "fraction.heterosexual_male.pairings.with.male",
                                "fraction.msm.pairings.with.female"),
    
    proportion.msm.of.male = c(
        'black.proportion.msm.of.male.mult',
        'hispanic.proportion.msm.of.male.mult',
        'other.proportion.msm.of.male.mult'),
    relapse=c(
        "prop.early.latent.to.secondary"
    ),
    infectiousness=c(
        "el.rel.secondary.transmissibility"
    )
)

# TESTING.SAMPLING.BLOCKS ----
TESTING.SAMPLING.BLOCKS = list(
    symptomatic.testing.primary.sex = c(
        "or.symptomatic.primary.msm",
        "or.symptomatic.primary.heterosexual_male",
        "or.symptomatic.primary.female"
    ),
    symptomatic.testing.secondary.sex = c(
        "or.symptomatic.secondary.msm",
        "or.symptomatic.secondary.heterosexual_male",
        "or.symptomatic.secondary.female"
    ),
    symptomatic.testing.primary.race = c(
        "or.symptomatic.primary.black",
        "or.symptomatic.primary.hispanic",
        "or.symptomatic.primary.other"
    ),
    symptomatic.testing.secondary.race = c(
        "or.symptomatic.secondary.black",
        "or.symptomatic.secondary.hispanic",
        "or.symptomatic.secondary.other"
    ),
    # sympt.primary.race.sex.1 = c(
    #     "or.symptomatic.primary.black.heterosexual_male",
    #     "or.symptomatic.primary.black.msm",
    #     "or.symptomatic.primary.black.female"
    # ),
    # sympt.primary.race.sex.2 = c(
    #     "or.symptomatic.primary.hispanic.heterosexual_male",
    #     "or.symptomatic.primary.hispanic.msm",
    #     "or.symptomatic.primary.hispanic.female"
    # ),
    # sympt.primary.race.sex.3 = c(
    #     "or.symptomatic.primary.other.heterosexual_male",
    #     "or.symptomatic.primary.other.msm",
    #     "or.symptomatic.primary.other.female"
    # ),
    # sympt.secondary.race.sex.1 = c(
    #     "or.symptomatic.secondary.black.heterosexual_male",
    #     "or.symptomatic.secondary.black.msm",
    #     "or.symptomatic.secondary.black.female"
    # ),
    # sympt.secondary.race.sex.2 = c(
    #     "or.symptomatic.secondary.hispanic.heterosexual_male",
    #     "or.symptomatic.secondary.hispanic.msm",
    #     "or.symptomatic.secondary.hispanic.female"
    # ),
    # sympt.secondary.race.sex.3 = c(
    #     "or.symptomatic.secondary.other.heterosexual_male",
    #     "or.symptomatic.secondary.other.msm",
    #     "or.symptomatic.secondary.other.female"
    # ),
    symptomatic.testing.time1 = c(
        "or.symptomatic.1970",
        "or.symptomatic.1990",
        "or.symptomatic.1995"
    ),
    symptomatic.testing.time2 = c(
        "or.symptomatic.2000",
        "or.symptomatic.2010",
        "or.symptomatic.2020"#,
        #"or.symptomatic.2030"
    ),
    hiv.testing = c(
        "hiv.testing.or",
        "hiv.testing.slope.or"
    ),
    sti.screening.by.stage1=c(
        "sti.screening.multiplier.ps",     
        "sti.screening.multiplier.el",
        "sti.screening.multiplier.ll"
    ),
    sti.screening.by.stage2=c(
        "sti.screening.multiplier.tertiary",
        "sti.screening.multiplier.cns"
    ),    
    screening.by.time1 = c(
        "sti.screening.multiplier.1970",
        "sti.screening.multiplier.1990",
        "sti.screening.multiplier.1995"
    ),
    screening.by.time2 = c(
        "sti.screening.multiplier.2000",
        "sti.screening.multiplier.2010",
        "sti.screening.multiplier.2020"#,
        #"sti.screening.multiplier.2030"
    ),
    
    screening.by.race = c(
        "sti.screening.multiplier.black",
        "sti.screening.multiplier.hispanic",
        "sti.screening.multiplier.other"
    ),
    screening.by.sex = c(
        "sti.screening.multiplier.heterosexual_male",
        "sti.screening.multiplier.msm",
        "sti.screening.multiplier.female"
    ),
    # screening.by.sex.race1 = c(
    #     "sti.screening.multiplier.black.heterosexual_male",
    #     "sti.screening.multiplier.black.msm",
    #     "sti.screening.multiplier.black.female"
    # ),
    # screening.by.sex.race2 = c(
    #     "sti.screening.multiplier.hispanic.heterosexual_male",
    #     "sti.screening.multiplier.hispanic.msm",
    #     "sti.screening.multiplier.hispanic.female"
    # ),
    # screening.by.sex.race3 = c(
    #     "sti.screening.multiplier.other.heterosexual_male",
    #     "sti.screening.multiplier.other.msm",
    #     "sti.screening.multiplier.other.female"
    # ),
    future.change.screen=c("sti.screening.future.change.mult"),
    contact.tracing=c(
        "prop.index.cases.reached.for.contact.tracing"   
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

#### ----
# SUMMARIZE ---- 
#these will be registered in the specification 
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
    POPULATION.PARAMETERS.PRIOR,
    AGING.PARAMETERS.PRIOR,
    TRANSMISSION.PARAMETERS.PRIOR,
    TESTING.PARAMETERS.PRIOR,
    PRENATAL.PARAMETERS.PRIOR
)

SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS=c(
    POPULATION.SAMPLING.BLOCKS,
    AGING.SAMPLING.BLOCKS ,
    TRANSMISSION.SAMPLING.BLOCKS,
    TESTING.SAMPLING.BLOCKS,
    PRENATAL.SAMPLING.BLOCKS
    
)