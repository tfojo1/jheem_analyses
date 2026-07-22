source("../jheem_analyses/applications/SHIELD/shield_base_parameters.R")
source("../jheem_analyses/applications/SHIELD/inputs/input_estimate_sexual_activity_by_age.R")
source("../jheem_analyses/applications/SHIELD/R/shield_multivariate_spline_prior.R")

# source("../jheem_analyses/applications/SHIELD/inputs/input_syphilis_misclassification_error.R")
# source("../jheem_analyses/applications/SHIELD/inputs/input_prop_symp_primary.R")

#Notes:
# We use this distribution to model uncertainty in changes to event odds.
# We assume the odds ratio is most likely between half and double the prior odds.
# Lognormal.Distribution(meanlog = log(1), sdlog = 0.5*log(2)), # 95% CI= (0.5, 1.9) 

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

create.auto.regressive.covariance.matrix = function(correlation.coefficient,n,sd){
    delta = matrix(rep(1:n,n)-rep(1:n,each=n),nrow=n)
    corr.matrix = correlation.coefficient^abs(delta)
    corr.matrix*(sd^2)
}

# Defining the calibration parameters and prior distributions

#***** PARAMETER PRIORS *****----
## POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
    # Fertility rates ----
    # (6 agegroups, 3 race, 2 knots)-> max 36 params
    # we start with 6 age, and 3 race, parameters applied to both knots -> 9 total
    # Race-level fertility rate multipliers
    black.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    hispanic.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    other.fertility.rate.multiplier    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # Age-level fertility rate multipliers
    age15.19.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    age20.24.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    age25.29.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    age30.34.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    age35.39.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    age40.44.fertility.rate.multiplier = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
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
    ## Initial infections in 1970 ----
    # because we don't have true infection counts, we estimated from reported diagnosis in 1970
    # we assume a certain ratio of undiagnosed to diagnosed cases in 1970
    # we also assume a certain proportion of infections were made among MSM, and the rest are divided among heterosexual men and women
    ratio.of.undiagnosed.to.diagnosed.1970 = Lognormal.Distribution(meanlog = log(3), sdlog = 0.5*log(2)), #CI=[1.5, 5.9]
    prp.infections.among.msm.1970 = Logitnormal.Distribution(meanlogit = log(.5), sdlogit = log(2)), #CI=[0.13 0.66] larger SD because of uncertainty
    
    ## Global transmission ----
    # Moved back down to 2.2 from 3.1 when raised msm logmean baseline back to log(3) from log(1)
    global.transmission.rate.msm = Lognormal.Distribution(meanlog = log(2.3), sdlog = 0.5*log(10)), # large SD to allow more mixing
    global.transmission.rate.het = Lognormal.Distribution(meanlog = log(2.3), sdlog = 0.5*log(10)), # large SD to allow more mixing
    
    # Transmission multipliers 
    # we built a joint prior for: transmission.rate.multiplier.msm & transmission.rate.multiplier.heterosexual
    # each one has a baseline value in year 2000, and changes over spline years:    1970, 1990, 1995, 2000, 2010, 2022
    # The baseline transmission rates at year 2000 are specified separately for MSM and heterosexuals (independant)
    # The spline-point values within each group are linked through shared latent baseline/delta structure.
    # The two groups are correlated at the same spline points through the correlation parameter.
    # we assume that trate can change by 1.5 over 10 years :logsd(Delta10)=log(1.5)
    # this means that over 5 years, trate can change by sqrt(1.5) and by 20 years, it can change by 1.5^2
    make.joint.mv.spline.prior(
        parameters = paste0("transmission.rate.multiplier.", c("msm", "heterosexual")),
        logmean.baseline = c(log(3), #msm
                             log(1)), #het
        logsd.baseline = c(log(2)*2, #msm
                           log(2)*2
        ), #het
        logsd.deltas.past = c("1970" = 0.5*log(1.5^2), #20-year delta
                              "1990" = 0.5*log(sqrt(1.5)), #5-year delta
                              "1995" = 0.5*log(sqrt(1.5))), #5-year delta
        logsd.deltas.future = c("2010" = 0.5*log(1.5), #10-year delta
                                "2022" = 0.5*log(1.5)),#10-year delta
        spline.times = c("1970", "1990", "1995", "2000", "2010", "2022"),
        correlation = 0.7
    ),
    
    ## race multipliers (not sex stratified anymore) ----
    # increased SD to allow more variation CI= [0.25 - 3.9] 
    # transmission.rate.multiplier.black.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # transmission.rate.multiplier.black.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # #
    # transmission.rate.multiplier.hispanic.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # transmission.rate.multiplier.hispanic.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # #
    # transmission.rate.multiplier.other.msm= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    # transmission.rate.multiplier.other.heterosexual= Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.black = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    transmission.rate.multiplier.other = Lognormal.Distribution(meanlog = 0, sdlog = log(2)),
    
    ## future change ----
    transmission.rate.future.change.mult = Normal.Distribution(mean = 0.75, sd=0.25, lower = 0), #CI=(0.25 - 1.25) #assumption
    
    ## Sexual Mixing by Age ----
    #directly used in specification helper function to control the standard deviation of the contact matrix by age
    age.mixing.sd.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #the model is sensitive to this parameter-we reduce the sdlog=1/4*log(2)
    
    ## Sexual Mixing by Race ----
    #this is multiplied in the race mixing matrix
    black.black.sexual.multi = Lognormal.Distribution(meanlog = log(4), sdlog = log(2)), #Mu and SD are chosen empirically 
    hispanic.hispanic.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(2)),  
    other.other.sexual.multi = Lognormal.Distribution(meanlog =  log(4), sdlog = log(2)),
    
    ## Sexual Mixing by Risk ----
    # see shield_inputManager_pairing.R
    oe.female.pairings.with.msm = Lognormal.Distribution(meanlog = log(.3), sdlog = log(2)/2), #range [pathela 0.12 -dodge 0.6]
    fraction.heterosexual_male.pairings.with.male = Logitnormal.Distribution(meanlogit = logit(0.004), sdlogit = log(2)), 
    fraction.msm.pairings.with.female = Logitnormal.Distribution(meanlogit = logit(0.1187612), sdlogit = log(2)),
    
    # Proportion MSM ----
    black.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.125*log(2)), #reduced SD to limit deviation from mean (since we are not formally calibrating this)
    hispanic.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.125*log(2)),
    other.proportion.msm.of.male.mult = Lognormal.Distribution(meanlog =0, sdlog = 0.125*log(2)),
    
    # relapse & infectiousness for EL ----  
    prop.early.latent.to.secondary=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2)),# CI= [0.079 0.56] larger SD to reflect lack of confidence in both parameters
    el.rel.secondary.transmissibility=Logitnormal.Distribution(meanlogit = logit(.25), sdlogit = log(2))
) 

## STI.TESTING.PARAMETERS.PRIOR ----
STI.TESTING.PARAMETERS.PRIOR=join.distributions( 
    
    ## Fraction Symptomatic ----
    #  inputs/input_prop_symp_primary.R
    ## Data source: Study of MSM followed at PrEP clinics: Proportion of incident syphilis presenting with symptomatic primary 25% or secondary at 16% disease
    # >> we use this to inform the prior for MSM and het_male
    # >> for female, we compute the ratio_group = (primary diagnoses) / (primary + secondary diagnoses) as a proxy for proportion of 
    # primary disease that is symptomatic and use ratio_female/ratio_male to compute females relative to MSM (see input_prop_symp_primary.R ~ 0.66)
    ## Primary Stage by Sex 
    prp.symptomatic.primary.msm=Logitnormal.Distribution(meanlogit = logit(0.25),sdlogit = 0.5*log(2)), 
    #relative ratio of female & het_male to MSM
    rr.prp.symptomatic.primary.female=Logitnormal.Distribution(meanlogit = logit( 0.66), sdlogit = 0.5*log(2)),# data range is from .57-0.81, which is close to this interval (0.49-.79)
    rr.prp.symptomatic.primary.heterosexual_male=Lognormal.Distribution(meanlog = log(1),sdlog = 0.5*log(1.2)), #we manually set the sd so that the interval ranges from 0.8-1.2
    
    ## Secondary stage (total) assuming a single parameter accross all groups
    prp.symptomatic.secondary=Logitnormal.Distribution(meanlogit = logit(0.16),sdlogit = 0.5*log(2)), 
    
    ## Careseeking among Symptomatic cases (by sex and race) ---- #a Logistic Linear function (Intercept by race and sex, Slope)
    or.careseeking.symptomatic.ps.msm = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    or.careseeking.symptomatic.ps.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    or.careseeking.symptomatic.ps.female = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    #
    or.careseeking.symptomatic.ps.black = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    or.careseeking.symptomatic.ps.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    or.careseeking.symptomatic.ps.other = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2) ) ,
    # Changing the slope for everyone (We avoid sampling slopes across many dimensions at once, because small changes in the slope
    # can lead to very large increases in the odds ratio)
    or.slope.careseeking.symptomatic.ps = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10), #smaller sd for slopes 
    
    ## COVID ----
    # sti.screening: 5 alphas (leaving out age for now) #'@Melissa: why log? not logit?
    # parameter: max.covid.effect.sti.screening.reduction
    black.sti.screening.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    hispanic.sti.screening.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    other.sti.screening.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    heterosexual.sti.screening.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    msm.sti.screening.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    ## STI Screening ----
    
    #OPTION1: using logistic linear function with an intercept and slope
    # or.sti.screening.msm = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.sti.screening.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.sti.screening.female = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # #
    # or.sti.screening.black = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.sti.screening.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.sti.screening.other = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # #
    # or.slope.sti.screening.msm = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10),
    # or.slope.sti.screening.heterosexual = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10),
    
    #OPTION2: using a linear spline function (knots 1990,2000,2010,2020)  
    # for msm seperately
    screening.rate.multiplier.msm.1990 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.msm.2000 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.msm.2010 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.msm.2020 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # for heterosexuals
    screening.rate.multiplier.heterosexuals.1990 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.heterosexuals.2000 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.heterosexuals.2010 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.heterosexuals.2020 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # multipliers for female and het male:
    screening.rate.multiplier.female = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
    # race specific multipliers that apply to both msm and het
    screening.rate.multiplier.black = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    screening.rate.multiplier.other = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
    # future change multiplier
    screening.rate.future.change.mult  = Normal.Distribution(mean = 0.75, sd=0.25, lower = 0), #CI=(0.25 - 1.25) #assumption
    
    
    ## Syphilis to HIV Testing Ratio ----
    # Stratify intercept by race and sex: I think that this is creating an identifiability problem
    # weu have two sets of subgroup multipliers — one on the screening rate, one on the ratio — but only one calibration target (BRFSS HIV testing proportions by subgroup). 
    # The model cannot distinguish whether a subgroup's high HIV testing rate reflects higher syphilis screening or a higher ratio of HIV testing to syphilis screening. 
    # The two sets of parameters are interchangeable in fitting the data, so neither is uniquely estimable.
    
    # or.syphilis.to.hiv.testing.msm = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.syphilis.to.hiv.testing.heterosexual_male = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.syphilis.to.hiv.testing.female = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # #
    # or.syphilis.to.hiv.testing.black = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.syphilis.to.hiv.testing.hispanic = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # or.syphilis.to.hiv.testing.other = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    #
    or.syphilis.to.hiv.testing = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    or.slope.syphilis.to.hiv.testing = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10), # changed from 2 to make change slower
    
    ## Misclassification Error ----
    # see inputs/input_syphilis_misclassification_error.R
    fraction.el.misclassified.ll =Logitnormal.Distribution(meanlogit = logit(0.096), sdlog =  log(2)) , #CI=[0.025 0.27]
    fraction.ll.misclassified.el =Logitnormal.Distribution(meanlogit = logit(0.27), sdlog =  log(2)) #CI=[0.064 0.51]
)

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
msm_sexualActivity_sdlog <- 0.5*log(2)     
# Heterosexual_sexualActivity (these are female values)
# het_sexualActivity_means <- ESTIAMTED IN input_estimate_sexual_activity_by_age.R 
het_sexualActivity_meanlog <- log(het_sexualActivity_means)
het_sexualActivity_sdlog <- 0.5*log(2)     
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
                                                                    sdlog = 0.5*log(10)), #widened the prior to allow a wider search (emperically) CI=[0.001 - 0.1]
    
    # The other agegroups are tied together through a MVN distribution 
    TRANSMISSION.AGE.MSM.PRIOR <- Multivariate.Lognormal.Distribution( 
        mu = msm_sexualActivity_meanlog,
        sigma = msm_sexualActivity_sigma,
        var.names = msm_varnames
    ),
    
    ## HET Sexual activity ----
    transmission.rate.multiplier.age14.heterosexual = Lognormal.Distribution(meanlog = log(0.01), 
                                                                             sdlog = 0.5*log(10)),
    TRANSMISSION.AGE.HET.PRIOR <- Multivariate.Lognormal.Distribution(
        mu = het_sexualActivity_meanlog,
        sigma = het_sexualActivity_sigma,
        var.names = het_varnames
    )
)

## PRENATAL.PARAMETER.PRIOR ----
PRENATAL.PARAMETERS.PRIOR=join.distributions(
    # First‑trimester 
    first.trimester.intercept.mult      = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    first.trimester.slope.mult          = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10), 
    
    # black.first.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # hispanic.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # other.first.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # age15.19.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age20.24.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age25.29.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age30.34.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age35.39.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age40.44.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
    # Second‑trimester 
    second.trimester.intercept.mult     = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), 
    second.trimester.slope.mult         = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10),
    
    # black.second.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # hispanic.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # other.second.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # age15.19.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age20.24.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age25.29.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age30.34.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age35.39.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age40.44.second.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # Third‑trimester 
    third.trimester.intercept.mult      = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    third.trimester.slope.mult          = Lognormal.Distribution(meanlog = 0, sdlog = (0.5*log(2))/10)
    
    # black.third.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # hispanic.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # other.third.trimester.odds.mult     = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # age15.19.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age20.24.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age25.29.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age30.34.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age35.39.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age40.44.third.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    
    # ## non fertile ages ##
    # # First‑trimester
    # age0.14.first.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age45.49.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age50.54.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age55.64.first.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age65.plus.first.trimester.odds.mult= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # # Second‑trimester
    # age0.14.second.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age45.49.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age50.54.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age55.64.second.trimester.odds.mult  = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age65.plus.second.trimester.odds.mult= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # 
    # # Third‑trimester
    # age0.14.third.trimester.odds.mult    = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age45.49.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age50.54.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age55.64.third.trimester.odds.mult   = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
    # age65.plus.third.trimester.odds.mult = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
    
)

# *********************************************************************************************************************************************************************
#***** LINKING PARAMETERS TO FUNCTIONAL FORMS *****  -----
# *********************************************************************************************************************************************************************
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
    for(time in c("1970","1990","1995","2000","2010","2022")){   
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
                                                       # values = parameters[c("transmission.rate.multiplier.black.msm",
                                                       #                       "transmission.rate.multiplier.hispanic.msm", 
                                                       #                       "transmission.rate.multiplier.other.msm")],
                                                       values = parameters[c("transmission.rate.multiplier.black",
                                                                             "transmission.rate.multiplier.hispanic", 
                                                                             "transmission.rate.multiplier.other")],
                                                       dimension = "race.to", #recipient
                                                       applies.to.dimension.values = c("black","hispanic", "other"))
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "transmission.rate.heterosexual",
                                                       alpha.name = time,
                                                       # values = parameters[c("transmission.rate.multiplier.black.heterosexual",
                                                       #                       "transmission.rate.multiplier.hispanic.heterosexual", 
                                                       #                       "transmission.rate.multiplier.other.heterosexual")],
                                                       values = parameters[c("transmission.rate.multiplier.black",
                                                                             "transmission.rate.multiplier.hispanic", 
                                                                             "transmission.rate.multiplier.other")],
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
    # Future change multiplier ----
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
    
    ## COVID multipliers ----
    # # Sex/risk 
    # set.element.functional.form.interaction.alphas(model.settings,
    #                                                element.name = "max.covid.effect.sti.screening.reduction",
    #                                                alpha.name = "value",
    #                                                value = parameters['heterosexual.sti.screening.covid.multiplier'],
    #                                                applies.to.dimension.values=list(sex = c("female","heterosexual_male"))
    # )
    # set.element.functional.form.interaction.alphas(model.settings,
    #                                                element.name = "max.covid.effect.sti.screening.reduction",
    #                                                alpha.name = "value",
    #                                                value = parameters['msm.sti.screening.covid.multiplier'],
    #                                                applies.to.dimension.values=list(sex = c("msm"))
    # )
    # # race
    # set.element.functional.form.interaction.alphas(model.settings,
    #                                                element.name = "max.covid.effect.sti.screening.reduction",
    #                                                alpha.name = "value",
    #                                                values = parameters[paste0(races,'.sti.screening.covid.multiplier')],
    #                                                dimension = 'race',
    #                                                applies.to.dimension.values = races
    # )
    # Sex/risk 
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sti.screening.reduction",
                                                   alpha.name = "value",
                                                   value = parameters['heterosexual.sti.screening.covid.multiplier'],
                                                   dimension = "sex",
                                                   applies.to.dimension.values= c("female","heterosexual_male")
    )
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sti.screening.reduction",
                                                   alpha.name = "value",
                                                   value = parameters['msm.sti.screening.covid.multiplier'],
                                                   dimension = "sex",
                                                   applies.to.dimension.values="msm"
    )
    # race
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sti.screening.reduction",
                                                   alpha.name = "value",
                                                   values = parameters[paste0(races,'.sti.screening.covid.multiplier')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values = races
    )
    
    ## Symptomatic Testing ---- 
    # Logit Linear function 
    # changes in intercept by sex
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "prob.careseek.if.symptomatic.ps",
                                                   alpha.name = "intercept",
                                                   values = parameters[paste0("or.careseeking.symptomatic.ps.", sexes)],
                                                   dimension = "sex", 
                                                   applies.to.dimension.values = sexes)
    # changes in intercept by race
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "prob.careseek.if.symptomatic.ps",
                                                   alpha.name = "intercept",
                                                   values = parameters[paste0("or.careseeking.symptomatic.ps.", races)],
                                                   dimension = "race",  
                                                   applies.to.dimension.values = races)
    # changing the slope
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "prob.careseek.if.symptomatic.ps",
                                                   alpha.name = "slope",
                                                   values = parameters["or.slope.careseeking.symptomatic.ps"],
                                                   dimension = "all", 
                                                   applies.to.dimension.values = "all") 
    
    ## STI Screening  ----
    # # OPTION1: Change intercept and slope 
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                element.name = "rate.sti.screening.over.14.without.covid",
    #                                                alpha.name = "intercept",
    #                                                values = parameters[paste0("or.sti.screening.", races)],
    #                                                dimension = "race", #recipient
    #                                                applies.to.dimension.values = races)
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                element.name = "rate.sti.screening.over.14.without.covid",
    #                                                alpha.name = "intercept",
    #                                                values = parameters[paste0("or.sti.screening.", sexes)],
    #                                                dimension = "sex", #recipient
    #                                                applies.to.dimension.values = sexes)
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                element.name = "rate.sti.screening.over.14.without.covid",
    #                                                alpha.name = "slope",
    #                                                values = parameters["or.slope.sti.screening.msm"],
    #                                                dimension = "sex", #recipient
    #                                                applies.to.dimension.values = c("msm"))
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                # element.name = "rate.sti.screening.over.14",
    #                                                element.name = "rate.sti.screening.over.14.without.covid",
    #                                                alpha.name = "slope",
    #                                                values = parameters["or.slope.sti.screening.heterosexual"],
    #                                                dimension = "sex", #recipient
    #                                                applies.to.dimension.values = c("heterosexual_male", "female"))
    #OPTION2:
    for(time in c("1990","2000","2010","2020")){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "rate.sti.screening.over.14.without.covid",
                                                       alpha.name = time,
                                                       values = parameters[paste0("screening.rate.multiplier.msm.",time)],
                                                       dimension = 'sex',
                                                       applies.to.dimension.values = "msm")
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "rate.sti.screening.over.14.without.covid",
                                                       alpha.name = time,
                                                       values = parameters[paste0("screening.rate.multiplier.heterosexuals.",time)],
                                                       dimension = 'sex',
                                                       applies.to.dimension.values = c("female","heterosexual_male"))
    }
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "rate.sti.screening.over.14.without.covid",
                                                   alpha.name = time,
                                                   values = parameters[paste0("screening.rate.multiplier.female")],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = c("female"))
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "rate.sti.screening.over.14.without.covid",
                                                   alpha.name = time,
                                                   values = parameters[paste0("screening.rate.multiplier.heterosexual_male")],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = c("heterosexual_male"))
    for(race in races){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "rate.sti.screening.over.14.without.covid",
                                                       alpha.name = time,
                                                       values = parameters[paste0("screening.rate.multiplier.",race)],
                                                       dimension = 'race',
                                                       applies.to.dimension.values = race)
    }
    
    
    # Future change multiplier ----
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "rate.sti.screening.over.14.without.covid",
                                                   alpha.name = "after.modifier",
                                                   values = parameters["screening.rate.future.change.mult"],
                                                   applies.to.dimension.values = "all",
                                                   dimension = "all"
    )
    
    ## Syphilis to HIV tests Ratio ----
    # Change intercept and slope
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                element.name = "ratio.syphilis.screening.to.hiv.tests",
    #                                                alpha.name = "intercept",
    #                                                values = parameters[paste0("or.syphilis.to.hiv.testing.", sexes)],
    #                                                dimension = "sex", #recipient
    #                                                applies.to.dimension.values = sexes)
    # set.element.functional.form.main.effect.alphas(model.settings,
    #                                                element.name = "ratio.syphilis.screening.to.hiv.tests",
    #                                                alpha.name = "intercept",
    #                                                values = parameters[paste0("or.syphilis.to.hiv.testing.", races)],
    #                                                dimension = "race", #recipient
    #                                                applies.to.dimension.values = races)
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "ratio.syphilis.screening.to.hiv.tests",
                                                   alpha.name = "intercept",
                                                   values = parameters["or.syphilis.to.hiv.testing"],
                                                   dimension = "all", #recipient
                                                   applies.to.dimension.values = "all")
    
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
    
}



# *********************************************************************************************************************************************************************
#***** SAMPLING BLOCKS ***** ----
#### *********************************************************************************************************************************************************************
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
    global.transmission.rate=c("global.transmission.rate.msm",
                               "global.transmission.rate.het"),
    #
    age.mixing.sd.mult=("age.mixing.sd.mult"),
    #
    relapse=c("prop.early.latent.to.secondary"),
    #
    infectiousness=c("el.rel.secondary.transmissibility"),
    #
    prp.inf.msm.1970=c("prp.infections.among.msm.1970"),
    #
    inf.to.diag.1970=c("ratio.of.undiagnosed.to.diagnosed.1970"),
    #
    msm.transmission.block1 = c(
        "transmission.rate.multiplier.msm1970",
        "transmission.rate.multiplier.msm1990",
        "transmission.rate.multiplier.msm1995"),
    msm.transmission.block2=c(
        "transmission.rate.multiplier.msm2000",
        "transmission.rate.multiplier.msm2010",
        "transmission.rate.multiplier.msm2022"  
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
        "transmission.rate.multiplier.heterosexual2022"
        #"transmission.rate.multiplier.heterosexual2030"
    ),
    #
    # race.transmission.msm = c(
    #     "transmission.rate.multiplier.black.msm",
    #     "transmission.rate.multiplier.hispanic.msm",
    #     "transmission.rate.multiplier.other.msm"
    # ),
    # race.transmission.heterosexual = c(
    #     "transmission.rate.multiplier.black.heterosexual",
    #     "transmission.rate.multiplier.hispanic.heterosexual",
    #     "transmission.rate.multiplier.other.heterosexual"
    # ),
    race.transmission = c(
        "transmission.rate.multiplier.black",
        "transmission.rate.multiplier.hispanic",
        "transmission.rate.multiplier.other"
    ),
    #
    trans.rate.future.change=c("transmission.rate.future.change.mult"),
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
    covid.screening.red.sex=c(
        "heterosexual.sti.screening.covid.multiplier",
        "msm.sti.screening.covid.multiplier"),
    covid.screening.red.race=c("black.sti.screening.covid.multiplier",
                               "hispanic.sti.screening.covid.multiplier",
                               "other.sti.screening.covid.multiplier"),
    #
    prp.sym.ps=c(
        "prp.symptomatic.primary.msm",
        "rr.prp.symptomatic.primary.female",
        "rr.prp.symptomatic.primary.heterosexual_male",
        "prp.symptomatic.secondary"
    ),
    #
    or.careseeking.sym.sex=c(
        "or.careseeking.symptomatic.ps.msm",
        "or.careseeking.symptomatic.ps.heterosexual_male",
        "or.careseeking.symptomatic.ps.female"
    ),
    or.careseeking.sym.race.slope=c(
        "or.careseeking.symptomatic.ps.black",
        "or.careseeking.symptomatic.ps.hispanic",
        "or.careseeking.symptomatic.ps.other",
        "or.slope.careseeking.symptomatic.ps"
    ),
    #OPTION1
    # or.sti.screening.sex<-c(
    #     'or.sti.screening.msm',
    #     'or.sti.screening.heterosexual_male',
    #     'or.sti.screening.female'),
    # or.sti.screening.race.slope<-c(
    #     'or.sti.screening.black',
    #     'or.sti.screening.hispanic',
    #     'or.sti.screening.other',
    #     # 'or.slope.sti.screening',
    #     'or.slope.sti.screening.msm',
    #     'or.slope.sti.screening.heterosexual'
    # ),
    #OPTION2
    sti.screening.sex1<- c(
        "screening.rate.multiplier.msm.1990",
        "screening.rate.multiplier.msm.2000",
        "screening.rate.multiplier.msm.2010",
        "screening.rate.multiplier.msm.2020"
    ),
    sti.screening.sex2<- c(
        "screening.rate.multiplier.heterosexuals.1990",
        "screening.rate.multiplier.heterosexuals.2000",
        "screening.rate.multiplier.heterosexuals.2010",
        "screening.rate.multiplier.heterosexuals.2020"
    ),
    sti.screening.sex3<-c(
        "screening.rate.multiplier.female",
        "screening.rate.multiplier.heterosexual_male"
    ),
    sti.screening.race1<-c(
        "screening.rate.multiplier.black" ,
        "screening.rate.multiplier.hispanic",
        "screening.rate.multiplier.other"
    ),
    sti.screening.future.change<-c(
        "screening.rate.future.change.mult"
    ),
    
    #
    # syphilis.to.hiv.testing.ratio.sex<-c(
    #     # "or.syphilis.to.hiv.testing.msm",
    #     "or.syphilis.to.hiv.testing.heterosexual_male",
    #     "or.syphilis.to.hiv.testing.female",
    # ),
    # syphilis.to.hiv.testing.ratio.race<-c(
    #     "or.syphilis.to.hiv.testing.black",
    #     "or.syphilis.to.hiv.testing.hispanic",
    #     "or.syphilis.to.hiv.testing.other"
    # ),
    #
    syphilis.to.hiv.testing.ratio.sex.slope<-c(
        "or.syphilis.to.hiv.testing",
        "or.slope.syphilis.to.hiv.testing"
    ),
    misclas.error<-c(
        "fraction.el.misclassified.ll",
        "fraction.ll.misclassified.el"
    )
)

## AGE.TRANS.TEST.SAMPLING.BLOCKS ----
TRANS.BY.AGE.SAMPLING.BLOCKS = list(
    age.transmission.young<-c(
        "transmission.rate.multiplier.age14.msm",
        "transmission.rate.multiplier.age14.heterosexual"
    ),
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


# ****************************************************************************************************************************************************************
# SUMMARIZE ---- 
# ****************************************************************************************************************************************************************
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
    
    PRENATAL.SAMPLING.BLOCKS
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
