
##------------------------------------##
##------------------------------------##
##-- SOME UP-FRONT HELPERS / SET-up --##
##------------------------------------##
##------------------------------------##

MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = .25 # from https://www.shelbytnhealth.com/DocumentCenter/View/608/Subsequent-HIV-Disease-Risk-Following-Syphilis-Diagnosis-in-a-Southern-MSM-Population-PDF?bidId=
IDU.BASE.TRATE.MEAN = 12
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3

# The next three functions defined in ehe_parameters_helpers.R
SEXUAL.SUSCEPTIBILITY.BY.AGE = get.sexual.susceptibility.by.age(c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'))
IDU.SUSCEPTIBILITY.BY.AGE = get.idu.susceptibility.by.age(c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'))
SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR = get.msm.age.susceptibility.1.vs.2(ages = c('13-24 years', '25-34 years'),
                                                                          year1 = 2010, year2 = 2019)
AIDS.RELATIVE.SUSCEPTIBILITY.BY.AGE = get.peak.susceptibility.by.age(c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'))/
  (0.5* SEXUAL.SUSCEPTIBILITY.BY.AGE + 0.5 * IDU.SUSCEPTIBILITY.BY.AGE)

#-- SOME HELPERS WE WILL NEED --#
create.auto.regressive.covariance.matrix = function(correlation.coefficient,
                                                    n,sd){
  delta = matrix(rep(1:n,n)-rep(1:n,each=n),nrow=n)
  corr.matrix = correlation.coefficient^abs(delta)
  corr.matrix*(sd^2)
  
}

create.compound.symmetry.covariance.matrix = function(correlation.coefficient,
                                                      n,sd){

  corr.matrix = matrix(correlation.coefficient,nrow=n,ncol=n)
  diag(corr.matrix) = 1
  corr.matrix*(sd^2)
  
}

##---------------------------##
##---------------------------##
##-- MAKE THE DISTRIBUTION --##
##---------------------------##
##---------------------------##
# POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR = distributions::join.distributions(
  
    #-- Birth rates --#
    black.birth.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)), # can be off by factor of 4
    hispanic.birth.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),
    other.birth.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),
    
    #-- Non IDU general mortality rates --#
    black.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    hispanic.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    other.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    
    age1.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    age2.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    age3.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    age4.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    age5.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    
    male.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    female.non.idu.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
    
    #-- Immigration/emigration rates --#
    
    Multivariate.Lognormal.Distribution(mu=0,
                                        sigma = create.compound.symmetry.covariance.matrix(correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".immigration.multiplier.time.1")),
    
    Multivariate.Lognormal.Distribution(mu=0,
                                        sigma = create.compound.symmetry.covariance.matrix(correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".emigration.multiplier.time.1")),

    black.age1.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age1.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age1.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age2.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age2.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age2.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age3.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age3.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age3.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age4.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age4.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age4.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age5.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age5.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age5.migration.multiplier.time.1 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    
    Multivariate.Lognormal.Distribution(mu=0,
                                        sigma = create.compound.symmetry.covariance.matrix(correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".immigration.multiplier.time.2")),
    
    Multivariate.Lognormal.Distribution(mu=0,
                                        sigma = create.compound.symmetry.covariance.matrix(correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".emigration.multiplier.time.2")),
    
    black.age1.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age1.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age1.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age2.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age2.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age2.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age3.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age3.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age3.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age4.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age4.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age4.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    black.age5.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    hispanic.age5.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    other.age5.migration.multiplier.time.2 = Lognormal.Distribution(0, sdlog = 0.5 * log(2)),
    
    #-- Aging --#
    black.age1.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age1.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age1.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age1.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age1.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age1.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age2.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age2.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age2.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age2.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age2.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age2.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age3.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age3.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age3.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age3.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age3.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age3.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age4.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age4.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age4.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.age4.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age4.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.age4.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2))
)
# BASE.HIV.PARAMETERS.PRIOR ----
BASE.HIV.PARAMETERS.PRIOR = distributions::join.distributions(
    global.trate = Loguniform.Distribution(0,Inf),
    
    #-- MSM Transmission --#
    black.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='black',
                                                                    route='msm'),
    
    hispanic.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                       r1.log.sd=log(BASE.TRATE.CV),
                                                                       rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                       rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                       race='hispanic',
                                                                       route='msm'),
    
    other.msm.transmission = create.transmission.prior.distribution(r1.log.mean=log(MSM.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='other',
                                                                    route='msm'),
    
    msm.peak.trate.multiplier =  Lognormal.Distribution(log(3.1), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    
    #-- Heterosexual Transmission --#
    black.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                             r1.log.sd=log(BASE.TRATE.CV),
                                                                             rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                             rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                             race='black',
                                                                             route='heterosexual'),
    
    hispanic.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                                r1.log.sd=log(BASE.TRATE.CV),
                                                                                rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                                rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                                race='hispanic',
                                                                                route='heterosexual'),
    
    other.heterosexual.transmission = create.transmission.prior.distribution(r1.log.mean=log(HET.BASE.TRATE.MEAN),
                                                                             r1.log.sd=log(BASE.TRATE.CV),
                                                                             rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                             rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                             race='other',
                                                                             route='heterosexual'),
    
    heterosexual.peak.trate.multiplier =  Lognormal.Distribution(log(2.2), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    
    #-- IDU Transmission --#
    black.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='black',
                                                                    route='idu'),
    
    hispanic.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                       r1.log.sd=log(BASE.TRATE.CV),
                                                                       rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                       rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                       race='hispanic',
                                                                       route='idu'),
    
    other.idu.transmission = create.transmission.prior.distribution(r1.log.mean=log(IDU.BASE.TRATE.MEAN),
                                                                    r1.log.sd=log(BASE.TRATE.CV),
                                                                    rr.2.to.1.log.sd=0.5*log(TRATE.RR.1.2.SPAN),
                                                                    rr.0.to.1.log.sd=0.5*log(TRATE.RR.0.1.SPAN),
                                                                    race='other',
                                                                    route='idu'),
    
    idu.peak.trate.multiplier =  Lognormal.Distribution(log(4.7), 0.5*log(TRATE.RR.0.PEAK.SPAN)),
    
    #-- MSM-IDU Transmission --#
    
    # take the OR of borrowing needles from table 2 of 
    # https://pubmed.ncbi.nlm.nih.gov/9489050/
    # as an RR
    msm.idu.susceptibility.rr.peak = Lognormal.Distribution(log(3.3), 0.5*log(4)),
    msm.idu.susceptibility.rr.0 = Lognormal.Distribution(log(3.3), 0.5*log(4)),
    msm.idu.susceptibility.rr.1 = Lognormal.Distribution(log(3.3), 0.5*log(4)),
    msm.idu.susceptibility.rr.2 = Lognormal.Distribution(log(3.3), 0.5*log(4)),
    
    #-- Age Susceptibility --#
    
    # MSM Susceptibility by Age
    age1.black.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[1]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['black',1])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age1.black.msm.susceptibility.rr.01',
                                                                                         'age1.black.msm.susceptibility.rr.2')),
    
    age2.black.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[2]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['black',2])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age2.black.msm.susceptibility.rr.01',
                                                                                         'age2.black.msm.susceptibility.rr.2')),
    
    age1.hispanic.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[1]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['hispanic',1])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age1.hispanic.msm.susceptibility.rr.01',
                                                                                         'age1.hispanic.msm.susceptibility.rr.2')),
    
    age2.hispanic.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[2]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['hispanic',2])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age2.hispanic.msm.susceptibility.rr.01',
                                                                                         'age2.hispanic.msm.susceptibility.rr.2')),
    
    age1.other.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[1]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['other',1])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age1.other.msm.susceptibility.rr.01',
                                                                                         'age1.other.msm.susceptibility.rr.2')),
    
    age2.other.msm.susceptibility.rr = Multivariate.Lognormal.Distribution(mu = log(SEXUAL.SUSCEPTIBILITY.BY.AGE[2]) +
                                                                             c(0, log(SEXUAL.SUSCEPTIBILITY.AGE.RACE.MSM.RR['other',2])),
                                                                           sigma = (0.5 * log(2))^2 *
                                                                             matrix(c(1,0.5,0.5,1), nrow=2),
                                                                           var.names = c('age2.other.msm.susceptibility.rr.01',
                                                                                         'age2.other.msm.susceptibility.rr.2')),
    
    age3.msm.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.msm.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.msm.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    age3.msm.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.msm.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.msm.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    # Heterosexual Susceptibility by Age
    age1.heterosexual.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[1]), 0.5*log(2)),
    age2.heterosexual.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[2]), 0.5*log(2)),
    age3.heterosexual.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.heterosexual.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.heterosexual.susceptibility.rr.01 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    age1.heterosexual.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[1]), 0.5*log(2)),
    age2.heterosexual.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[2]), 0.5*log(2)),
    age3.heterosexual.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.heterosexual.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.heterosexual.susceptibility.rr.2 = Lognormal.Distribution(log(SEXUAL.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    # IDU Susceptibility by Age
    age1.idu.susceptibility.rr.01 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[1]), 0.5*log(2)),
    age2.idu.susceptibility.rr.01 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[2]), 0.5*log(2)),
    age3.idu.susceptibility.rr.01 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.idu.susceptibility.rr.01 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.idu.susceptibility.rr.01 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    age1.idu.susceptibility.rr.2 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[1]), 0.5*log(2)),
    age2.idu.susceptibility.rr.2 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[2]), 0.5*log(2)),
    age3.idu.susceptibility.rr.2 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[3]), 0.5*log(2)),
    age4.idu.susceptibility.rr.2 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.idu.susceptibility.rr.2 = Lognormal.Distribution(log(IDU.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
    # Peak Age Susceptibility Multiplier
    
    age1.susceptibility.rr.mult.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.susceptibility.rr.mult.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.susceptibility.rr.mult.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.susceptibility.rr.mult.0 = Lognormal.Distribution(log(AIDS.RELATIVE.SUSCEPTIBILITY.BY.AGE[4]), 0.5*log(2)),
    age5.susceptibility.rr.mult.0 = Lognormal.Distribution(log(AIDS.RELATIVE.SUSCEPTIBILITY.BY.AGE[5]), 0.5*log(2)),
    
   
    
    #-- HIV AGING --#
    
    # Aging - MSM with HIV 
    age1.msm.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.msm.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.msm.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.msm.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.msm.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.msm.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.msm.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.msm.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),

    age1.msm.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.msm.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.msm.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.msm.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    # Aging - Heterosexual with HIV
    age4.heterosexual.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age1.heterosexual.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.heterosexual.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.heterosexual.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.heterosexual.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.heterosexual.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.heterosexual.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.heterosexual.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.heterosexual.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.heterosexual.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.heterosexual.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.heterosexual.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),

    # Aging - IDU with HIV
    age4.idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age1.idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    # idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    # idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    # idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    # Aging by race with HIV
    black.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    
    # msm.age1.aging.base = Lognormal.Distribution(log(12209/2/22537), 0.25*log(2)),
    # msm.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    # msm.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    # msm.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    # 
    # heterosexual.age1.aging.base = Lognormal.Distribution(log((427+1861)/2/(814+3752)), 0.25*log(2)),
    # heterosexual.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    # heterosexual.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    # heterosexual.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    # 
    # idu.age1.aging.base = Lognormal.Distribution(log((180+221)/2/(218+411)), 0.25*log(2)),
    # idu.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    # idu.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    # idu.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    
    
    
    #-- Other Sex-Specific Transmission Parameters --#
    
    #products of 
    # 1) ratio of female.to.male vs male.to.female - from Maunank's paper
    # 2) ratio of condomless vaginal sex (male vs female)
    male.vs.female.heterosexual.rr = Lognormal.Distribution(log(3.75/4.75 * 87.4/92), 0.5*log(2)),
    
    #idu by sex from table 9 and 10 from
    #  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
    # RR of prob of daily use (heroin) * prob of needle sharing
    female.vs.heterosexual.male.idu.susceptibility.rr = Lognormal.Distribution(log(.777/.755*.626/.585), 0.5*log(2)),
    
    #-- HIV Testing --#
    
    # Testing by Risk
    
    proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    
    risk.proportion.tested.or = Multivariate.Lognormal.Distribution(
        mu = rep(0, 3),
        sigma = create.compound.symmetry.covariance.matrix(0.5, 3, 0.5*log(1.5)),
        var.names = paste0(c('heterosexual','idu','msm.idu'), '.proportion.tested.or')
    ),
    
    risk.proportion.tested.slope.or = Multivariate.Lognormal.Distribution(
      mu = rep(0, 3),
      sigma = create.compound.symmetry.covariance.matrix(0.5, 3, 0.5*log(1.5)/10),
      var.names = paste0(c('heterosexual','idu','msm.idu'), '.proportion.tested.slope.or')
    ),
    
    # Testing by Race
    
    race.proportion.tested.or = Multivariate.Lognormal.Distribution(
      mu = rep(0, 2),
      sigma = create.compound.symmetry.covariance.matrix(0.5, 2, 0.5*log(1.5)),
      var.names = paste0(c('black','hispanic'), ".proportion.tested.or")
    ),
    
    # race.proportion.tested.slope.or = Multivariate.Lognormal.Distribution(
    #   mu = rep(0, 2),
    #   sigma = create.compound.symmetry.covariance.matrix(0.5, 2, 0.5*log(2)/10),
    #   var.names = paste0(c('black','hispanic'), ".proportion.tested.slope.or")
    # ),

    # Testing by Age
    age1.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age2.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age3.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age4.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    age5.proportion.tested.or = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    age2.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    age3.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    age4.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    age5.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/10),
    
    # age.proportion.tested.or = Multivariate.Lognormal.Distribution(
    #   mu = rep(0, 5),
    #   #sigma = create.compound.symmetry.covariance.matrix(0.5, 5, 0.5*log(1.5)),
    #   sigma = create.auto.regressive.covariance.matrix(0.7, 5, sd=0.5*log(1.5)),
    #   var.names = paste0("age", 1:5, ".proportion.tested.or")
    # ),
    # 
    # age.proportion.tested.slope.or = Multivariate.Lognormal.Distribution(
    #   mu = rep(0, 5),
    #   sigma = create.auto.regressive.covariance.matrix(0.7, 5, 0.5*log(1.5)/10),
    #   var.names = paste0("age", 1:5, ".proportion.tested.slope.or")
    # ),
    # 
    
    # Testing Ramp
    msm.testing.ramp = Logitnormal.Distribution(0, 0.5 * log(1.5)),
    heterosexual.testing.ramp = Logitnormal.Distribution(0, 0.5 * log(1.5)),
    idu.testing.ramp = Logitnormal.Distribution(0, 0.5 * log(1.5)),
    
    # Increase in Testing for Undiagnosed
    msm.undiagnosed.testing.increase.rr = Lognormal.Distribution(0, 0.5*log(2)),
    heterosexual.undiagnosed.testing.increase.rr = Lognormal.Distribution(0, 0.5*log(2)),
    idu.undiagnosed.testing.increase.rr = Lognormal.Distribution(0, 0.5*log(2)),
    
    
    #-- PrEP --#
    
    msm.prep.intercept.or = Lognormal.Distribution(0, log(8)),
    non.msm.prep.intercept.or = Lognormal.Distribution(0, log(4)),
    
    # NB: These slope ORs are NOT exponentiated logistic slopes
    # They are an OR that is applied to the linear (ie probability scale) slope
    msm.prep.slope.or = Lognormal.Distribution(0, log(8)),
    non.msm.prep.slope.or = Compound.Symmetry.Multivariate.Normal.Distribution(rho=0.8,
                                                                               n=2,
                                                                               mu=0,
                                                                               sds = log(4),
                                                                               transformations = 'log',
                                                                               lower=0,
                                                                               var.names=c('idu.prep.slope.or',
                                                                                           'heterosexual.prep.slope.or')),
    
    # These ORs are applied to both intercept and slope
    black.prep.or = Lognormal.Distribution(0, log(1.25)/2),
    hispanic.prep.or = Lognormal.Distribution(0, log(1.25)/2),
    
    age1.prep.or = Lognormal.Distribution(0, log(2)),
    age2.prep.or = Lognormal.Distribution(0, log(2)),
    age3.prep.or = Lognormal.Distribution(0, log(2)),
    age4.prep.or = Lognormal.Distribution(0, log(2)),
    age5.prep.or = Lognormal.Distribution(0, log(2)),
    
    prep.efficacy.z = Normal.Distribution(0, 1),
    # oral.prep.persistence = Normal.Distribution(0.56, 0.0587, lower=0, upper=1),
    oral.prep.persistence.or = Lognormal.Distribution(0, log(1.2)/2),
    #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6378757/
    # with Wald CI inflated 10x
    
    prep.fraction.sexual.transmission.avoidable.z = Normal.Distribution(0,1),

    msm.prep.indications.or = Lognormal.Distribution(0, 0.5*log(4)), 
    non.msm.prep.indications.or = Lognormal.Distribution(0, 0.5*log(4)),
    prep.indications.slope.or = Lognormal.Distribution(0, 0.5*log(4)/5),
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    
    #-- IDU Transitions --#
    
    # black.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    # hispanic.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    # other.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    
    black.incident.idu.multiplier.1 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.1 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.1 = Lognormal.Distribution(0, .5*log(2)),
    
    black.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    age1.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age2.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age3.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age4.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age5.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),

    # msm.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    msm.incident.idu.multiplier.1 = Lognormal.Distribution(0, .5*log(2)),
    msm.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    black.active.idu.initial.prevalence.ratio = Lognormal.Distribution(0, 0.5*log(4)),
    hispanic.active.idu.initial.prevalence.ratio = Lognormal.Distribution(0, 0.5*log(4)),
    other.active.idu.initial.prevalence.ratio = Lognormal.Distribution(0, 0.5*log(4)),
    msm.active.idu.initial.prevalence.ratio = Lognormal.Distribution(0, 0.5*log(4)),
        
    idu.remission.multiplier = Lognormal.Distribution(0, .5*log(2)),
    idu.relapse.multiplier = Lognormal.Distribution(0, .5*log(2)),
    #idu.mortality = Lognormal.Distribution(log(0.0166), 0.1322), 
    #citation=25409098,
    #non-AIDS mortality in hiv-negative from table 3'
    
    #-- Increased mortality among HIV vs. HIV-negative --#
    # # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6088250/ 
          # idu.adjustment = 2.41/3.01
          # non.aids.mortality.adjustment = 2.43
          # non.aids.mortality.adjustment*idu.adjustment # 1.945615
    hiv.general.mortality.multiplier = Lognormal.Distribution(log(1.95),sdlog = log(2)/2,lower=1),

    #-- HIV-Specific Mortality (unsuppressed) --#
    unsuppressed.hiv.mortality.0 = Lognormal.Distribution(log(9.5/6.1 * 23/1000), log(2)/2),
    unsuppressed.hiv.mortality.1 = Lognormal.Distribution(log(23/1000), log(2)),
    unsuppressed.peak.hiv.mortality = Lognormal.Distribution(log(41/6.1 * 23/1000), log(2)/2),
    #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.688.1831&rep=rep1&type=pdf
    
    # age1.unsuppressed.hiv.mortality.multiplier = Lognormal.Distribution(log(1),sdlog = log(2)/2),
    # age2.unsuppressed.hiv.mortality.multiplier = Lognormal.Distribution(log(1),sdlog = log(2)/2),
    # age3.unsuppressed.hiv.mortality.multiplier = Lognormal.Distribution(log(1),sdlog = log(2)/2),
    # age4.unsuppressed.hiv.mortality.multiplier = Lognormal.Distribution(log(1),sdlog = log(2)/2),
    # age5.unsuppressed.hiv.mortality.multiplier = Lognormal.Distribution(log(1),sdlog = log(2)/2),
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Other Sexual Mixing --#
    
    oe.female.pairings.with.msm = Lognormal.Distribution(log(0.0895), 0.5*log(2), upper = 1), #Pathela 2006 - see below
    fraction.heterosexual.male.pairings.with.male = Lognormal.Distribution(log(.0004), 0.5*log(2), upper=1),
    oe.never.idu.pairings.with.idu = Lognormal.Distribution(log(0.2), 0.5*log(2), upper=1), #see calculations below
    
    black.sexual.assortativity.multiplier = Normal.Distribution(1, 0.5, lower=0),
    hispanic.sexual.assortativity.multiplier = Normal.Distribution(1, 0.5, lower=0),
    other.sexual.assortativity.multiplier = Normal.Distribution(1, 0.5, lower=0),
    race.needle.sharing.assortativity.multiplier = Normal.Distribution(1, 0.5, lower=0),

    # black.black.sexual.oe = Normal.Distribution((3.76), 0.5*3.76, lower=1), # changed 2/23/24
    # hispanic.hispanic.sexual.oe = Normal.Distribution((2.19), 0.5*2.19, lower=1),
    # other.other.sexual.oe = Normal.Distribution((1.55), 0.5*1.55, lower=1),
    # 
    # black.black.idu.oe = Normal.Distribution((9.12), 0.5*9.12, lower=1), # added 2/29/24
    # hispanic.hispanic.idu.oe = Normal.Distribution((1.05), 0.5*1.05, lower=1),
    # other.other.idu.oe = Normal.Distribution((1.05), 0.5*1.05, lower=1),
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = Lognormal.Distribution(log(12), 0.25*log(2)), #do I have a more evidence based range?
    diagnosed.transmission.rr = Lognormal.Distribution(log(mean(c(1-.68, 1/3.5))), 0.25*log(2), upper=1), #avg of Marks 2006 and Marks 2005
    
    #-- Uncertainty About the Future --#
    msm.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    heterosexual.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    idu.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    
    #-- COVID Parameters --#
    
    # Sexual transmission: 8 alphas
    # parameter: max.covid.effect.sexual.transmission.reduction
    black.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    hispanic.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    other.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    age12.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    age34.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    age5.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    heterosexual.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    msm.sexual.transmission.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    # Testing: 9 alphas
    # parameter: max.covid.effect.testing.reduction
    black.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    hispanic.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    other.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    age12.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    age34.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    age5.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    heterosexual.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    msm.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    idu.testing.covid.multiplier = Lognormal.Distribution(0, .5*log(2)),
    
    # Undiagnosed Testing RR
    # parameter: max.covid.effect.undiagnosed.testing.rr.increase
    max.covid.effect.undiagnosed.testing.rr.increase = Lognormal.Distribution(0, .5*log(2)),
    
    # PrEP use
    # parameter: max.covid.effect.prep.uptake.reduction
    max.covid.effect.prep.uptake.reduction = Lognormal.Distribution(0, .5*log(2)),
    
    # Suppression
    # parameter: max.covid.effect.suppression.of.diagnosed.reduction
    max.covid.effect.suppression.of.diagnosed.reduction = Lognormal.Distribution(0, .5*log(1.1)),
    
    # IDU
    # parameter: max.covid.effect.idu.transmission.reduction
    max.covid.effect.idu.transmission.reduction = Lognormal.Distribution(0, .5*log(2)),
    
    # PrEP indications: 0 alphas (tied to sexual transmission alphas)
    # parameter: sexual.susceptibility.covid.multiplier (same alphas as above)
    
    # ^ don't need to map these four parameters because they are named exactly as they are in the specification
    
    
    
    #-- AIDS Diagnoses --#
    aids.to.new.diagnosis.ratio = Multivariate.Lognormal.Distribution(mu = c(1.09264522805781, -0.022054632609793, -0.0811281287465434),
                                                                      sigma = 1/96* matrix(c(0.0540150046153106, -0.00544775631061186, 0.00227929482952582, -0.00544775631061186, 0.00140533026447334, -0.000587978210286446, 0.00227929482952582, -0.000587978210286446, 0.00156311624587849), nrow=3),
                                                                        # ^for the mean and cov mata, see aids_diagnoses_multiplier.R
                                                                      # The 1/135 is us saying that this ratio is going to independently apply to all 135 strata
                                                                      # The 1/96 is us saying that this ratio is going to independently to all 135 strata, if they were correlated with a compound symmetry matrix
                                                                      var.names = paste0('aids.to.new.diagnoses.ratio.', c('peak','0','1'))) # see aids_diagnoses_multiplier.R
)

# EHE.SUPPRESSION.PRIOR ----
EHE.SUPPRESSION.PRIOR = distributions::join.distributions(

    #-- Suppression --#
    heterosexual.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.suppressed.or = Lognormal.Distribution(0, log(2)),
    idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    black.suppressed.or = Lognormal.Distribution(0, log(2)),
    hispanic.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    age1.suppressed.or = Lognormal.Distribution(0, log(2)),
    age2.suppressed.or = Lognormal.Distribution(0, log(2)),
    age3.suppressed.or = Lognormal.Distribution(0, log(2)),
    age4.suppressed.or = Lognormal.Distribution(0, log(2)),
    age5.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                                  5),
    msm.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) / 5),
    idu.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) / 5),
    msm.idu.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                             5),
    
    black.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                           5),
    hispanic.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                              5),
    
    age1.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                          5),
    age2.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                        5),
    age3.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                        5),
    age4.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                          5),
    age5.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                          5)
)

# EHE.PARAMETERS.PRIOR ----
EHE.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  BASE.HIV.PARAMETERS.PRIOR,
  EHE.SUPPRESSION.PRIOR
)
  

##---------------------##
##---------------------##
##-- SAMPLING BLOCKS --## ----
##---------------------##
##---------------------##
# EHE.POPULATION.SAMPLING.BLOCKS ----
EHE.POPULATION.SAMPLING.BLOCKS = list(

#-- POPULATION PARAMETERS --#

    black.birth.rate = c('black.birth.rate.multiplier'),
  
    black.immigration = c(
      "black.immigration.multiplier.time.1",
      "black.immigration.multiplier.time.2"
    ),
    
    black.death.and.emigration = c(
      "black.emigration.multiplier.time.1",
      "black.emigration.multiplier.time.2",
      'black.non.idu.general.mortality.rate.multiplier'
    ),
  
    black.age1.aging.and.migration.1 = c(
      'black.age1.aging.multiplier.1',
      'black.age1.migration.multiplier.time.1'
    ),
    
    black.age1.aging.and.migration.2 = c(
      'black.age1.aging.multiplier.2',
      'black.age1.migration.multiplier.time.2'
    ),
    
    black.age2.aging.and.migration.1 = c(
      'black.age2.aging.multiplier.1',
      'black.age2.migration.multiplier.time.1'
    ),
    
    black.age2.aging.and.migration.2 = c(
      'black.age2.aging.multiplier.2',
      'black.age2.migration.multiplier.time.2'
    ),
    
    black.age3.aging.and.migration.1 = c(
      'black.age3.aging.multiplier.1',
      'black.age3.migration.multiplier.time.1'
    ),
    
    black.age3.aging.and.migration.2 = c(
      'black.age3.aging.multiplier.2',
      'black.age3.migration.multiplier.time.2'
    ),
    
    black.age4.aging.and.migration.1 = c(
      'black.age4.aging.multiplier.1',
      'black.age4.migration.multiplier.time.1'
    ),
    
    black.age4.aging.and.migration.2 = c(
      'black.age4.aging.multiplier.2',
      'black.age4.migration.multiplier.time.2'
    ),
    
    black.age5migration = c(
      'black.age5.migration.multiplier.time.1',
      'black.age5.migration.multiplier.time.2'
    ),
    
    hispanic.birth.rate = c('hispanic.birth.rate.multiplier'),
    
    hispanic.immigration = c(
      "hispanic.immigration.multiplier.time.1",
      "hispanic.immigration.multiplier.time.2"
    ),
    
    hispanic.death.and.emigration = c(
      "hispanic.emigration.multiplier.time.1",
      "hispanic.emigration.multiplier.time.2",
      'hispanic.non.idu.general.mortality.rate.multiplier'
    ),
    
    hispanic.age1.aging.and.migration.1 = c(
      'hispanic.age1.aging.multiplier.1',
      'hispanic.age1.migration.multiplier.time.1'
    ),
    
    hispanic.age1.aging.and.migration.2 = c(
      'hispanic.age1.aging.multiplier.2',
      'hispanic.age1.migration.multiplier.time.2'
    ),
    
    hispanic.age2.aging.and.migration.1 = c(
      'hispanic.age2.aging.multiplier.1',
      'hispanic.age2.migration.multiplier.time.1'
    ),
    
    hispanic.age2.aging.and.migration.2 = c(
      'hispanic.age2.aging.multiplier.2',
      'hispanic.age2.migration.multiplier.time.2'
    ),
    
    hispanic.age3.aging.and.migration.1 = c(
      'hispanic.age3.aging.multiplier.1',
      'hispanic.age3.migration.multiplier.time.1'
    ),
    
    hispanic.age3.aging.and.migration.2 = c(
      'hispanic.age3.aging.multiplier.2',
      'hispanic.age3.migration.multiplier.time.2'
    ),
    
    hispanic.age4.aging.and.migration.1 = c(
      'hispanic.age4.aging.multiplier.1',
      'hispanic.age4.migration.multiplier.time.1'
    ),
    
    hispanic.age4.aging.and.migration.2 = c(
      'hispanic.age4.aging.multiplier.2',
      'hispanic.age4.migration.multiplier.time.2'
    ),
    
    hispanic.age5migration = c(
      'hispanic.age5.migration.multiplier.time.1',
      'hispanic.age5.migration.multiplier.time.2'
    ),
  
    other.birth.rate = c('other.birth.rate.multiplier'),
  
    other.immigration = c(
      "other.immigration.multiplier.time.1",
      "other.immigration.multiplier.time.2"
    ),
    
    other.death.and.emigration = c(
      "other.emigration.multiplier.time.1",
      "other.emigration.multiplier.time.2",
      'other.non.idu.general.mortality.rate.multiplier'
    ),
    
    other.age1.aging.and.migration.1 = c(
      'other.age1.aging.multiplier.1',
      'other.age1.migration.multiplier.time.1'
    ),
    
    other.age1.aging.and.migration.2 = c(
      'other.age1.aging.multiplier.2',
      'other.age1.migration.multiplier.time.2'
    ),
    
    other.age2.aging.and.migration.1 = c(
      'other.age2.aging.multiplier.1',
      'other.age2.migration.multiplier.time.1'
    ),
    
    other.age2.aging.and.migration.2 = c(
      'other.age2.aging.multiplier.2',
      'other.age2.migration.multiplier.time.2'
    ),
    
    other.age3.aging.and.migration.1 = c(
      'other.age3.aging.multiplier.1',
      'other.age3.migration.multiplier.time.1'
    ),
    
    other.age3.aging.and.migration.2 = c(
      'other.age3.aging.multiplier.2',
      'other.age3.migration.multiplier.time.2'
    ),
    
    other.age4.aging.and.migration.1 = c(
      'other.age4.aging.multiplier.1',
      'other.age4.migration.multiplier.time.1'
    ),
    
    other.age4.aging.and.migration.2 = c(
      'other.age4.aging.multiplier.2',
      'other.age4.migration.multiplier.time.2'
    ),
    
    other.age5migration = c(
      'other.age5.migration.multiplier.time.1',
      'other.age5.migration.multiplier.time.2'
    ),

    age.specific.mortality = c(
      'age1.non.idu.general.mortality.rate.multiplier',
      'age2.non.idu.general.mortality.rate.multiplier',
      'age3.non.idu.general.mortality.rate.multiplier',
      'age4.non.idu.general.mortality.rate.multiplier',
      'age5.non.idu.general.mortality.rate.multiplier'
    ),
    
    sex.specific.mortality = c(
      'male.non.idu.general.mortality.rate.multiplier',
      'female.non.idu.general.mortality.rate.multiplier'
    )
)
# BASE.HIV.SAMPLING.BLOCKS ----
BASE.HIV.SAMPLING.BLOCKS = list(
    
    proportion.msm.of.male = 'proportion.msm.of.male.mult',
    
#-- HIV AGING --#

    msm.hiv.aging.0 = c('age1.msm.hiv.aging.multiplier.0',
                        'age2.msm.hiv.aging.multiplier.0',
                        'age3.msm.hiv.aging.multiplier.0',
                        'age4.msm.hiv.aging.multiplier.0'),
    
    heterosexual.hiv.aging.0 = c('age1.heterosexual.hiv.aging.multiplier.0',
                                 'age2.heterosexual.hiv.aging.multiplier.0',
                                 'age3.heterosexual.hiv.aging.multiplier.0',
                                 'age4.heterosexual.hiv.aging.multiplier.0'),
    
    idu.hiv.aging.0 = c('age1.idu.hiv.aging.multiplier.0',
                        'age2.idu.hiv.aging.multiplier.0',
                        'age3.idu.hiv.aging.multiplier.0',
                        'age4.idu.hiv.aging.multiplier.0'),
    
    msm.hiv.aging.1 = c('age1.msm.hiv.aging.multiplier.1',
                        'age2.msm.hiv.aging.multiplier.1',
                        'age3.msm.hiv.aging.multiplier.1',
                        'age4.msm.hiv.aging.multiplier.1'),
    
    heterosexual.hiv.aging.1 = c('age1.heterosexual.hiv.aging.multiplier.1',
                                 'age2.heterosexual.hiv.aging.multiplier.1',
                                 'age3.heterosexual.hiv.aging.multiplier.1',
                                 'age4.heterosexual.hiv.aging.multiplier.1'),
    
    idu.hiv.aging.1 = c('age1.idu.hiv.aging.multiplier.1',
                        'age2.idu.hiv.aging.multiplier.1',
                        'age3.idu.hiv.aging.multiplier.1',
                        'age4.idu.hiv.aging.multiplier.1'),
  
    msm.hiv.aging.2 = c('age1.msm.hiv.aging.multiplier.2',
                        'age2.msm.hiv.aging.multiplier.2',
                        'age3.msm.hiv.aging.multiplier.2',
                        'age4.msm.hiv.aging.multiplier.2'),
    
    heterosexual.hiv.aging.2 = c('age1.heterosexual.hiv.aging.multiplier.2',
                                 'age2.heterosexual.hiv.aging.multiplier.2',
                                 'age3.heterosexual.hiv.aging.multiplier.2',
                                 'age4.heterosexual.hiv.aging.multiplier.2'),
    
    idu.hiv.aging.2 = c('age1.idu.hiv.aging.multiplier.2',
                        'age2.idu.hiv.aging.multiplier.2',
                        'age3.idu.hiv.aging.multiplier.2',
                        'age4.idu.hiv.aging.multiplier.2'),
    
    race.hiv.aging.0 = c('black.hiv.aging.multiplier.0',
                         'hispanic.hiv.aging.multiplier.0',
                         'other.hiv.aging.multiplier.0'),
    
    race.hiv.aging.1 = c('black.hiv.aging.multiplier.1',
                         'hispanic.hiv.aging.multiplier.1',
                         'other.hiv.aging.multiplier.1'),
    
    race.hiv.aging.2 = c('black.hiv.aging.multiplier.2',
                         'hispanic.hiv.aging.multiplier.2',
                         'other.hiv.aging.multiplier.2'),
  
#-- MSM TRANSMISSION --#

    peak.msm.transmission = 'msm.peak.trate.multiplier',
    
    black.msm.transmission = c(
      'black.msm.trate.0',
      'black.msm.trate.1',
      'black.msm.trate.2'
    ),
    
    hispanic.msm.transmission = c(
      'hispanic.msm.trate.0',
      'hispanic.msm.trate.1',
      'hispanic.msm.trate.2'
    ),
    
    other.msm.transmission = c(
      'other.msm.trate.0',
      'other.msm.trate.1',
      'other.msm.trate.2',
      'msm.fraction.trate.change.after.t2'
    ),

    young.black.msm.age.susceptibility = c(
      'age1.black.msm.susceptibility.rr.01',
      'age1.black.msm.susceptibility.rr.2',
      'age2.black.msm.susceptibility.rr.01',
      'age2.black.msm.susceptibility.rr.2'
    ),
    
    young.hispanic.msm.age.susceptibility = c(
      'age1.hispanic.msm.susceptibility.rr.01',
      'age1.hispanic.msm.susceptibility.rr.2',
      'age2.hispanic.msm.susceptibility.rr.01',
      'age2.hispanic.msm.susceptibility.rr.2'
    ),
    
    young.other.msm.age.susceptibility = c(
      'age1.other.msm.susceptibility.rr.01',
      'age1.other.msm.susceptibility.rr.2',
      'age2.other.msm.susceptibility.rr.01',
      'age2.other.msm.susceptibility.rr.2'
    ),

    old.msm.age.susceptibility.01 = c(
      'age3.msm.susceptibility.rr.01',
      'age4.msm.susceptibility.rr.01',
      'age5.msm.susceptibility.rr.01'
    ),

    old.msm.age.susceptibility.2 = c(
      'age3.msm.susceptibility.rr.2',
      'age4.msm.susceptibility.rr.2',
      'age5.msm.susceptibility.rr.2'
    ),


#-- ASSORTATIVITY --# 

    assortativity = c(
        'black.sexual.assortativity.multiplier',
        'hispanic.sexual.assortativity.multiplier',
        'other.sexual.assortativity.multiplier',
        'race.needle.sharing.assortativity.multiplier'
    ),

    sexual.pairing = c(
      'oe.female.pairings.with.msm',
      'fraction.heterosexual.male.pairings.with.male',
      'oe.never.idu.pairings.with.idu'
    ),

    age.mixing = 'age.mixing.sd.mult',

  
#-- HETEROSEXUAL TRANSMISSION --#

    peak.heterosexual.transmission = 'heterosexual.peak.trate.multiplier',
    
    black.heterosexual.transmission = c(
      'black.heterosexual.trate.0',
      'black.heterosexual.trate.1',
      'black.heterosexual.trate.2'
    ),
    
    hispanic.heterosexual.transmission = c(
      'hispanic.heterosexual.trate.0',
      'hispanic.heterosexual.trate.1',
      'hispanic.heterosexual.trate.2'
    ),
    
    other.heterosexual.transmission = c(
      'other.heterosexual.trate.0',
      'other.heterosexual.trate.1',
      'other.heterosexual.trate.2',
      'heterosexual.fraction.trate.change.after.t2'
    ),
    
    male.vs.female.heterosexual.rr = 'male.vs.female.heterosexual.rr',

    heterosexual.age.susceptibility.01 = c(
      'age1.heterosexual.susceptibility.rr.01',
      'age2.heterosexual.susceptibility.rr.01',
      'age3.heterosexual.susceptibility.rr.01',
      'age4.heterosexual.susceptibility.rr.01',
      'age5.heterosexual.susceptibility.rr.01'
    ),

    heterosexual.age.susceptibility.2 = c(
      'age1.heterosexual.susceptibility.rr.2',
      'age2.heterosexual.susceptibility.rr.2',
      'age3.heterosexual.susceptibility.rr.2',
      'age4.heterosexual.susceptibility.rr.2',
      'age5.heterosexual.susceptibility.rr.2'
    ),



#-- IDU TRANSMISSION --#
  
    peak.idu.transmission = 'idu.peak.trate.multiplier',
    
    black.idu.transmission = c(
      'black.idu.trate.0',
      'black.idu.trate.1',
      'black.idu.trate.2'
    ),
    
    hispanic.idu.transmission = c(
      'hispanic.idu.trate.0',
      'hispanic.idu.trate.1',
      'hispanic.idu.trate.2'
    ),
    
    other.idu.transmission = c(
      'other.idu.trate.0',
      'other.idu.trate.1',
      'other.idu.trate.2',
      'idu.fraction.trate.change.after.t2'
    ),
    
    female.vs.heterosexual.male.idu.susceptibility = 'female.vs.heterosexual.male.idu.susceptibility.rr',
    
    msm.idu.transmission = c(
      'msm.idu.susceptibility.rr.peak',
      'msm.idu.susceptibility.rr.0',
      'msm.idu.susceptibility.rr.1',
      'msm.idu.susceptibility.rr.2'
    ),

    idu.age.susceptibility.01 = c(
      'age1.idu.susceptibility.rr.01',
      'age2.idu.susceptibility.rr.01',
      'age3.idu.susceptibility.rr.01',
      'age4.idu.susceptibility.rr.01',
      'age5.idu.susceptibility.rr.01'
    ),

    idu.age.susceptibility.2 = c(
      'age1.idu.susceptibility.rr.2',
      'age2.idu.susceptibility.rr.2',
      'age3.idu.susceptibility.rr.2',
      'age4.idu.susceptibility.rr.2',
      'age5.idu.susceptibility.rr.2'
    ),

#-- IDU INCIDENCE/REMISSION/RELAPSE --#
  
    black.idu.incidence = c(
      'black.active.idu.initial.prevalence.ratio',
      'black.incident.idu.multiplier.1',
      'black.incident.idu.multiplier.2'),
  
    hispanic.idu.incidence = c(
      'hispanic.active.idu.initial.prevalence.ratio',
      'hispanic.incident.idu.multiplier.1',
      'hispanic.incident.idu.multiplier.2'),
    
    other.idu.incidence = c(
      'other.active.idu.initial.prevalence.ratio',
      'other.incident.idu.multiplier.1',
      'other.incident.idu.multiplier.2'),
    
    msm.idu.incidence = c(
      'msm.active.idu.initial.prevalence.ratio',
      'msm.incident.idu.multiplier.1',
      'msm.incident.idu.multiplier.2'),
  
    age.idu.transitions = c(
      'age1.incident.idu.multiplier',
      'age2.incident.idu.multiplier',
      'age3.incident.idu.multiplier',
      'age4.incident.idu.multiplier',
      'age5.incident.idu.multiplier'
    ),
    
    other.idu.transitions = c('idu.remission.multiplier',
                              'idu.relapse.multiplier'),
    
    diagnosed.transmission = c('diagnosed.transmission.rr',
                               'global.trate'),
    
    acute.transmissibility = c('acute.transmissibility.rr',
                               'global.trate'),
  
    age.peak.multiplier = c(
      'age1.susceptibility.rr.mult.0',
      'age2.susceptibility.rr.mult.0',
      'age3.susceptibility.rr.mult.0',
      'age4.susceptibility.rr.mult.0',
      'age5.susceptibility.rr.mult.0'
    ),

#-- TESTING --#
    overall.testing = c(
      'proportion.tested.or',
      'proportion.tested.slope.or',
      'msm.undiagnosed.testing.increase.rr'
    ),
    
    heterosexual.testing = c(
      'heterosexual.proportion.tested.or',
      'heterosexual.proportion.tested.slope.or',
      'heterosexual.undiagnosed.testing.increase.rr'
    ),
  
    idu.testing = c(
      'idu.proportion.tested.or',
      'idu.proportion.tested.slope.or',
      'idu.undiagnosed.testing.increase.rr'
    ),
    
    msm.idu.testing = c(
      'msm.idu.proportion.tested.or',
      'msm.idu.proportion.tested.slope.or'
    ),
    
    race.testing = c(
      'black.proportion.tested.or',
      'hispanic.proportion.tested.or'
    ),

    # black.testing = c(
    #   'black.proportion.tested.or',
    #   'black.proportion.tested.slope.or'
    # ),
    # 
    # hispanic.testing = c(
    #   'hispanic.proportion.tested.or',
    #   'hispanic.proportion.tested.slope.or'
    # ),
    
    young.age.testing = c('age1.proportion.tested.or',
                          'age1.proportion.tested.slope.or',
                          'age2.proportion.tested.or',
                          'age2.proportion.tested.slope.or',
                          'age3.proportion.tested.or',
                          'age3.proportion.tested.slope.or'),
    
    old.age.testing = c('age4.proportion.tested.or',
                        'age4.proportion.tested.slope.or',
                        'age5.proportion.tested.or',
                        'age5.proportion.tested.slope.or'),

    testing.ramp.by.risk = c(
      'msm.testing.ramp',
      'heterosexual.testing.ramp',
      'idu.testing.ramp'),


#-- PREP --#

    msm.prep = c(
      'msm.prep.intercept.or',
      'msm.prep.slope.or',
      'msm.prep.indications.or',
      'oral.prep.persistence.or'
    ),
    
    non.msm.prep = c(
      'non.msm.prep.intercept.or',
      'idu.prep.slope.or',
      'heterosexual.prep.slope.or',
      'non.msm.prep.indications.or'
    ),
    
    prep.eligible.and.indications = c(
      'prep.fraction.sexual.transmission.avoidable.z',
      'prep.indications.slope.or'),
  
    prep.by.race = c('black.prep.or',
                     'hispanic.prep.or',
                     'prep.efficacy.z'),
    
    prep.by.age = c(
      'age1.prep.or',
      'age2.prep.or',
      'age3.prep.or',
      'age4.prep.or',
      'age5.prep.or'
    ),
  
#-- HIV MORTALITY --#

    unsuppressed.hiv.mortality = c('unsuppressed.peak.hiv.mortality',
                                   'unsuppressed.hiv.mortality.0',
                                   'unsuppressed.hiv.mortality.1',
                                   'hiv.general.mortality.multiplier'),

#-- COVID --#

    covid.sexual.transmission.race = c('black.sexual.transmission.covid.multiplier',
                                       'hispanic.sexual.transmission.covid.multiplier',
                                       'other.sexual.transmission.covid.multiplier'),
    
    covid.sexual.transmission.age = c('age12.sexual.transmission.covid.multiplier',
                                      'age34.sexual.transmission.covid.multiplier',
                                      'age5.sexual.transmission.covid.multiplier'),
    
    covid.sexual.transmission.risk = c('heterosexual.sexual.transmission.covid.multiplier',
                                       'msm.sexual.transmission.covid.multiplier'),
    
    covid.testing.race = c('black.testing.covid.multiplier',
                           'hispanic.testing.covid.multiplier',
                           'other.testing.covid.multiplier'),
    
    covid.testing.age = c('age12.testing.covid.multiplier',
                          'age34.testing.covid.multiplier',
                          'age5.testing.covid.multiplier'),
    
    covid.testing.risk = c('heterosexual.testing.covid.multiplier',
                           'msm.testing.covid.multiplier',
                           'idu.testing.covid.multiplier'),
    
    additional.covid.multipliers = c('max.covid.effect.undiagnosed.testing.rr.increase',
                                     'max.covid.effect.prep.uptake.reduction',
                                     'max.covid.effect.suppression.of.diagnosed.reduction',
                                     'max.covid.effect.idu.transmission.reduction'),


#-- AIDS to NEW DIAGNOSES --#

    aids.to.new.diagnoses.ratio = c(
      'aids.to.new.diagnoses.ratio.peak',
      'aids.to.new.diagnoses.ratio.0',
      'aids.to.new.diagnoses.ratio.1')
)

#-- SUPPRESSION --#
# EHE.SUPPRESSION.SAMPLING.BLOCKS ----
EHE.SUPPRESSION.SAMPLING.BLOCKS = list(
  msm.suppression = c(
    'msm.suppressed.or',
    'msm.suppressed.slope.or',
    'msm.idu.suppressed.or',
    'msm.idu.suppressed.slope.or'
  ),
  
  idu.heterosexual.suppression = c(
    'idu.suppressed.or',
    'idu.suppressed.slope.or',
    'heterosexual.suppressed.or',
    'heterosexual.suppressed.slope.or'
  ),
  
  suppression.by.race = c(
    'black.suppressed.or',
    'black.suppressed.slope.or',
    'hispanic.suppressed.or',
    'hispanic.suppressed.slope.or'
  ),
  
  young.suppression = c(
    'age1.suppressed.or',
    'age1.suppressed.slope.or',
    'age2.suppressed.or',
    'age2.suppressed.slope.or',
    'age3.suppressed.or'
  ),
  
  old.suppression = c(
    'age3.suppressed.slope.or',
    'age4.suppressed.or',
    'age4.suppressed.slope.or',
    'age5.suppressed.or',
    'age5.suppressed.slope.or'
  )
)
# EHE.NON.POPULATION.SAMPLING.BLOCKS ----
EHE.NON.POPULATION.SAMPLING.BLOCKS = c(BASE.HIV.SAMPLING.BLOCKS,
                                       EHE.SUPPRESSION.SAMPLING.BLOCKS)

# Population parameters are sampled 1/3 as much
# EHE.PARAMETER.SAMPLING.BLOCKS ----
EHE.PARAMETER.SAMPLING.BLOCKS = 
  c(EHE.POPULATION.SAMPLING.BLOCKS,
    EHE.NON.POPULATION.SAMPLING.BLOCKS,
    EHE.NON.POPULATION.SAMPLING.BLOCKS,
    EHE.NON.POPULATION.SAMPLING.BLOCKS
    )
    