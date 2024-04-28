MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = 1
IDU.BASE.TRATE.MEAN = 1
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3

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

BASE.PARAMETERS.PRIOR = join.distributions(
    global.trate = Loguniform.Distribution(0,Inf),
    
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
 
    immigration.multiplier.time.1 = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),
    emigration.multiplier.time.1 = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),

    Multivariate.Lognormal.Distribution(mu=0,sigma = create.compound.symmetry.covariance.matrix(
                                          correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".migration.multiplier.time.1")),
    
    Multivariate.Lognormal.Distribution(mu=0,sigma = create.auto.regressive.covariance.matrix(
                                          correlation.coefficient = 0.5,n=5,sd=0.5*log(2)),
                                        var.names = paste0("age",c(1:5),".migration.multiplier.time.1")),
    
    
    immigration.multiplier.time.2 = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),
    emigration.multiplier.time.2 = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(4)),
    
    Multivariate.Lognormal.Distribution(mu=0,sigma = create.compound.symmetry.covariance.matrix(
                                          correlation.coefficient = 0.3,n=3,sd=0.5*log(2)),
                                        var.names = paste0(c("black","hispanic","other"),".migration.multiplier.time.2")),

    Multivariate.Lognormal.Distribution(mu=0,sigma = create.auto.regressive.covariance.matrix(
                                          correlation.coefficient = 0.5,n=5,sd=0.5*log(2)),
                                        var.names = paste0("age",c(1:5),".migration.multiplier.time.2")),

    
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
    msm.vs.heterosexual.male.idu.susceptibility.rr.peak = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.0 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.1 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    msm.vs.heterosexual.male.idu.susceptibility.rr.2 = Lognormal.Distribution(log(3.3), 0.5*log(2)),
    
    #-- Age Susceptibility --#
    
    age1.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age2.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age4.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    age5.susceptibility.rr.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    
    age1.msm.susceptibility.rr.mult.1 = Lognormal.Distribution(0, 0.25*log(2)),
    age2.msm.susceptibility.rr.mult.1 = Lognormal.Distribution(0, 0.25*log(2)),
    age4.msm.susceptibility.rr.mult.12 = Lognormal.Distribution(0, 0.25*log(2)),
    age5.msm.susceptibility.rr.mult.12 = Lognormal.Distribution(0, 0.25*log(2)),
    
    age1.msm.susceptibility.rr.mult.2 = Lognormal.Distribution(0, 0.25*log(2)),
    age2.msm.susceptibility.rr.mult.2 = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Aging --#
    
    #Age1 rates from
    #https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-supplemental-report-vol-24-5.pdf
    #table 10b
    
    #aging.2 rates from 2018 data in table 16b of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2018-updated-vol-31.pdf
    
    #aging.1 rates from 2010 data in table 15a of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2011-vol-23.pdf
    
    #aging.0 rates from 2000 data in table 10 of
    # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2002-vol-14.pdf
    
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
  
    black.age3.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.age3.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    other.age3.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    
    age4.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    
    # black.domino.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    # hispanic.domino.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    # other.domino.aging.multiplier = Lognormal.Distribution(0, 0.5*log(2)),

    # HIV AGING
    age1.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    age1.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age2.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age3.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    age4.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    black.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    hispanic.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    other.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
    msm.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    idu.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    heterosexual.hiv.aging.multiplier.0 = Lognormal.Distribution(0, 0.5*log(2)),
    
    msm.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    idu.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    heterosexual.hiv.aging.multiplier.1 = Lognormal.Distribution(0, 0.5*log(2)),
    
    msm.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    idu.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    heterosexual.hiv.aging.multiplier.2 = Lognormal.Distribution(0, 0.5*log(2)),
    
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
    heterosexual.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    msm.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    idu.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    msm.idu.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    black.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    hispanic.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    age1.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age2.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age4.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    age5.proportion.tested.or = Lognormal.Distribution(0, log(2)),
    
    heterosexual.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    idu.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    msm.idu.proportion.tested.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
    
    testing.ramp.up.vs.current.rr = Lognormal.Distribution(log(0.5), 0.25*log(2), upper = 1),
    
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
    age4.prep.or = Lognormal.Distribution(0, log(2)),
    age5.prep.or = Lognormal.Distribution(0, log(2)),
    
    prep.efficacy.z = Normal.Distribution(0, 1),
    #oral.prep.persistence = Normal.Distribution(0.56, 0.0587, lower=0, upper=1),
    #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6378757/
    # with Wald CI inflated 10x
    
    prep.fraction.sexual.transmission.avoidable.z = Normal.Distribution(0,1),

    msm.prep.indications.or = Lognormal.Distribution(0, 0.5*log(4)), 
    non.msm.prep.indications.or = Lognormal.Distribution(0, 0.5*log(4)),
    prep.indications.slope.or = Lognormal.Distribution(0, 0.5*log(4)/5),
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    
    #-- IDU Transitions --#
    
    black.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    
    black.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    age1.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age2.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age3.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age4.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),
    age5.incident.idu.multiplier = Lognormal.Distribution(0, 0.5*log(2)),

    msm.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    msm.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
    idu.remission.multiplier = Lognormal.Distribution(0, .5*log(2)),
    idu.relapse.multiplier = Lognormal.Distribution(0, .5*log(2)),
    #idu.mortality = Lognormal.Distribution(log(0.0166), 0.1322), 
    #citation=25409098,
    #non-AIDS mortality in hiv-negative from table 3'
    
    #-- HIV-Specific Mortality --#
    hiv.mortality.0 = Lognormal.Distribution(log(9.5/6.1 * 23/1000), log(2)/2),
    hiv.mortality.2 = Lognormal.Distribution(log(23/1000), log(2)),
    peak.hiv.mortality = Lognormal.Distribution(log(41/6.1 * 23/1000), log(2)/2),
    #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.688.1831&rep=rep1&type=pdf
    
    #-- Sexual Mixing by Age --#
    age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)),
    
    #-- Other Sexual Mixing --#
    
    oe.female.pairings.with.msm = Lognormal.Distribution(log(0.0895), 0.5*log(2), upper = 1), #Pathela 2006 - see below
    fraction.heterosexual.male.pairings.with.male = Lognormal.Distribution(log(.004), 0.5*log(2), upper=1),
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
    max.covid.effect.suppression.of.diagnosed.reduction = Lognormal.Distribution(0, .5*log(2)),
    
    # IDU
    # parameter: max.covid.effect.idu.transmission.reduction
    max.covid.effect.idu.transmission.reduction = Lognormal.Distribution(0, .5*log(2))
    
    # PrEP indications: 0 alphas (tied to sexual transmission alphas)
    # parameter: sexual.susceptibility.covid.multiplier (same alphas as above)
    
    # ^ don't need to map these four parameters because they are named exactly as they are in the specification
)


EHE.PARAMETERS.PRIOR = join.distributions(
    
    BASE.PARAMETERS.PRIOR,
    
    #-- Suppression --#
    heterosexual.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.suppressed.or = Lognormal.Distribution(0, log(2)),
    idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    msm.idu.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    black.suppressed.or = Lognormal.Distribution(0, log(2)),
    hispanic.suppressed.or = Lognormal.Distribution(0, log(2)),
    
    age1.suppressed.or = Lognormal.Distribution(0, log(2)),
    age2.suppressed.or = Lognormal.Distribution(0, log(2)),
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
    age4.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                          5),
    age5.suppressed.slope.or = Lognormal.Distribution(0, 0.5 * log(2) /
                                                          5)
)

BASE.PARAMETER.SAMPLING.BLOCKS = list(

  immigration.multipliers = c(
    "immigration.multiplier.time.1",
    "immigration.multiplier.time.2"
  ),
  
  emigration.multipliers = c(
    "emigration.multiplier.time.1",
    "emigration.multiplier.time.2"
  ),
  
  black.age1.population.rates = c(
    'black.birth.rate.multiplier',
    'black.age1.aging.multiplier.1',
    'black.age1.aging.multiplier.2'
#    'black.age1.domino.aging.multiplier'
  ),
  
  black.age2.population.rates = c(
    'black.age2.aging.multiplier.1',
    'black.age2.aging.multiplier.2',
    'black.age3.aging.multiplier'
#    'black.domino.aging.multiplier'
  ),
  
  black.population.rates = c(
    'black.non.idu.general.mortality.rate.multiplier',
    'black.migration.multiplier.time.1',
    'black.migration.multiplier.time.2'
  ),
  
  hispanic.age1.population.rates = c(
    'hispanic.birth.rate.multiplier',
    'hispanic.age1.aging.multiplier.1',
    'hispanic.age1.aging.multiplier.2'
#    'hispanic.age1.domino.aging.multiplier'
  ),
  
  hispanic.age2.population.rates = c(
    'hispanic.age2.aging.multiplier.1',
    'hispanic.age2.aging.multiplier.2',
    'hispanic.age3.aging.multiplier'
#    'hispanic.domino.aging.multiplier'
  ),
  
  hispanic.population.rates = c(
    'hispanic.non.idu.general.mortality.rate.multiplier',
    'hispanic.migration.multiplier.time.1',
    'hispanic.migration.multiplier.time.2'
  ),
  
  other.age1.population.rates = c(
    'other.birth.rate.multiplier',
    'other.age1.aging.multiplier.1',
    'other.age1.aging.multiplier.2'
#    'other.age1.domino.aging.multiplier'
  ),
  
  other.age2.population.rates = c(
    'other.age2.aging.multiplier.1',
    'other.age2.aging.multiplier.2',
    'other.age3.aging.multiplier'
#    'other.domino.aging.multiplier'
  ),
  
  other.population.rates = c(
    'other.non.idu.general.mortality.rate.multiplier',
    'other.migration.multiplier.time.1',
    'other.migration.multiplier.time.2'
  ),
  
  age1.population.rates = c(
    'age1.non.idu.general.mortality.rate.multiplier',
    'age1.migration.multiplier.time.1',
    'age1.migration.multiplier.time.2'
  ),
  
  age2.population.rates = c(
    'age2.non.idu.general.mortality.rate.multiplier',
    'age2.migration.multiplier.time.1',
    'age2.migration.multiplier.time.2'
  ),
  
  age3.population.rates = c(
    'age3.non.idu.general.mortality.rate.multiplier',
    'age3.migration.multiplier.time.1',
    'age3.migration.multiplier.time.2'
  ),
  
  age4.population.rates = c(
    'age4.non.idu.general.mortality.rate.multiplier',
    'age4.aging.multiplier',
    'age4.migration.multiplier.time.1',
    'age4.migration.multiplier.time.2'
  ),
  
  age5.population.rates = c(
    'age5.non.idu.general.mortality.rate.multiplier',
    'age4.aging.multiplier',
    'age5.migration.multiplier.time.1',
    'age5.migration.multiplier.time.2'
  ),
  
  sex.non.idu.general.mortality.rates = c(
    'male.non.idu.general.mortality.rate.multiplier',
    'female.non.idu.general.mortality.rate.multiplier'
  ),

  age.hiv.aging.0 = c('age1.hiv.aging.multiplier.0',
                      'age2.hiv.aging.multiplier.0',
                      'age3.hiv.aging.multiplier.0',
                      'age4.hiv.aging.multiplier.0'),
  
  age.hiv.aging.1 = c('age1.hiv.aging.multiplier.1',
                      'age2.hiv.aging.multiplier.1',
                      'age3.hiv.aging.multiplier.1',
                      'age4.hiv.aging.multiplier.1'),
  
  age.hiv.aging.2 = c('age1.hiv.aging.multiplier.2',
                      'age2.hiv.aging.multiplier.2',
                      'age3.hiv.aging.multiplier.2',
                      'age4.hiv.aging.multiplier.2'),
  
  race.hiv.aging.0 = c('black.hiv.aging.multiplier.0',
                      'hispanic.hiv.aging.multiplier.0',
                      'other.hiv.aging.multiplier.0'),
  
  race.hiv.aging.1 = c('black.hiv.aging.multiplier.1',
                       'hispanic.hiv.aging.multiplier.1',
                       'other.hiv.aging.multiplier.1'),
  
  race.hiv.aging.2 = c('black.hiv.aging.multiplier.2',
                       'hispanic.hiv.aging.multiplier.2',
                       'other.hiv.aging.multiplier.2'),
  
  sex.risk.hiv.aging.0 = c('msm.hiv.aging.multiplier.0',
                       'idu.hiv.aging.multiplier.0',
                       'heterosexual.hiv.aging.multiplier.0'),
  
  sex.risk.hiv.aging.1 = c('msm.hiv.aging.multiplier.1',
                           'idu.hiv.aging.multiplier.1',
                           'heterosexual.hiv.aging.multiplier.1'),
  
  sex.risk.hiv.aging.2 = c('msm.hiv.aging.multiplier.2',
                           'idu.hiv.aging.multiplier.2',
                           'heterosexual.hiv.aging.multiplier.2'),
  
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
  
  assortativity = c(
      'black.sexual.assortativity.multiplier',
      'hispanic.sexual.assortativity.multiplier',
      'other.sexual.assortativity.multiplier',
      'race.needle.sharing.assortativity.multiplier'
  ),
  # 
  # 
  # sexual.mixing = c(
  #   'black.black.sexual.oe',
  #   'hispanic.hispanic.sexual.oe',
  #   'other.other.sexual.oe'
  # ),
  # 
  # idu.mixing = c(
  #   'black.black.idu.oe',
  #   'hispanic.hispanic.idu.oe',
  #   'other.other.idu.oe'
  # ),
  
  msm.age1.susceptibility = c(
    'age1.msm.susceptibility.rr.mult.1',
    'age1.msm.susceptibility.rr.mult.2'
  ),
  
  msm.age2.susceptibility = c(
    'age2.msm.susceptibility.rr.mult.1',
    'age2.msm.susceptibility.rr.mult.2'
  ),
  
  old.msm.age.susceptibility = c(
    'age4.msm.susceptibility.rr.mult.12',
    'age5.msm.susceptibility.rr.mult.12'
  ),
  
  sexual.pairing = c(
    'oe.female.pairings.with.msm',
    'fraction.heterosexual.male.pairings.with.male',
    'oe.never.idu.pairings.with.idu'
  ),
  
  proportion.msm.of.male = 'proportion.msm.of.male.mult',
  
  age.mixing = 'age.mixing.sd.mult',
  
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
    'msm.vs.heterosexual.male.idu.susceptibility.rr.peak',
    'msm.vs.heterosexual.male.idu.susceptibility.rr.0',
    'msm.vs.heterosexual.male.idu.susceptibility.rr.1',
    'msm.vs.heterosexual.male.idu.susceptibility.rr.2'
  ),
  

  young.age.susceptibility = c(
    'age1.susceptibility.rr.mult',
    'age2.susceptibility.rr.mult'
  ),
  
  age4.susceptibility = 'age4.susceptibility.rr.mult',
  
  age5.susceptibility = 'age5.susceptibility.rr.mult',
  
  idu.transitions.0 = c(
    'black.incident.idu.multiplier.0',
    'hispanic.incident.idu.multiplier.0',
    'other.incident.idu.multiplier.0',
 #   'young.incident.idu.multiplier.0',
    'msm.incident.idu.multiplier.0'
  ),
  
  idu.transitions.2 = c(
    'black.incident.idu.multiplier.2',
    'hispanic.incident.idu.multiplier.2',
    'other.incident.idu.multiplier.2',
#    'young.incident.idu.multiplier.2',
    'msm.incident.idu.multiplier.2'
  ),

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
  
# = Normal.Distribution(0,1),


  msm.testing = c(
    'msm.proportion.tested.or',
    'msm.proportion.tested.slope.or',
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
  
  testing.by.race = c(
    'black.proportion.tested.or',
    'hispanic.proportion.tested.or'
  ),
  
  young.age.testing = c('age1.proportion.tested.or',
                        'age2.proportion.tested.or'),
  
  old.age.testing = c('age4.proportion.tested.or',
                      'age5.proportion.tested.or'),
  
  testing.ramp.up = 'testing.ramp.up.vs.current.rr',
  
  msm.prep = c(
    'msm.prep.intercept.or',
    'msm.prep.slope.or',
    'msm.prep.indications.or'
   # 'oral.prep.persistence'
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
    'age4.prep.or',
    'age5.prep.or'
  ),
  
  hiv.mortality = c('peak.hiv.mortality',
                    'hiv.mortality.0',
                    'hiv.mortality.2'),

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
                                   'max.covid.effect.idu.transmission.reduction')
)
EHE.PARAMETER.SAMPLING.BLOCKS = 
  c(BASE.PARAMETER.SAMPLING.BLOCKS,
    list(
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
        'age2.suppressed.slope.or'
      ),
      
      old.suppression = c(
        'age4.suppressed.or',
        'age4.suppressed.slope.or',
        'age5.suppressed.or',
        'age5.suppressed.slope.or'
      )
    ))
    