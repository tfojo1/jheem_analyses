MSM.BASE.TRATE.MEAN = 1
HET.BASE.TRATE.MEAN = 1
IDU.BASE.TRATE.MEAN = 1
BASE.TRATE.CV = 20

TRATE.RR.1.2.SPAN = 2#1.5
TRATE.RR.0.1.SPAN = 4#2#1.5
TRATE.RR.0.PEAK.SPAN = 8#3

BASE.PARAMETERS.PRIOR = join.distributions(
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
    
    msm.age1.aging.base = Lognormal.Distribution(log(12209/2/22537), 0.25*log(2)),
    msm.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    msm.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #msm.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    msm.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #msm.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #msm.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #msm.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    heterosexual.age1.aging.base = Lognormal.Distribution(log((427+1861)/2/(814+3752)), 0.25*log(2)),
    heterosexual.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    heterosexual.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #heterosexual.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    heterosexual.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #heterosexual.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #heterosexual.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #heterosexual.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    idu.age1.aging.base = Lognormal.Distribution(log((180+221)/2/(218+411)), 0.25*log(2)),
    idu.age2.aging.0 = Lognormal.Distribution(log(get.aging.rate.mid.of.20(56552,151476)), 0.5*log(2)),
    idu.age2.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(49965,69515)), 0.5*log(2)),
    #idu.age2.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(11562,16831), 0.5*log(2)),
    idu.age3.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(91431,140082)), 0.5*log(2)),
    #idu.age3.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(23488,28704)), 0.5*log(2)),
    #idu.age4.aging.1 = Lognormal.Distribution(log(get.aging.rate.last.of.10(173559,139643)), 0.5*log(2)),
    #idu.age4.aging.2 = Lognormal.Distribution(log(get.aging.rate.last.of.10(34401,38437)), 0.5*log(2)),
    
    
    
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
    oral.prep.persistence = Normal.Distribution(0.56, 0.0587, lower=0, upper=1),
    #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6378757/
    # with Wald CI inflated 10x
    
    
    #-- Proportion MSM --#
    proportion.msm.of.male.mult = Lognormal.Distribution(0, 0.125*log(2)),
    
    #-- IDU Transitions --#
    
    black.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.0 = Lognormal.Distribution(0, .5*log(2)),
    
    black.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    hispanic.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    other.incident.idu.multiplier.2 = Lognormal.Distribution(0, .5*log(2)),
    
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
    
    black.black.sexual.oe = Lognormal.Distribution(log(3.76), 0.25*log(2), lower=1), #see below
    hispanic.hispanic.sexual.oe = Lognormal.Distribution(log(2.19), 0.25*log(2), lower=1),
    other.other.sexual.oe = Lognormal.Distribution(log(1.55), 0.25*log(2), lower=1),
    
    #-- Acute HIV and the Effect of Diagnosis --#
    acute.transmissibility.rr = Lognormal.Distribution(log(12), 0.25*log(2)), #do I have a more evidence based range?
    diagnosed.transmission.rr = Lognormal.Distribution(log(mean(c(1-.68, 1/3.5))), 0.25*log(2), upper=1), #avg of Marks 2006 and Marks 2005
    
    #-- Uncertainty About the Future --#
    msm.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    heterosexual.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5),
    idu.fraction.trate.change.after.t2 = Lognormal.Distribution(meanlog=log(0.1), sdlog=log(2), lower=0, upper=0.5)
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