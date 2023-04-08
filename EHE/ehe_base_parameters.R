
add.parameter <- function(params, param.name,
                          value,
                          ci.lower,
                          ci.upper,
                          citation=NA,
                          comment=NA)
{
    params$values[param.name] = value
    params$ci.lower[param.name] = ci.lower
    params$ci.upper[param.name] = ci.upper
    params$citation[[param.name]] = list(citation)
    params$comment[param.name] = comment

    params
}

EHE_BASE_PARAMETERS = list(values=numeric(),
              ci.lower=numeric(),
              ci.upper=numeric(),
              citation=list(),
              comment=character())

##-- MORTALITY --#
EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'idu.mortality',
                       1.66/100, 1.23/100, 2.09/100,
                       citation=25409098,
                       comment='non-AIDS mortality in hiv-negative from table 3')

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'untreated.hiv.mortality',
                       0.0869, 0.1578, 0.0368,
                       citation=27349729,
                       comment='inverse of (inverse of average rate to AIDS + inverse of average rate from AIDS to death)')

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'aids.progression.rate',
                        0.0815, NA, NA,
                        citation=27349729)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'untreated.aids.mortality',
                        0.33, NA, NA,
                        citation=30289817)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'treated.aids.mortality',
                       0.0458, NA, NA,
                       citation=30289817)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'cd4.recovery.rate',
                        0.0576, NA, NA,
                        citation=30289817)

##-- TRANSMISSIBILITY --##
EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'acute.transmissibility.rr',
                       12, 2, 24,
                       citation=26362321
                       )

#EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'diagnosed.sexual.transmissibility.rr',
#                       1-0.53,1-0.60, 1-0.45,
#                       citation=16010168)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'diagnosed.needle.sharing.rr',
                       0.777, 0.699, 0.864,
                       citation=25789742)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'diagnosed.msm.condomless.rr',
                       0.768, 0.600, 0.983,
                       citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-hssr-nhbs-msm-2014.pdf')

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'diagnosed.het.male.condomless.rr',
                       0.552, 0.380, 0.802,
                       citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-HSSR_NHBS_HET_2013.pdf')

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'diagnosed.female.condomless.rr',
                       0.751, 0.560, 0.948,
                       citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-HSSR_NHBS_HET_2013.pdf')


##-- TRANSMISSION --##

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'male.to.female.sexual.transmission',
                       4.75, 2.4, 7.1,
                       citation=26362321)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'female.to.male.sexual.transmission',
                       3.75, 1.8, 5.6,
                       citation=26362321)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'male.to.male.sexual.transmission',
                       5, 2.5, 7.5,
                       citation=26362321)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'proportion.msm.sex.with.female',
                       0.18, 0.18*5, 0.18*2,
                       citation=9525438)

##-- ACUTE INFECTION --##
EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'acute.infection.duration',
                       2.9/12, 1/12, 4/12,
                       citation=26362321
                       )

##-- TRANSMISSION RATES --##
EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'sexual.transmission.rate',
                       1/100,.1,2)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'idu.transmission.rate',
                       .25/100,1,20)


##-- PrEP --##
EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'prep.rr.msm',
                        1-0.86, 1-0.96, 1-0.58,
                        citation=26364263)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'prep.rr.heterosexual',
                       1-0.75, 1-0.87, 1-0.55,
                       citation=22784037)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'prep.rr.idu',
                        1-0.489, 1-0.722, 1-0.096,
                        citation=23769234)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'prep.persistence',
                       0.56, 0.55, 0.57,
                       citation=30775846)

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'prep.screening.frequency',
                       3/12, 3/12, 3/12)


##-- Continuum of Care --##

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'viral.suppression.rate',
                       2.4095, 1.7211, 4.2166,
                       citation=30412140)


##-- Needle Exchange --##

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'needle.exchange.rr',
                                0.6917807,NA,NA,
                                citation='10.1080/10826080600669579')


EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'needle.exchange.remission.rate.ratio',
                                1.357033, NA, NA,
                                citation=' 11027894;10609594;17034440')


EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'moud.relapse.rr',
                                0.4469188, NA, NA,
                                citation='10.1111/ajad.13051')

EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'fraction.opioid.of.idu',
                                0.6320987, NA, NA,
                                citation="NSDUH 2014-2016")

## oe.never.idu.pairings.with.idu

frac.last.partner.idu = 0.052 #From Jenness 2010
prevalence.idu.in.sample = (.269 * 410 + .234 * 436) / (410 + 436)#From Jenness 2009
frac.never.idu.partner = 1 - frac.last.partner.idu
prev.never.idu = 1 - prevalence.idu.in.sample
oe.never.idu = frac.never.idu.partner / prev.never.idu
oe.never.idu
oe.never.with.idu = frac.last.partner.idu / prevalence.idu.in.sample


EHE_BASE_PARAMETERS = add.parameter(EHE_BASE_PARAMETERS, 'oe.never.idu.pairings.with.idu',
                                oe.never.with.idu, NA, NA,
                                citation = "Jeness 2009, Jenness 2010")

#-- to calculate AIDS progression rate --#
if (1==2)
{
    #Poorolajal 2016
    aids.free.survival = c(0.82, 0.72, 0.64, 0.57, 0.26, 0.19)
    aids.free.times = c(2,4,6,8,10,12)

    aids.free.rates = -log(aids.free.survival)/aids.free.times
    mean(aids.free.rates[1:4])

    aids.untreated.survival = c(0.48, 0.26, 0.18)
    aids.untreated.times = c(2,4,6)
    aids.untreated.mortality.rates = -log(aids.untreated.survival)/aids.untreated.times
    mean(aids.untreated.mortality.rates)

    aids.treated.survival = c(0.87,0.86,0.78,0.78,0.61)
    aids.treated.times = c(2,4,6,8,10)
    aids.treated.mortality.rates = -log(aids.treated.survival)/aids.treated.times
    mean(aids.treated.mortality.rates)

    #CD4 recovery - Roul 2018
    recovery.proportion = c(0.3,0.5) #<100, 100-200 at baseline
    recover.num = c(901,939)
    recovery.time = 6

    recovery.rates = -log(1-recovery.proportion) / recovery.time

    -log(1-(sum(recovery.proportion*recover.num)/sum(recover.num))) / recovery.time



}

EHE_BASE_PARAMETER_VALUES = EHE_BASE_PARAMETERS$values

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6135657/
#


#THINGS I WILL WANT TO CITE

#No increase in mortality if well controlled: https://www.ncbi.nlm.nih.gov/pubmed/23698063

#The CDC MMWR that describes how they calculate fraction undiagnosed
#https://www.cdc.gov/mmwr/preview/mmwrhtml/mm6424a2.htm
#An older one with race/age/etc stratifications
#https://www.cdc.gov/mmwr/preview/mmwrhtml/su6102a10.htm
#
#


#MISC LINKS
#
#MMWR Reports based on NHBS
#https://www.cdc.gov/hiv/statistics/systems/nhbs/reports.html

