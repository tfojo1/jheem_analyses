# made from SHIELD_BASE_PARAMETERS.R
# what are the citation numbers? 


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

SHIELD_BASE_PARAMETERS = list(values=numeric(),
                           ci.lower=numeric(),
                           ci.upper=numeric(),
                           citation=list(),
                           comment=character())

##-- STATE DURATIONS --##
SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'primarySecondary.infection.duration',
                                       3/12, 1/12, 6/12,
                                       citation=0
)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'earlyLatent.infection.duration', #how to model it's dependancy on primarySecondary state?
                                       9/12, 6/12, 11/12,
                                       citation=0
)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'lateLatent.infection.duration',  
                                       xx,xxx,xxx
                                       citation=0
)

##-- MORTALITY --#
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'untreated.hiv.mortality',
#                                     0.0869, 0.1578, 0.0368,
#                                     citation=27349729,
#                                     comment='inverse of (inverse of average rate to AIDS + inverse of average rate from AIDS to death)')
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'aids.progression.rate',
#                                     0.0815, NA, NA,
#                                     citation=27349729)
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'untreated.aids.mortality',
#                                     0.33, NA, NA,
#                                     citation=30289817)
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'treated.aids.mortality',
#                                     0.0458, NA, NA,
#                                     citation=30289817)
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'cd4.recovery.rate',
                                    0.0576, NA, NA,
                                    citation=30289817)

##-- TRANSMISSIBILITY --##
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'acute.transmissibility.rr',
#                                     12, 2, 24,
#                                     citation=26362321
)
#   
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'diagnosed.msm.condomless.rr',
#                                     0.768, 0.600, 0.983,
#                                     citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-hssr-nhbs-msm-2014.pdf')
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'diagnosed.het.male.condomless.rr',
#                                     0.552, 0.380, 0.802,
#                                     citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-HSSR_NHBS_HET_2013.pdf')
# 
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'diagnosed.female.condomless.rr',
#                                     0.751, 0.560, 0.948,
#                                     citation='https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-HSSR_NHBS_HET_2013.pdf')
# 

##-- TRANSMISSION --##

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'male.to.female.sexual.transmission',
                                    4.75, 2.4, 7.1,
                                    citation=26362321)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'female.to.male.sexual.transmission',
                                    3.75, 1.8, 5.6,
                                    citation=26362321)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'male.to.male.sexual.transmission',
                                    5, 2.5, 7.5,
                                    citation=26362321)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'proportion.msm.sex.with.female',
                                    0.18, 0.18*5, 0.18*2,
                                    citation=9525438)

##-- TRANSMISSION RATES --##
SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'sexual.transmission.rate',
                                    1/100,.1,2)

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'idu.transmission.rate',
                                    .25/100,1,20)


##-- Continuum of Care --##

SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'viral.suppression.rate',
                                    2.4095, 1.7211, 4.2166,
                                    citation=30412140)
  
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

EHE_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETERS$values

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

