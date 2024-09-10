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

SHIELD_BASE_PARAMETER = list(values=numeric(),
                           ci.lower=numeric(),
                           ci.upper=numeric(),
                           citation=list(),
                           comment=character())

# ci's are not used
# citation numbers are oubmed ID, they're for our own records'


##-- TRANSMISSION --##
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                             'oe.female.pairings.with.msm', #observed to estimated ratio of contacts for females with male who are msm
                                             0.0895,0.0895*.75,0.0895*1.25, #Todd: I'm not sure what the CI should be here?
                                             citation='Pathela 2006')

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                             'fraction.heterosexual.male.pairings.with.male',
                                             0.004,0.004*.75,0.004*1.25, #Todd: I'm not sure what the CI should be here?
                                             citation='assumption')

#base sexual contact oes by race for the same race (black-black, hispanic-hispanic, other-other)
#these are average values from 4 different studies that are included in the pairing_input_manager
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.bb',
                                      3.76, 3.76*.75, 3.76*1.25,
                                      citation='assumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.hh',
                                      2.19, 2.19*.75, 2.19*1.25,
                                      citation='assumption')
SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER,
                                      'oe.sexual.byrace.oo',
                                      1.55, 1.55*.75, 1.55*1.25,
                                      citation='assumption')


SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.female.sexual.transmission',
                                       4.75, 2.4, 7.1,
                                       citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'female.to.male.sexual.transmission',
                                       3.75, 1.8, 5.6,
                                       citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'male.to.male.sexual.transmission',
                                       5, 2.5, 7.5,
                                       citation=26362321)

SHIELD_BASE_PARAMETER = add.parameter(SHIELD_BASE_PARAMETER, 'proportion.msm.sex.with.female',
                                       0.18, 0.18*5, 0.18*2,
                                       citation=9525438)
##-- STATE DURATIONS --##
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'primarySecondary.infection.duration',
#                                        3/12, 1/12, 6/12,
#                                        citation=0
# )
#
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'earlyLatent.infection.duration', #how to model it's dependancy on primarySecondary state?
#                                        9/12, 6/12, 11/12,
#                                        citation=0
# )
#
# SHIELD_BASE_PARAMETERS = add.parameter(SHIELD_BASE_PARAMETERS, 'lateLatent.infection.duration',
#                                        xx,xxx,xxx
#                                        citation=0
# )


SHIELD_BASE_PARAMETER_VALUES = SHIELD_BASE_PARAMETER$values

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

