
# DEFINED IN RYAN_WHITE_INTERVENTIONS:
# lose.adap.expansion.effect; lose.adap.nonexpansion.effect
# defined in RW.effect.values = get_suppresion_effect_arcsin(1000, rw_survey)

ADAP.START.YEAR = 2026 + 2/12



set.seed(12345)

# Setting up these vectors of 1,000 values helps us in the future if we want to sample them

# Assume 50% of ADAP recipients on Biktarvy 
ADAP.LOSE.BIK.FRACTION = rep(0.5,1000)
dim(ADAP.LOSE.BIK.FRACTION) = c(1,1000)
dimnames(ADAP.LOSE.BIK.FRACTION) = list('adap.lose.bik.fraction',NULL)

# Assume 25% of those who lose Biktarvy will lose their viral suppression (no difference by expansion/non-expansion)
LOSE.BIK.EXPANSION.EFFECT = rep(0.25,1000)
dim(LOSE.BIK.EXPANSION.EFFECT) = c(1,1000)
dimnames(LOSE.BIK.EXPANSION.EFFECT) = list('lose.bik.expansion.effect',NULL)

LOSE.BIK.NONEXPANSION.EFFECT = rep(0.25,1000)
dim(LOSE.BIK.NONEXPANSION.EFFECT) = c(1,1000)
dimnames(LOSE.BIK.NONEXPANSION.EFFECT) = list('lose.bik.nonexpansion.effect',NULL)

adap.biktarvy.cessation.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                                      start.time = ADAP.START.YEAR,
                                                                      #effect.values = (1-0.5*0.25), # = 0.875
                                                                      # if I do it this ^ way, explicitly, can comment out the parameters line in the create intervention code below
                                                                      effect.values = expression(1-adap.lose.bik.fraction*lose.bik.expansion.effect), # (1-0.5*0.25) = 0.875
                                                                      # but this ^ way allow me to set it up later to sample this value 
                                                                      apply.effects.as = 'value',
                                                                      scale = 'proportion',
                                                                      times = ADAP.START.YEAR + LOSS.LAG,
                                                                      allow.values.less.than.otherwise = T,
                                                                      allow.values.greater.than.otherwise = F )

adap.biktarvy.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                         start.time = ADAP.START.YEAR,
                                                                         effect.values = expression(1-adap.lose.bik.fraction*lose.bik.nonexpansion.effect),
                                                                         apply.effects.as = 'value',
                                                                         scale = 'proportion',
                                                                         times = ADAP.START.YEAR + LOSS.LAG,
                                                                         allow.values.less.than.otherwise = T,
                                                                         allow.values.greater.than.otherwise = F )


adap.biktarvy.cessation = create.intervention(adap.biktarvy.cessation.expansion.effect,
                                              adap.biktarvy.cessation.nonexpansion.effect,
                                              parameters = rbind(
                                                  LOSE.BIK.EXPANSION.EFFECT,
                                                  LOSE.BIK.NONEXPANSION.EFFECT,
                                                  ADAP.LOSE.BIK.FRACTION),
                                              WHOLE.POPULATION, 
                                              code = paste0("rw.bik.end",rw.intervention.suffix))


