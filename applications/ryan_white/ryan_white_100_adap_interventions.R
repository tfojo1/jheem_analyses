

ADAP.START.YEAR = 2026 + 2/12

set.seed(12345)

LOSE.ADAP.FRACTION = rep(1,1000)
dim(LOSE.ADAP.FRACTION) = c(1,1000)
dimnames(LOSE.ADAP.FRACTION) = list('lose.adap.fraction',NULL)

##-- CESSATION --##

# Effect on ADAP only -- survey

adap.cessation.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                             start.time = ADAP.START.YEAR,
                                                             effect.values = expression(1-lose.adap.fraction*lose.adap.expansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = ADAP.START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )

adap.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                start.time = ADAP.START.YEAR,
                                                                effect.values = expression(1-lose.adap.fraction*lose.adap.nonexpansion.effect),
                                                                apply.effects.as = 'value',
                                                                scale = 'proportion',
                                                                times = ADAP.START.YEAR + LOSS.LAG,
                                                                allow.values.less.than.otherwise = T,
                                                                allow.values.greater.than.otherwise = F )


adap.cessation = create.intervention(adap.cessation.expansion.effect,
                                     adap.cessation.nonexpansion.effect,
                                     parameters = rbind(
                                         RW.effect.values[c(1,4),],
                                         LOSE.ADAP.FRACTION),
                                     WHOLE.POPULATION, 
                                     code = paste0("adap.end.survey",rw.intervention.suffix))


# Effect on ADAP only -- survey + no transmission cost

no.transmission.effect = create.intervention.effect(quantity.name = 'global.trate',
                                             start.time = ADAP.START.YEAR,
                                             effect.values = 0,
                                             apply.effects.as = 'value',
                                             scale = 'proportion',
                                             times = ADAP.START.YEAR,
                                             allow.values.less.than.otherwise = T,
                                             allow.values.greater.than.otherwise = F )


adap.cessation.trate = create.intervention(adap.cessation.expansion.effect,
                                     adap.cessation.nonexpansion.effect,
                                     no.transmission.effect,
                                     parameters = rbind(
                                         RW.effect.values[c(1,4),],
                                         LOSE.ADAP.FRACTION),
                                     WHOLE.POPULATION, 
                                     code = paste0("adap.end.survey.no.trate",rw.intervention.suffix))



# Effect on ADAP only -- concrete 50% lose coverage

adap.cessation.50.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                             start.time = ADAP.START.YEAR,
                                                             effect.values = expression(1-lose.adap.fraction*0.5),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = ADAP.START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )



adap.cessation.50 = create.intervention(adap.cessation.50.effect,
                                     parameters = rbind(
                                         RW.effect.values[c(1,4),],
                                         LOSE.ADAP.FRACTION),
                                     WHOLE.POPULATION, 
                                     code = paste0("adap.end.50",rw.intervention.suffix))

# Effect on ADAP only -- concrete 50% lose coverage + no transmission cost


adap.cessation.50.trate = create.intervention(adap.cessation.50.effect,
                                           no.transmission.effect,
                                           parameters = rbind(
                                               RW.effect.values[c(1,4),],
                                               LOSE.ADAP.FRACTION),
                                           WHOLE.POPULATION, 
                                           code = paste0("adap.end.50.no.trate",rw.intervention.suffix))



