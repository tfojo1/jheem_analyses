
ADAP.START.YEAR = 2026 + 6/12
#ADAP.RESTART.YEAR = 2029


set.seed(12345)
#FRACTION.NANB.VARIATION = rlnorm(1000, meanlog=0, sdlog=log(1.25)/2)

LOSE.ADAP.FRACTION = rep(0.5,1000)
dim(LOSE.ADAP.FRACTION) = c(1,1000)
dimnames(LOSE.ADAP.FRACTION) = list('lose.adap.fraction',NULL)

##-- CESSATION --##

# Effect on ADAP

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
                                        code = paste0("adap.50.end",rw.intervention.suffix))


adap.cessation.cons = create.intervention(adap.cessation.expansion.effect,
                                             adap.cessation.nonexpansion.effect,
                                             parameters = rbind(
                                                 adjusted.RW.effect.values[c(1,4),],
                                                 LOSE.ADAP.FRACTION),
                                             WHOLE.POPULATION, 
                                             code = paste0("adap.50.end.cons",rw.intervention.suffix))
