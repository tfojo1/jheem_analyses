
FL.ADAP.START.YEAR = 2026 + 2/12
FL.ADAP.RESTART.YEAR = 2029


set.seed(12345)
#FRACTION.NANB.VARIATION = rlnorm(1000, meanlog=0, sdlog=log(1.25)/2)

FL.LOSE.ADAP.FRACTION = rep(0.5,1000)
dim(FL.LOSE.ADAP.FRACTION) = c(1,1000)
dimnames(FL.LOSE.ADAP.FRACTION) = list('lose.adap.fraction',NULL)

##-- CESSATION --##

# Effect on ADAP

fl.adap.cessation.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                             start.time = FL.ADAP.START.YEAR,
                                                             effect.values = expression(1-lose.adap.fraction*lose.adap.expansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = FL.ADAP.START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )

fl.adap.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                             start.time = FL.ADAP.START.YEAR,
                                                             effect.values = expression(1-lose.adap.fraction*lose.adap.nonexpansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = FL.ADAP.START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )


fl.adap.cessation = create.intervention(fl.adap.cessation.expansion.effect,
                                           fl.adap.cessation.nonexpansion.effect,
                                           parameters = rbind(
                                               RW.effect.values[c(1,4),],
                                               FL.LOSE.ADAP.FRACTION),
                                           WHOLE.POPULATION, 
                                           code = paste0("fl.adap.end",rw.intervention.suffix))


fl.adap.cessation.cons = create.intervention(fl.adap.cessation.expansion.effect,
                                        fl.adap.cessation.nonexpansion.effect,
                                        parameters = rbind(
                                            adjusted.RW.effect.values[c(1,4),],
                                            FL.LOSE.ADAP.FRACTION),
                                        WHOLE.POPULATION, 
                                        code = paste0("fl.adap.end.cons",rw.intervention.suffix))
