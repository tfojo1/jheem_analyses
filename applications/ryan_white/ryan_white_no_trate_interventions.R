ADAP.START.YEAR = 2026 + 2/12

set.seed(12345)
LOSE.ADAP.FRACTION = rep(1,1000)
dim(LOSE.ADAP.FRACTION) = c(1,1000)
dimnames(LOSE.ADAP.FRACTION) = list('lose.adap.fraction',NULL)

no.transmission.effect = create.intervention.effect(
    quantity.name = 'global.trate',
    start.time = ADAP.START.YEAR,
    effect.values = 0,
    apply.effects.as = 'value',
    scale = 'proportion',
    times = ADAP.START.YEAR,
    allow.values.less.than.otherwise = TRUE,
    allow.values.greater.than.otherwise = FALSE
)



adap.coverage.50.exp.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                         start.time = ADAP.START.YEAR,
                                                         effect.values = expression(1-lose.adap.fraction*0.5 + 0*lose.adap.expansion.effect),
                                                         apply.effects.as = 'value',
                                                         scale = 'proportion',
                                                         times = ADAP.START.YEAR + LOSS.LAG,
                                                         allow.values.less.than.otherwise = T,
                                                         allow.values.greater.than.otherwise = F )
adap.coverage.50.nexp.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                          start.time = ADAP.START.YEAR,
                                                          effect.values = expression(1-lose.adap.fraction*0.5 + 0*lose.adap.nonexpansion.effect),
                                                          apply.effects.as = 'value',
                                                          scale = 'proportion',
                                                          times = ADAP.START.YEAR + LOSS.LAG,
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F )


noint.no.trate = create.intervention(no.transmission.effect,
                                     parameters = rbind(
                                         RW.effect.values[c(1,4),],
                                         LOSE.ADAP.FRACTION),
                                     WHOLE.POPULATION, 
                                     code = paste0("noint.no.trate",rw.intervention.suffix))



adap.coverage.50.no.trate = create.intervention(no.transmission.effect, 
                                                adap.coverage.50.exp.effect,
                                                adap.coverage.50.nexp.effect,
                                                parameters = rbind(
                                                    RW.effect.values[c(1,4),],
                                                    LOSE.ADAP.FRACTION),
                                                WHOLE.POPULATION, 
                                                code = paste0("adap.end.50.no.trate",rw.intervention.suffix))



