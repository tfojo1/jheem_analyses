ADAP.START.YEAR = 2026 + 2/12

set.seed(12345)
no.transmission.effect = create.intervention.effect(quantity.name = 'global.trate',
                                                    start.time = ADAP.START.YEAR,
                                                    effect.values = 0,
                                                    apply.effects.as = 'value',
                                                    scale = 'proportion',
                                                    times = ADAP.START.YEAR,
                                                    allow.values.less.than.otherwise = T,
                                                    allow.values.greater.than.otherwise = F )

# Effect on ADAP only -- concrete 50% lose coverage + no transmission cost
noint.no.trate = create.intervention(adap.cessation.50.effect,
                                     no.transmission.effect,
                                     parameters = rbind(
                                         RW.effect.values[c(1,4),],
                                         LOSE.ADAP.FRACTION),
                                     WHOLE.POPULATION, 
                                     code = paste0("noint.no.trate",rw.intervention.suffix))


adap.cessation.50.trate = create.intervention(adap.cessation.50.effect,
                                              no.transmission.effect,
                                              parameters = rbind(
                                                  RW.effect.values[c(1,4),],
                                                  LOSE.ADAP.FRACTION),
                                              WHOLE.POPULATION, 
                                              code = paste0("adap.end.50.no.trate",rw.intervention.suffix))



