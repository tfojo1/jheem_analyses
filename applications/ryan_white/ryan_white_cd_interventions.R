# Complete Loss of OAHS
cd.oahs.cessation.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                                start.time = START.YEAR,
                                                                effect.values = expression(1-lose.oahs.expansion.effect*fraction.oahs.on.cd),
                                                                apply.effects.as = 'value',
                                                                scale = 'proportion',
                                                                times = START.YEAR + LOSS.LAG,
                                                                allow.values.less.than.otherwise = T,
                                                                allow.values.greater.than.otherwise = F )

cd.oahs.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                   start.time = START.YEAR,
                                                                   effect.values = expression(1-lose.oahs.nonexpansion.effect*fraction.oahs.on.cd),
                                                                   apply.effects.as = 'value',
                                                                   scale = 'proportion',
                                                                   times = START.YEAR + LOSS.LAG,
                                                                   allow.values.less.than.otherwise = T,
                                                                   allow.values.greater.than.otherwise = F )


# Complete Loss of RW support
cd.rw.support.cessation.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                      start.time = START.YEAR,
                                                                      effect.values = expression(1-lose.rw.support.expansion.effect*fraction.rw.support.on.cd),
                                                                      apply.effects.as = 'value',
                                                                      scale = 'proportion',
                                                                      times = START.YEAR + LOSS.LAG,
                                                                      allow.values.less.than.otherwise = T,
                                                                      allow.values.greater.than.otherwise = F )

cd.rw.support.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                         start.time = START.YEAR,
                                                                         effect.values = expression(1-lose.rw.support.nonexpansion.effect*fraction.rw.support.on.cd),
                                                                         apply.effects.as = 'value',
                                                                         scale = 'proportion',
                                                                         times = START.YEAR + LOSS.LAG,
                                                                         allow.values.less.than.otherwise = T,
                                                                         allow.values.greater.than.otherwise = F )
sampled.cd.fraction = rnorm(1000, mean=0.2, sd=0.025)
cd.fraction.values = rbind(
    fraction.oahs.on.cd = sampled.cd.fraction,
    fraction.rw.support.on.cd = sampled.cd.fraction
)

cd.rw.cessation = create.intervention(cd.oahs.cessation.expansion.effect,
                                      cd.oahs.cessation.nonexpansion.effect,
                                      cd.rw.support.cessation.expansion.effect, 
                                      cd.rw.support.cessation.nonexpansion.effect, 
                                      parameters = rbind(RW.effect.values[c(2,3,5,6),], cd.fraction.values), 
                                      WHOLE.POPULATION, 
                                      code = paste0("cd.rw.end",rw.intervention.suffix))