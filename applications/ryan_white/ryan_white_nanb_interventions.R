
NANB.START.YEAR = 2026 + 1/12
NANB.RESTART.YEAR = 2029


set.seed(12345)
FRACTION.NANB.VARIATION = rlnorm(1000, meanlog=0, sdlog=log(1.25)/2)


##-- CESSATION --##

# Effect on ADAP
nanb.cessation.adap.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                             start.time = NANB.START.YEAR,
                                                             effect.values = expression(1-lose.oahs.expansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw)),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = NANB.START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )

nanb.cessation.adap.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                start.time = NANB.START.YEAR,
                                                                effect.values = expression(1-lose.oahs.nonexpansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw)),
                                                                apply.effects.as = 'value',
                                                                scale = 'proportion',
                                                                times = NANB.START.YEAR + LOSS.LAG,
                                                                allow.values.less.than.otherwise = T,
                                                                allow.values.greater.than.otherwise = F )

# Effect on OAHS
nanb.cessation.oahs.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                                start.time = NANB.START.YEAR,
                                                                effect.values = expression(1-lose.oahs.expansion.effect*fraction.nanb),
                                                                apply.effects.as = 'value',
                                                                scale = 'proportion',
                                                                times = NANB.START.YEAR + LOSS.LAG,
                                                                allow.values.less.than.otherwise = T,
                                                                allow.values.greater.than.otherwise = F )

nanb.cessation.oahs.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                   start.time = NANB.START.YEAR,
                                                                   effect.values = expression(1-lose.oahs.nonexpansion.effect*fraction.nanb),
                                                                   apply.effects.as = 'value',
                                                                   scale = 'proportion',
                                                                   times = NANB.START.YEAR + LOSS.LAG,
                                                                   allow.values.less.than.otherwise = T,
                                                                   allow.values.greater.than.otherwise = F )


# Effect on RW support
nanb.cessation.rw.support.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                      start.time = NANB.START.YEAR,
                                                                      effect.values = expression(1-lose.rw.support.expansion.effect*fraction.nanb),
                                                                      apply.effects.as = 'value',
                                                                      scale = 'proportion',
                                                                      times = NANB.START.YEAR + LOSS.LAG,
                                                                      allow.values.less.than.otherwise = T,
                                                                      allow.values.greater.than.otherwise = F )

nanb.cessation.rw.support.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                         start.time = NANB.START.YEAR,
                                                                         effect.values = expression(1-lose.rw.support.nonexpansion.effect*fraction.nanb),
                                                                         apply.effects.as = 'value',
                                                                         scale = 'proportion',
                                                                         times = NANB.START.YEAR + LOSS.LAG,
                                                                         allow.values.less.than.otherwise = T,
                                                                         allow.values.greater.than.otherwise = F )

nanb.rw.cessation = create.intervention(nanb.cessation.adap.expansion.effect,
                                        nanb.cessation.adap.nonexpansion.effect,
                                        nanb.cessation.oahs.expansion.effect,
                                        nanb.cessation.oahs.nonexpansion.effect,
                                        nanb.cessation.rw.support.expansion.effect, 
                                        nanb.cessation.rw.support.nonexpansion.effect, 
                                        parameters = RW.effect.values[c(2,3,5,6),],
                                        generate.parameters.function = function(n, parameters, sim){
                                            rbind(fraction.nanb = state.rw.fraction.c.d.ehe.minority.of.non.adap[sim$location] * FRACTION.NANB.VARIATION[1:n])
                                        },
                                        WHOLE.POPULATION, 
                                        code = paste0("nanb.rw.end",rw.intervention.suffix))


##-- INTERRUPTION --##

# Effect on ADAP
nanb.interruption.adap.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                                  start.time = NANB.START.YEAR,
                                                                  end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.oahs.expansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw),
                                                                                               1-lose.oahs.expansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw))),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

nanb.interruption.adap.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                     start.time = NANB.START.YEAR,
                                                                     end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                     effect.values = expression(c(1-lose.oahs.nonexpansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw),
                                                                                                  1-lose.oahs.nonexpansion.effect*fraction.nanb*(1-proportion.adap.without.non.adap.rw))),
                                                                     apply.effects.as = 'value',
                                                                     scale = 'proportion',
                                                                     times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),,
                                                                     allow.values.less.than.otherwise = T,
                                                                     allow.values.greater.than.otherwise = F )

# Effect on OAHS
nanb.interruption.oahs.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                                  start.time = NANB.START.YEAR,
                                                                  end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.oahs.expansion.effect*fraction.nanb,
                                                                                               1-lose.oahs.expansion.effect*fraction.nanb)),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),,
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

nanb.interruption.oahs.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                     start.time = NANB.START.YEAR,
                                                                     end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                     effect.values = expression(c(1-lose.oahs.nonexpansion.effect*fraction.nanb,
                                                                                                  1-lose.oahs.nonexpansion.effect*fraction.nanb)),
                                                                     apply.effects.as = 'value',
                                                                     scale = 'proportion',
                                                                     times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),,
                                                                     allow.values.less.than.otherwise = T,
                                                                     allow.values.greater.than.otherwise = F )


# Effect on RW support
nanb.interruption.rw.support.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                        start.time = NANB.START.YEAR,
                                                                        end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                        effect.values = expression(c(1-lose.rw.support.expansion.effect*fraction.nanb,
                                                                                                     1-lose.rw.support.expansion.effect*fraction.nanb)),
                                                                        apply.effects.as = 'value',
                                                                        scale = 'proportion',
                                                                        times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),,
                                                                        allow.values.less.than.otherwise = T,
                                                                        allow.values.greater.than.otherwise = F )

nanb.interruption.rw.support.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                           start.time = NANB.START.YEAR,
                                                                           end.time = NANB.RESTART.YEAR + RESTART.LAG,
                                                                           effect.values = expression(c(1-lose.rw.support.nonexpansion.effect*fraction.nanb,
                                                                                                        1-lose.rw.support.nonexpansion.effect*fraction.nanb)),
                                                                           apply.effects.as = 'value',
                                                                           scale = 'proportion',
                                                                           times = c(NANB.START.YEAR + LOSS.LAG, NANB.RESTART.YEAR),,
                                                                           allow.values.less.than.otherwise = T,
                                                                           allow.values.greater.than.otherwise = F )

nanb.rw.interruption = create.intervention(nanb.interruption.adap.expansion.effect,
                                           nanb.interruption.adap.nonexpansion.effect,
                                           nanb.interruption.oahs.expansion.effect,
                                           nanb.interruption.oahs.nonexpansion.effect,
                                           nanb.interruption.rw.support.expansion.effect, 
                                           nanb.interruption.rw.support.nonexpansion.effect, 
                                           parameters = RW.effect.values[c(2,3,5,6),],
                                           generate.parameters.function = function(n, parameters, sim){
                                               rbind(fraction.nanb = state.rw.fraction.c.d.ehe.minority.of.non.adap[sim$location] * FRACTION.NANB.VARIATION[1:n])
                                           },
                                           WHOLE.POPULATION, 
                                           code = paste0("nanb.rw.intr",rw.intervention.suffix))
