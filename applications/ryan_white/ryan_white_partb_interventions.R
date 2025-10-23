
source('applications/ryan_white/ryan_white_main.R')
#source('applications/ryan_white/ryan_white_interventions.R')
#source('applications/ryan_white/read_rw_costs.R')

PART.B.START.YEAR = 2026 + 1/12
PART.B.RESTART.YEAR = 2029


set.seed(12345)
FRACTION.PART.B.VARIATION = rlnorm(1000, meanlog=0, sdlog=log(1.25)/2)

# DEFINED IN SPECIFICATION: 
# proportion.adap.without.non.adap.rw

# DEFINED IN RYAN_WHITE_INTERVENTIONS:
# lose.adap.expansion.effect; lose.adap.nonexpansion.effect; 
# lose.oahs.expansion.effect; lose.oahs.nonexpansion.effect; 
# lose.rw.support.expansion.effect; lose.rw.support.nonexpansion.effect

state.rw.fraction.part.b.non.adap.of.all.non.adap = (state.rw.costs[,c('part.b.non.adap')]) / (state.rw.costs[,'total'] - state.rw.costs[,'part.b.adap'])


##-- CESSATION --##

# Effect on ADAP
part.b.cessation.adap.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                                    start.time = PART.B.START.YEAR,
                                                                    effect.values = expression(1-lose.adap.expansion.effect), 
                                                                    apply.effects.as = 'value',
                                                                    scale = 'proportion',
                                                                    times = PART.B.START.YEAR + LOSS.LAG,
                                                                    allow.values.less.than.otherwise = T,
                                                                    allow.values.greater.than.otherwise = F )

part.b.cessation.adap.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                       start.time = PART.B.START.YEAR,
                                                                       effect.values = expression(1-lose.adap.nonexpansion.effect), 
                                                                       apply.effects.as = 'value',
                                                                       scale = 'proportion',
                                                                       times = PART.B.START.YEAR + LOSS.LAG,
                                                                       allow.values.less.than.otherwise = T,
                                                                       allow.values.greater.than.otherwise = F )

# Effect on OAHS
part.b.cessation.oahs.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                                  start.time = PART.B.START.YEAR,
                                                                  effect.values = expression(1-lose.oahs.expansion.effect*fraction.part.b), # this was 1-lose.oahs in the original; some fraction 
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = PART.B.START.YEAR + LOSS.LAG,
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

part.b.cessation.oahs.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                     start.time = PART.B.START.YEAR,
                                                                     effect.values = expression(1-lose.oahs.nonexpansion.effect*fraction.part.b),
                                                                     apply.effects.as = 'value',
                                                                     scale = 'proportion',
                                                                     times = PART.B.START.YEAR + LOSS.LAG,
                                                                     allow.values.less.than.otherwise = T,
                                                                     allow.values.greater.than.otherwise = F )


# Effect on RW support
part.b.cessation.rw.support.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                        start.time = PART.B.START.YEAR,
                                                                        effect.values = expression(1-lose.rw.support.expansion.effect*fraction.part.b),
                                                                        apply.effects.as = 'value',
                                                                        scale = 'proportion',
                                                                        times = PART.B.START.YEAR + LOSS.LAG,
                                                                        allow.values.less.than.otherwise = T,
                                                                        allow.values.greater.than.otherwise = F )

part.b.cessation.rw.support.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                           start.time = PART.B.START.YEAR,
                                                                           effect.values = expression(1-lose.rw.support.nonexpansion.effect*fraction.part.b),
                                                                           apply.effects.as = 'value',
                                                                           scale = 'proportion',
                                                                           times = PART.B.START.YEAR + LOSS.LAG,
                                                                           allow.values.less.than.otherwise = T,
                                                                           allow.values.greater.than.otherwise = F )


part.b.rw.cessation = create.intervention(part.b.cessation.adap.expansion.effect,
                                          part.b.cessation.adap.nonexpansion.effect,
                                          part.b.cessation.oahs.expansion.effect,
                                          part.b.cessation.oahs.nonexpansion.effect,
                                          part.b.cessation.rw.support.expansion.effect, 
                                          part.b.cessation.rw.support.nonexpansion.effect, 
                                          parameters = RW.effect.values, 
                                          generate.parameters.function = function(n, parameters, sim){
                                              rbind(fraction.part.b = state.rw.fraction.part.b.non.adap.of.all.non.adap[sim$location] * FRACTION.PART.B.VARIATION[1:n])
                                          },
                                          WHOLE.POPULATION, 
                                          code = paste0("part.b.rw.end",rw.intervention.suffix))


