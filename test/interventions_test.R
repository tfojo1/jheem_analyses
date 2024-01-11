

# See commoncode/target_populations.R for examples of setting up target populations

PREP.50 = create.intervention.effect('prep.coverage',
                                     start.time = 2025,
                                     effect.values = 0.5,
                                     times = 2030,
                                     apply.effects.as = 'value',
                                     scale = 'proportion',
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

TESTING.DOUBLE = create.intervention.effect('testing',
                                            start.time = 2025,
                                            effect.values = 2,
                                            times = 2030,
                                            apply.effects.as = 'multiplier',
                                            scale = 'rate',
                                            allow.values.less.than.otherwise = F,
                                            allow.values.greater.than.otherwise = T)

EXAMPLE.INTERVENTION = create.intervention(PREP.50, TESTING.DOUBLE, WHOLE.POPULATION,
                                           code = 'eg.intervention', 
                                           name = "PrEP 50% coverage and testing doubled by 2030")

PREP.UPTAKE = create.intervention.effect('prep.uptake',
                                                     start.time = 2025,
                                                     effect.values = 'prep.or',
                                                     times = 2030,
                                                     apply.effects.as = 'multiplier',
                                                     scale = 'odds',
                                                     allow.values.less.than.otherwise = F,
                                                     allow.values.greater.than.otherwise = T)
PREP.PERSISTENCE = create.intervention.effect('prep.persistence',
                                              start.time = 2025,
                                              effect.values = expression(prep.persistence.delta + 1),
                                              times = 2030,
                                              apply.effects.as = 'multiplier',
                                              scale = 'proportion',
                                              allow.values.less.than.otherwise = F,
                                              allow.values.greater.than.otherwise = T)
EG2.WITH.PARAMETERS = create.intervention(PREP.PERSISTENCE,
                                          PREP.UPTAKE,
                                          WHOLE.POPULATION,
                                          parameter.distribution = join.distributions(
                                              prep.or = Lognormal.Distribution(0, 1),
                                              prep.persistence.delta = Lognormal.Distribution(0, 2)
                                          ))

EG.JOINT = join.interventions(EXAMPLE.INTERVENTION, EG2.WITH.PARAMETERS, code='joint.test.int')
