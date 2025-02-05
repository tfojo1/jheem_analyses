# the six interventions I'll use in the jheem web tool.

START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

WHOLE.POPULATION = create.target.population(name = 'Whole Population')

simset = get(load("../jheem_analyses/cached/temp/test.simset_2024.07.24.Rdata"))
simset = copy.simulation.set(simset) # to make sure is up to date

# Prep 10%
prep.ten.percent = create.intervention.effect(quantity.name = 'oral.prep.uptake',
                                              start.time = START.YEAR,
                                              effect.values = 0.1,
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F,
                                              allow.values.greater.than.otherwise = T
)
prep.ten.percent.intervention = create.intervention(WHOLE.POPULATION,
                                                    prep.ten.percent)
prep.ten.percent.intervention.simset = prep.ten.percent.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# Prep 25%
prep.twenty.five.percent = create.intervention.effect(quantity.name = 'oral.prep.uptake',
                                                      start.time = START.YEAR,
                                                      effect.values = 0.25,
                                                      apply.effects.as = 'value',
                                                      scale = 'proportion',
                                                      times = IMPLEMENTED.BY.YEAR,
                                                      allow.values.less.than.otherwise = F,
                                                      allow.values.greater.than.otherwise = T
)
prep.twenty.five.percent.intervention = create.intervention(WHOLE.POPULATION,
                                                            prep.twenty.five.percent)
prep.twenty.five.percent.intervention.simset = prep.twenty.five.percent.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# Suppression 80%
suppression.eighty.percent = create.intervention.effect(quantity.name = 'suppression.of.diagnosed',
                                                        start.time = START.YEAR,
                                                        effect.values = 0.8,
                                                        apply.effects.as = 'value',
                                                        scale = 'proportion',
                                                        times = IMPLEMENTED.BY.YEAR,
                                                        allow.values.less.than.otherwise = F,
                                                        allow.values.greater.than.otherwise = T
)
suppression.eighty.percent.intervention = create.intervention(WHOLE.POPULATION,
                                                              suppression.eighty.percent)
suppression.eighty.percent.intervention.simset = suppression.eighty.percent.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# Suppression 90%
suppression.ninety.percent = create.intervention.effect(quantity.name = 'suppression.of.diagnosed',
                                                        start.time = START.YEAR,
                                                        effect.values = 0.9,
                                                        apply.effects.as = 'value',
                                                        scale = 'proportion',
                                                        times = IMPLEMENTED.BY.YEAR,
                                                        allow.values.less.than.otherwise = F,
                                                        allow.values.greater.than.otherwise = T
)
suppression.ninety.percent.intervention = create.intervention(WHOLE.POPULATION,
                                                              suppression.ninety.percent)
suppression.ninety.percent.intervention.simset = suppression.ninety.percent.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# Testing once per year
testing.once.per.year = create.intervention.effect(quantity.name = 'general.population.testing',
                                                   start.time = START.YEAR,
                                                   effect.values = 1,
                                                   apply.effects.as = 'value',
                                                   scale = 'rate',
                                                   times = IMPLEMENTED.BY.YEAR,
                                                   allow.values.less.than.otherwise = F,
                                                   allow.values.greater.than.otherwise = T
)
testing.once.per.year.intervention = create.intervention(WHOLE.POPULATION,
                                                         testing.once.per.year)
testing.once.per.year.intervention.simset = testing.once.per.year.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# Testing twice per year
testing.twice.per.year = create.intervention.effect(quantity.name = 'general.population.testing',
                                                    start.time = START.YEAR,
                                                    effect.values = 2,
                                                    apply.effects.as = 'value',
                                                    scale = 'rate',
                                                    times = IMPLEMENTED.BY.YEAR,
                                                    allow.values.less.than.otherwise = F,
                                                    allow.values.greater.than.otherwise = T
)
testing.twice.per.year.intervention = create.intervention(WHOLE.POPULATION,
                                                          testing.twice.per.year)
testing.twice.per.year.intervention.simset = testing.twice.per.year.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# All the weaker ones
all.weaker.interventions = create.intervention(WHOLE.POPULATION,
                                               prep.ten.percent,
                                               suppression.eighty.percent,
                                               testing.once.per.year
)
all.weaker.interventions.simset = all.weaker.interventions$run(simset, start.year=2025, end.year=2035, verbose=T)

# All the stronger ones
all.stronger.interventions = create.intervention(WHOLE.POPULATION,
                                                 prep.twenty.five.percent,
                                                 suppression.ninety.percent,
                                                 testing.twice.per.year
)
all.stronger.interventions.simset = all.stronger.interventions$run(simset, start.year=2025, end.year=2035, verbose=T)

# No intervention
no.intervention = get.null.intervention()
no.intervention.simset = no.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)
