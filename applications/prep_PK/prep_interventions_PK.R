START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

BLACK = create.target.population(race='black', name='Black')

#No intervention
noint=get.null.intervention()

# with set multiplier
prep.increase = create.intervention.effect('oral.prep.uptake', #% initiated PrEP
                                           start.time = START.YEAR,
                                           effect.values = 2, 
                                           scale = 'proportion', 
                                           apply.effects.as = 'multiplier', 
                                           times = IMPLEMENTED.BY.YEAR,
                                           allow.values.less.than.otherwise = F, 
                                           allow.values.greater.than.otherwise = T)

prep.intervention = create.intervention(BLACK,
                                        prep.increase,
                                        code="black.prep")


# with range of multipliers
prep.increase = create.intervention.effect('oral.prep.uptake', #% initiated PrEP
                                           start.time = START.YEAR,
                                           effect.values = 2, #'prep.multiplier',
                                           scale = 'proportion', 
                                           apply.effects.as = 'multiplier', 
                                           times = IMPLEMENTED.BY.YEAR,
                                           allow.values.less.than.otherwise = F, 
                                           allow.values.greater.than.otherwise = T)

prep.intervention = create.intervention(BLACK,
                                        prep.increase,
                                        parameter.distribution = Uniform.Distribution(1.5,3),
                                        code="black.prep")