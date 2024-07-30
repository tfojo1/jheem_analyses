
### Source model specification: structural assumptions for the core jheem model 
source("applications/EHE/ehe_specification.R") 

START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

#1- make the target population for the intervention 
# jheemanalysis/commoncode
BLACK = create.target.population(race='black', name='Black')

#No intervention
# to create a baseline no intervention object 
noint=get.null.intervention()

# with set multiplier
# how would I know if the intervention values could fo above 1?

# it assumes a linear scale up from baseline to values specified in times 
prep.increase.fix = create.intervention.effect(quantity.name = 'oral.prep.uptake', #% initiated PrEP
                                           start.time = START.YEAR,
                                           times = IMPLEMENTED.BY.YEAR, #is vector of some values to mark changes in scale up
                                           scale = 'proportion', 
                                           
                                           # option1: X times * baseline levels
                                           effect.values = 2, 
                                           apply.effects.as = 'multiplier', 
                                           
                                           # option2:setting PrEP to 25%
                                           # effect.values = 25%,
                                           # apply.effects.as = 'value', 
                                           
                                           # option3: add 5% on top of each stratum
                                           # effect.values = 5%,
                                           # apply.effects.as = 'addend', 
                                           
                                           # option4: if multiply would fall over 1 we can do this:
                                           # X times * baseline odes then convert to proportion
                                           # effect.values = 2, 
                                           # apply.effects.as = 'multiplier'
                                           # scale = 'odds', 
                                           
                                           allow.values.less.than.otherwise = F, # ??
                                           allow.values.greater.than.otherwise = T # if a groups's coverage is greater than target, it will keep it as is
)

# construct the intervention object
# create.intervention (..., the first two entries are the target population and intervention effects)
# Jheem2/intervention_main.R
# it's not returning it to the local memory-saving it as an R6class?? 
#Todd: how do I know which interventions defined so far? 
#Todd: how do I rewrite an intervention: when I created it again, it didn't let me rewrite it.
create.intervention(BLACK,
                    prep.increase.fix,
                    code="black.prep.fix")


# with range of multipliers
prep.increase.uniform = create.intervention.effect(quantity.name = 'oral.prep.uptake', #% initiated PrEP
                                           start.time = START.YEAR,
                                           effect.values = 'prep.multiplier',
                                           scale = 'proportion', 
                                           apply.effects.as = 'multiplier', 
                                           times = IMPLEMENTED.BY.YEAR,
                                           allow.values.less.than.otherwise = F, 
                                           allow.values.greater.than.otherwise = T)

create.intervention(BLACK,
                    prep.increase.uniform,
                    parameter.distribution = Uniform.Distribution(1.5,3,var.name = 'prep.multiplier'),
                    code="black.prep.uniform")


