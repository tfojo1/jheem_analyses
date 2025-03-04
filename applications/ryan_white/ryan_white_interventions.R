# Modeling the loss Ryan White funding

source("../jheem_analyses/applications/ryan_white/ryan_white_specification.R")

#Interventions are scaled up linearly from July 1st of START.YEAR to October 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025.5
IMPLEMENTED.BY.YEAR = 2025.8

RW.effect.values = matrix(rnorm(1000,0.5,0.05), nrow = 1, dimnames = list("lose.adap.effect", NULL) )

# Complete Loss of ADAP
lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = "lose.adap.effect",
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )



# Complete Loss of OAHS
lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = "lose.oahs.effect" ,
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )


# Complete Loss of RW support
lose.rw.support.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = "lose.rw.support.effect",
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )



RW.effect.values = rbind(
  matrix(rnorm(1000,0.5,0.05), nrow = 1, dimnames = list("lose.adap.effect", NULL) ),
  matrix(rnorm(1000,0.5,0.05), nrow = 1, dimnames = list("lose.oahs.effect", NULL) ),
  matrix(rnorm(1000,0.5,0.05), nrow = 1, dimnames = list("lose.rw.support.effect", NULL) ))

lose.RW.intervention = create.intervention(lose.adap.effect,lose.oahs.effect,lose.rw.support.effect, parameters = RW.effect.values, WHOLE.POPULATION, code = "loseRW")

# Temporary Lapse of ADAP
temp.lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(c(0,0,lose.adap.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, 2028, 2028.25),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
temp.lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(c(0,0,lose.oahs.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, 2028, 2028.25),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
temp.lose.rw.support.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(c(0,0,lose.rw.support.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, 2028, 2028.25),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

temp.lose.RW.intervention = create.intervention(temp.lose.adap.effect,temp.lose.oahs.effect,temp.lose.rw.support.effect, parameters = RW.effect.values, WHOLE.POPULATION, code = "temploseRW")
