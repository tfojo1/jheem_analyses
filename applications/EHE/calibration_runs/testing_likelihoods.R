
prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*FULL.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

race.one.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race"),
                                       levels.of.stratification = c(1), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

risk.one.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("risk"),
                                       levels.of.stratification = c(1), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )


age.one.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age"),
                                       levels.of.stratification = c(1), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )


sex.one.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("risk"),
                                       levels.of.stratification = c(1), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )


race.risk.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

age.sex.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

age.race.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

age.risk.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","risk"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )


race.sex.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race","sex"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

risk.sex.two.way.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("risk","sex"),
                                       levels.of.stratification = c(2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )
