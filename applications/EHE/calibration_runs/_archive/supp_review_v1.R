
age.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(1), 
                                                   from.year = 2008, 
                                                   
                                                   maximum.locations.per.type = 2,
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

sex.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   dimensions = c("sex"),
                                                   levels.of.stratification = c(1), 
                                                   from.year = 2008, 
                                                   
                                                   maximum.locations.per.type = 2,
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

race.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   dimensions = c("race"),
                                                   levels.of.stratification = c(1), 
                                                   from.year = 2008, 
                                                   
                                                   maximum.locations.per.type = 2,
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

risk.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   # want to be able to specify max # for each location type;
                                                   # have to decide how to order (probably by denominator)
                                                   location.types = c('COUNTY','STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   # limit.to.n.location
                                                   
                                                   dimensions = c("risk"),
                                                   levels.of.stratification = c(1), 
                                                   from.year = 2008, 
                                                   
                                                   maximum.locations.per.type = 2,
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = list(1), 
                                                   equalize.weight.by.year = T 
  )

nyc.supp.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
nyc.tot.supp.lik = TOTAL.suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
nyc.age.supp.lik = age.suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
nyc.sex.supp.lik = sex.suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
nyc.race.supp.lik = race.suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
nyc.risk.supp.lik = risk.suppression.likelihood.instructions$instantiate.likelihood('ehe','C.35620')

exp(nyc.supp.lik$compute(simset.new$last.sim()) - nyc.supp.lik$compute(simset.old$last.sim())) # says new is better
exp(nyc.tot.supp.lik$compute(simset.new$last.sim()) - nyc.tot.supp.lik$compute(simset.old$last.sim())) # says new is better, maybe the trend? 
exp(nyc.age.supp.lik$compute(simset.new$last.sim()) - nyc.age.supp.lik$compute(simset.old$last.sim())) # says new is worse
exp(nyc.sex.supp.lik$compute(simset.new$last.sim()) - nyc.sex.supp.lik$compute(simset.old$last.sim())) # says new is worse
exp(nyc.race.supp.lik$compute(simset.new$last.sim()) - nyc.race.supp.lik$compute(simset.old$last.sim())) # says new is worse 
exp(nyc.risk.supp.lik$compute(simset.new$last.sim()) - nyc.risk.supp.lik$compute(simset.old$last.sim())) # says new is better, possible

exp(nyc.lik$compute.piecewise(simset.new$last.sim()) - nyc.lik$compute.piecewise(simset.new$first.sim())) 
exp(nyc.tot.supp.lik$compute(simset.new$last.sim()) - nyc.tot.supp.lik$compute(simset.new$first.sim())) 

simplot(#simset.old$last.sim(),
        simset.new$first.sim(),
        simset.new$last.sim(),
        #facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
