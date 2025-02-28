source('applications/EHE/ehe_likelihoods.R')
rw.non.adap.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "non.adap.clients", 
  outcome.for.sim = "non.adap.clients",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
  correlation.different.years = 0.5, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)



rw.oahs.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.clients", 
  outcome.for.sim = "oahs.clients",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
  correlation.different.years = 0.5, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)

rw.oahs.suppression.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.suppression", 
  outcome.for.sim = "oahs.suppression",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
  correlation.different.years = 0.5, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)


rw.adap.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.proportion",
                                                   outcome.for.sim = "adap.proportion",
                                                   denominator.outcome.for.data = 'non.adap.clients',
                                                   
                                                   location.types = c('STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 2, # default
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = 0,
                                                   p.bias.sd.inside.location = 0.05,
                                                   p.bias.sd.outside.location = 0.05,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.05, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = 1,
                                                   equalize.weight.by.year = T
  )

rw.adap.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.suppression",
                                                   outcome.for.sim = "adap.suppression",
                                                   denominator.outcome.for.data = 'adap.clients',
                                                   
                                                   location.types = c('STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 2, # default
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.05, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = 1,
                                                   equalize.weight.by.year = T
  )

ryan.white.likelihood.instructions = join.likelihood.instructions(
  
#    rw.adap.likelihood.instructions,
#    rw.adap.suppression.likelihood.instructions,
  
    rw.non.adap.likelihood.instructions,
    rw.oahs.likelihood.instructions,
    rw.oahs.suppression.likelihood.instructions
    
)
