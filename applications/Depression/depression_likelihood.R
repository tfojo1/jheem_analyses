## Depression Likelihood

source("applications/EHE/ehe_likelihoods.R")

propdep.bias.estimates = get.cached.object.for.version(name = "propdep.bias.estimates", 
                                                           version = 'dep')


DEP.PARTITIONING.FUNCTION = function(arr, version='dep', location)
{
  if ("risk" %in% names(dim(arr)) &&
      all(array.access(arr, risk='active_IDU')==array.access(arr, risk='IDU_in_remission')))
  {
    risk.partition.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
    risk.partition.arr = array(c(0.25, 0.75), dim=sapply(risk.partition.dimnames, length), risk.partition.dimnames)
    risk.modified = array.access(arr, risk.partition.dimnames)
    risk.modified = risk.modified * expand.array(risk.partition.arr, dimnames(risk.modified))
    array.access(arr, dimnames(risk.modified)) = risk.modified
  }
  if ("sex" %in% names(dim(arr)) &&
      all(array.access(arr, sex='msm')==array.access(arr, sex='heterosexual_male')))
  {
    specification.metadata = get.specification.metadata(version=version, location=location)
    proportion.msm = get.best.guess.msm.proportions(location,
                                                    specification.metadata = specification.metadata,
                                                    keep.age = any(names(dim(arr))=='age'),
                                                    keep.race = any(names(dim(arr))=='race'),
                                                    ages = dimnames(arr)$age)
    
    sex.partition.arr = c(as.numeric(proportion.msm), 1-as.numeric(proportion.msm))
    sex.partition.dimnames = c(dimnames(proportion.msm), list(sex=c('msm','heterosexual_male')))
    dim(sex.partition.arr) = sapply(sex.partition.dimnames, length)
    dimnames(sex.partition.arr) = sex.partition.dimnames
    
    sex.modified = array.access(arr, sex.partition.dimnames)
    sex.modified = sex.modified * expand.array(sex.partition.arr, dimnames(sex.modified))
    
    array.access(arr, dimnames(sex.modified)) = sex.modified
  }
  arr
  
}
## Similar to EHE partitioning function

proportionDep_likelihood_inst <- create.nested.proportion.likelihood.instructions(outcome.for.data = 'depression', outcome.for.sim = 'proportion.depressed', 
                                        denominator.outcome.for.data = 'adult.population', 
                                        location.types = c('STATE','COUNTY'),
                                        minimum.geographic.resolution.type = 'COUNTY',
                                        dimensions = c("age"), # total, but can stratify by age, race, etc
                                        levels.of.stratification = c(0,1), # c(0,1,2) levels of stratification, 0=total, 1=one layer, etc
                                        omit.years = c(2020), # can omit any years we do not want included
                                        p.bias.inside.location = 0,# propdep.bias.estimates$in.mean, # does the MSA value depend on county (inside)/state (outside) value? calculated automatically but can specify
                                        p.bias.sd.inside.location = propdep.bias.estimates$out.sd, # $in.sd
                                        p.bias.outside.location = propdep.bias.estimates$out.mean,
                                        p.bias.sd.outside.location = propdep.bias.estimates$out.sd,
                                        
                                        ##-- specify these through the "get.cached.object" f() // see ehe_bias_estimates_cache.R code 
                                        
                                        # within.location.p.error.correlation = , # p, proportion // n, population -- default at 0.5
                                        # within.location.n.error.correlation = , 
                                        # correlation.different.locations = , # defaults to 0, places independent
                                        # correlation.different.years = , # defaults >0
                                        # correlation.different.strata = , # defaults to >0
                                        # correlation.different.sources = , # defaults to >0 / differences across sources for same data
                                        # correlation.same.source.different.details = , # defaults to >0
                                        # observation.correlation.form = , # related to years, how do the different years impact correlation? compound.symmetry default
                                        p.error.variance.type = "sd", 
                                        p.error.variance.term = 0.03, 
                                        # n.error.variance.type =, # defaults
                                        # n.error.variance.term =, 
                                        
                                        # measurement.error.sd = 0.03, ## must input ## related to data uncertainty ## current input from Melissa's % tested // can use data sd
                                        # denominator.measurement.error.cv = , # defaults to 0.05
                                        
                                        # n.multiplier.cv = , # defaults to 0.1 ## how do the denominators compare, internally done by the model
                                        weights = NULL, # are there certain data-points that are more important?
                                        # equalize.weight.by.year = , # normalises data points by year, so all years are equally important
                                        partitioning.function = DEP.PARTITIONING.FUNCTION 
                                                                   # different sources will stratify the data differently from the model, 
                                                                   # we use mapping to reconcile data from sources to what the model stratifications are 
)


##
prev_ratio_inst <- create.basic.likelihood.instructions.with.specified.outcome(outcome.for.sim = 'prev_ratio_depression',
                                                        outcome.value = 3.1,
                                                        from.year = 2014, ## check
                                                        to.year = 2020, ## check
                                                        error.variance.term = 0.03, ## To do: calculate from data CI
                                                        error.variance.type = 'sd') 
# % PWH w dep on TX
hiv_depTx_inst <- create.basic.likelihood.instructions.with.specified.outcome(outcome.for.sim = 'hiv_depression_tx',
                                                        outcome.value = 0.184, 
                                                        from.year = 2013, ## check
                                                        to.year = 2020, ## check
                                                        error.variance.term = 0.03, ## To do: calculate from data CI
                                                        error.variance.type = 'sd') 
# % general pop w dep on Tx
pop_depTx_inst <- create.basic.likelihood.instructions.with.specified.outcome(outcome.for.sim = 'pop_depression_tx',
                                                       outcome.value = 0.65, 
                                                       from.year = 2018, ## check 
                                                       to.year = 2021, ## check
                                                       error.variance.term = 0.03, ## To do: calculate from data CI
                                                       error.variance.type = 'sd') 


##---- Computing likelihoods ----##
proportionDep_likelihood <- proportionDep_likelihood_inst$instantiate.likelihood(version='dep', location='C.12580') # can also be a diff location
# add simulation and compute
proportionDep_likelihood$compute(sim) # sim=sim name // log-LL default // always about comparing, so we run another likelihood 
# we can look at the likelihoods using simplot, visual check against the data-points // also helps decide whether our SD decisions are too strict or not


# will yield 4 numbers, estimate and SD for being being inside/outside of MSA                                  
# look at Melissa's code for this to get a sense of what to do // in EHE folder, ehe_bias_estimates_cache.R & ehe_likelihoods.R

dep_likelihood_full <- join.likelihood.instructions(FULL.likelihood.instructions.with.aids, proportionDep_likelihood_inst, 
                                                    prev_ratio_inst, hiv_depTx_inst, pop_depTx_inst)

