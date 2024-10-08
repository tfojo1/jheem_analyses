# LIKELIHOODS INCLUDED: 
# population, immigration, emigration, new diagnoses, prevalence, hiv mortality, general mortality, 
# AIDS diagnoses, AIDS deaths, suppression, proportion.tested, hiv.test.positivity
# heroin, cocaine

#' @title shield.partitioning.function
#' @description  ????
#' @param arr ???
#' @param version version
#' @param location location
#' @return ????
SHIELD.PARTITIONING.FUNCTION = function(arr, version='shield', location){
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

# redo this when we get the new data 
# pop.year.cvs = .015*seq(1,2,length=10)
# pop.year.cvs = c(pop.year.cvs,pop.year.cvs)
# names(pop.year.cvs) = 2010:2029

#-- POPULATION----
population.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2010, #@TODD: why 2010? 
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       
                                       # assumes correlation between all combos of years is the same
                                       observation.correlation.form = 'autoregressive.1', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
                                       error.variance.term = 0.015, 
                                       #error.variance.term = pop.year.cvs,  
                                       error.variance.type = 'cv',
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       # weights = list(create.likelihood.weights(
                                       #   total.weight = 0.5,
                                       #   dimension.values = list(year = as.character(2007:2014)))), 
                                       
                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = F
  )


#-- IMMIGRATION----
# immigration.likelihood.instructions = 
#   create.basic.likelihood.instructions(outcome.for.data = "immigration", 
#                                        outcome.for.sim = "immigration",
#                                        dimensions = c("age","race"), 
#                                        levels.of.stratification = c(0,1),
#                                        from.year = 2011, 
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
#                                        error.variance.type = 'cv',
#                                        weights = 1,
#                                        equalize.weight.by.year = T 
#   )
# 
# #-- EMIGRATION----
# emigration.likelihood.instructions = 
#   create.basic.likelihood.instructions(outcome.for.data = "emigration", 
#                                        outcome.for.sim = "emigration",
#                                        dimensions = c("age","race"), 
#                                        levels.of.stratification = c(0,1),
#                                        from.year = 2011, 
#                                        observation.correlation.form = 'compound.symmetry', 
#                                        error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
#                                        error.variance.type = 'cv',
#                                        weights = 1,
#                                        equalize.weight.by.year = T 
#   )

#-- FULL LIKELIHOODS --# ----
FULL.likelihood.instructions =  join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions 
  # immigration.likelihood.instructions, 
  # emigration.likelihood.instructions
)