source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/EHE/ehe_aids_proportions_likelihood.R')
# LIKELIHOODS INCLUDED: 
# population, immigration, emigration, new diagnoses, prevalence, hiv mortality, general mortality, 
# AIDS diagnoses, AIDS deaths, suppression, proportion.tested, hiv.test.positivity
# heroin, cocaine

#TOTAL.WEIGHT = 0.5 # universally downweighting to try to allow more mixing 
POPULATION.WEIGHT = 1
TRANSMISSION.WEIGHT = 1
FULL.WEIGHT = 1

DIAGNOSES.ERROR.TERM  = 0.05368198 # for new diagnoses and aids diagnoses; from calculating_error_terms_for_ehe_likelihoods.R
PREVALENCE.ERROR.TERM = 0.08384422 # for prevalence and hiv.mortality; from calculating_error_terms_for_ehe_likelihoods.R

DIAGNOSES.CV = 0.03331971 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnoses", output='cv.and.exponent.of.variance')
DIAGNOSES.EXP.OF.VAR = 0.3893292 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnoses", output='cv.and.exponent.of.variance')
PREVALENCE.CV = 0.07956432 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.fixed.exponent.of.variance',PREVALENCE.EXP.OF.VAR)
PREVALENCE.EXP.OF.VAR = 0.590671901063418 #from error_for_prevalence_formula.R
  
DIAGNOSES.CV.STATE = mean(c(DIAGNOSES.ERROR.TERM, 0.05581213))
PREVALENCE.CV.STATE = mean(c(PREVALENCE.ERROR.TERM, 0.0377412))

#-- BIAS ESTIMATES FOR NESTED PROPORTIONS  ----
suppression.bias.estimates = get.cached.object.for.version(name = "suppression.bias.estimates", 
                                                           version = 'ehe')

proportion.tested.bias.estimates = get.cached.object.for.version(name = "proportion.tested.bias.estimates", 
                                                                 version = 'ehe')  

hiv.test.positivity.bias.estimates = get.cached.object.for.version(name = "hiv.test.positivity.bias.estimates", 
                                                                   version = 'ehe')  

awareness.bias.estimates = get.cached.object.for.version(name = "awareness.bias.estimates", 
                                                         version = 'ehe')  

cocaine.bias.estimates = get.cached.object.for.version(name = "cocaine.bias.estimates", 
                                                       version = 'ehe')  

heroin.bias.estimates = get.cached.object.for.version(name = "heroin.bias.estimates", 
                                                      version = 'ehe')  

hiv.tests.per.population.bias.estimates = get.cached.object.for.version(name = "hiv.tests.per.population.bias.estimates", 
                                                                        version = 'ehe')    


EHE.PARTITIONING.FUNCTION = function(arr, version='ehe', location)
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
    #sex.partition.dimnames = list(sex = c('heterosexual_male', 'msm'))
    #sex.partition.arr = array(c(0.5, 0.5), dim=sapply(sex.partition.dimnames, length), sex.partition.dimnames)
    
    specification.metadata = get.specification.metadata(version=version, location=location)
    proportion.msm = get.best.guess.msm.proportions(location,
                                                    specification.metadata = specification.metadata,
                                                    keep.age = any(names(dim(arr))=='age'),
                                                    keep.race = any(names(dim(arr))=='race'),
                                                    ages = dimnames(arr)$age)
    # sex.partition.arr = get.best.guess.msm.proportions.by.race(location,
    #                                                            specification.metadata = specification.metadata,
    #                                                            years = DEFAULT.POPULATION.YEARS,
    #                                                            min.age = specification.metadata$age.lower.bounds[1],
    #                                                            return.proportions = T)
    
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

#-- POPULATION  ----

population.error.sd.fn = function(data, details=attr(data, 'details'), version, location)
{
    # Massage the data a big
    melted.data = reshape2::melt(data)
    years.since.preceding.census = melted.data$year %% 10
    years.from.nearest.census = pmin(years.since.preceding.census, -melted.data$year %% 10)
      
    # Set up error terms
    inherent.census.cv = 0.015
    
    stratified.dimension.candidates = c('age','race','sex')
    n.stratified.dimensions = length(intersect(names(dim(data)), stratified.dimension.candidates))
    
    if (n.stratified.dimensions<=1)
        max.post.censal.cv = 0.1561269 # from calculating_error_terms_for_ehe_likelihoods.R
    else
        max.post.censal.cv = 0.1939618
    
    max.post.censal.var = inherent.census.cv^2 + max.post.censal.cv^2
    
    post.censal.cv = exp(log(inherent.census.cv) + years.since.preceding.census * (0.5*log(max.post.censal.var) - log(inherent.census.cv)) / 9)
    WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL = 4
    intercensal.cv = exp(log(inherent.census.cv) + years.from.nearest.census * (0.5*log(max.post.censal.var / WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL) - log(inherent.census.cv)) / 9)  # assume variance is halved
    
    # this is a hack now - need to talk to Zoe about how she will formulate the details field
    # I just know that totals are intercensal at this time
    is.intercensal = grepl('intercensal', details, ignore.case = T) | n.stratified.dimensions==0
    cv = post.censal.cv
    cv[is.intercensal] = intercensal.cv
    
    data * cv
}

population.likelihood.instructions.pop = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2010,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       
                                       observation.correlation.form = 'autoregressive.1', 

                                       error.variance.term = population.error.sd.fn,
                                       error.variance.type = 'function.sd',
                                       
                                       weights = (1*POPULATION.WEIGHT),

                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = F
  )

# state-level: downweighted (1/8) for pop only
population.likelihood.instructions.pop.state = 
    create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                         outcome.for.sim = "population",
                                         dimensions = c("age","sex","race"),
                                         levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                         from.year = 2010,
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         correlation.different.sources = 0.3, # default
                                         correlation.same.source.different.details = 0.3, # default
                                         
                                         observation.correlation.form = 'autoregressive.1', 
                                         
                                         error.variance.term = population.error.sd.fn,
                                         error.variance.type = 'function.sd',
                                         
                                         weights = ((1/8)*POPULATION.WEIGHT),
                                         
                                         # if there are more datapoints for certain years, this will normalize
                                         # e.g., if there are a few years with only the totals 
                                         # before the stratifications are available
                                         equalize.weight.by.year = F
    )

population.likelihood.instructions.trans = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'autoregressive.1', 
                                       error.variance.term = population.error.sd.fn,
                                       error.variance.type = 'function.sd',
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = F
  )

population.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2),
                                       from.year = 2010,
                                       observation.correlation.form = 'autoregressive.1', 
                                       error.variance.term = population.error.sd.fn,
                                       error.variance.type = 'function.sd',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = F
  )

#-- IMMIGRATION  ----
immigration.likelihood.instructions.pop = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", 
                                       outcome.for.sim = "immigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*POPULATION.WEIGHT),
                                       equalize.weight.by.year = T
  )

# state-level: downweighted (1/8) for pop only
immigration.likelihood.instructions.pop.state = 
    create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", 
                                         outcome.for.sim = "immigration",
                                         dimensions = c("age","race"), 
                                         levels.of.stratification = c(0,1),
                                         from.year = 2011, 
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                         error.variance.type = 'cv',
                                         weights = ((1/8)*POPULATION.WEIGHT),
                                         equalize.weight.by.year = T
    )

immigration.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.immigration", 
                                       outcome.for.sim = "immigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

#-- EMIGRATION  ----
emigration.likelihood.instructions.pop = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                       outcome.for.sim = "emigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*POPULATION.WEIGHT),
                                       equalize.weight.by.year = T
  )

# state-level: downweighted (1/8) for pop only
emigration.likelihood.instructions.pop.state = 
    create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                         outcome.for.sim = "emigration",
                                         dimensions = c("age","race"), 
                                         levels.of.stratification = c(0,1),
                                         from.year = 2011, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                         error.variance.type = 'cv',
                                         weights = ((1/8)*POPULATION.WEIGHT),
                                         equalize.weight.by.year = T
    )

emigration.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.emigration", 
                                       outcome.for.sim = "emigration",
                                       dimensions = c("age","race"), 
                                       levels.of.stratification = c(0,1),
                                       from.year = 2011, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                       error.variance.type = 'cv',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )


#-- NEW DIAGNOSES  ----
total.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       #correlation.different.years = 0, # zeroing out 1/17 because we don't care about trend for the population calibration
                                       error.variance.term = DIAGNOSES.ERROR.TERM,
                                       error.variance.type = 'cv',
                                       minimum.error.sd = 1,
                                       weights = (2*POPULATION.WEIGHT),
                                       equalize.weight.by.year = T,
                                       name = 'total.new'
  )

race.risk.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = DIAGNOSES.ERROR.TERM, 
                                       error.variance.type = 'cv',
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

race.risk.new.diagnoses.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.expv.new.diagnoses.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.new.diagnoses.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.new.diagnoses.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.1x.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = DIAGNOSES.ERROR.TERM, 
                                       error.variance.type = 'cv',
                                       minimum.error.sd = 1,
                                       weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

total.new.diagnoses.16x.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = character(),
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (16), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T,
                                       name = 'total.new'
  )

total.new.diagnoses.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.24x.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (24), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.24x.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (24), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.84x.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (84), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.8x.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (8), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.10x.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (10), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.10x.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (10), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

total.new.diagnoses.4x.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = character(),
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (4), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T,
                                       name = 'total.new'
  )

total.new.diagnoses.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = c(1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.new'
    )

new.diagnoses.halfx.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

new.diagnoses.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.halfx.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )


new.diagnoses.halfx.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.2x.one.way.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.1.5x.one.way.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.1.5x.one.way.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

new.diagnoses.7.8x.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (7/8), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )


#-- PREVALENCE  ----
total.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       levels.of.stratification = c(0), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry',
                                       #correlation.different.years = 0, # zeroing out 1/17 because we don't care about trend for the population calibration
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), # second term is from error_for_prevalence_formula.R
                                       error.variance.type = c('cv'), #,'exp.of.variance'), 
                                       minimum.error.sd = 1,
                                       weights = (2*POPULATION.WEIGHT),
                                       equalize.weight.by.year = T,
                                       name = 'total.prevalence'
  )

race.risk.prevalence.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

race.risk.prevalence.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(PREVALENCE.CV.STATE), 
                                         error.variance.type = c('cv'),
                                         minimum.error.sd = 1,
                                         weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.expv.prevalence.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.prevalence.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

race.risk.halfx.cv.prevalence.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

total.prevalence.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = c(1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.16x.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = character(),
                                       levels.of.stratification = 0, 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (16), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T,
                                       name = 'total.prevalence'
  )

total.prevalence.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )


total.prevalence.24x.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (24), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.84x.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (84), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )


total.prevalence.24x.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (24), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.8x.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (8), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.10x.cv.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (10), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.10x.cv.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (10), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T,
                                         name = 'total.prevalence'
    )

total.prevalence.4x.cv.expv.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = character(),
                                       levels.of.stratification = 0, 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (4), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T,
                                       name = 'total.prevalence'
  )

prevalence.2x.one.way.cv.and.exp.v.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = (2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

prevalence.1.5x.one.way.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

prevalence.1.5x.one.way.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

prevalence.halfx.cv.and.exp.v.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

prevalence.halfx.cv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

prevalence.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )


prevalence.halfx.cv.likelihood.instructions.state = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = PREVALENCE.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (1/2), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

prevalence.7.8x.cv.and.exp.v.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       minimum.error.sd = 1,
                                       weights = (7/8), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

prevalence.1x.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                       outcome.for.sim = "diagnosed.prevalence",
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2008, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = list(PREVALENCE.ERROR.TERM), 
                                       error.variance.type = c('cv'),
                                       minimum.error.sd = 1,
                                       weights = (1),
                                       equalize.weight.by.year = T
  )


#-- AIDS DIAGNOSES  ----
non.age.aids.diagnoses.likelihood.instructions.trans =
  create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                       outcome.for.sim = "aids.diagnoses",
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 1985,
                                       to.year = 1993,
                                       correlation.different.years = 0.3,
                                       #observation.correlation.form = 'compound.symmetry',
                                       observation.correlation.form = 'autoregressive.1',
                                       error.variance.term = DIAGNOSES.ERROR.TERM,
                                       error.variance.type = 'cv',
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

# state-level aids diagnoses - through 1994
non.age.aids.diagnoses.likelihood.instructions.trans.state = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1994, 
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         weights = (1*TRANSMISSION.WEIGHT),
                                         equalize.weight.by.year = T
    )

# total aids diagnoses - for adding to state population calibration (testing this out 4/11)
total.aids.diagnoses.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 1985,
                                         to.year = 1994, 
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         weights = (1*TRANSMISSION.WEIGHT),
                                         equalize.weight.by.year = T
    )

total.aids.diagnoses.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 1985,
                                         to.year = 1994, 
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         weights = (1*TRANSMISSION.WEIGHT),
                                         equalize.weight.by.year = T
    )

total.aids.diagnoses.4x.cv.expv.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = character(),
                                         levels.of.stratification = 0, 
                                         from.year = 1985,
                                         to.year = 1994, 
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         weights = c(4),
                                         equalize.weight.by.year = T
    )

# special case for C.31080
non.age.aids.diagnoses.4x.likelihood.instructions.trans =
  create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                       outcome.for.sim = "aids.diagnoses",
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 1985,
                                       to.year = 1993,
                                       correlation.different.years = 0.3,
                                       #observation.correlation.form = 'compound.symmetry',
                                       observation.correlation.form = 'autoregressive.1',
                                       error.variance.term = DIAGNOSES.ERROR.TERM,
                                       error.variance.type = 'cv',
                                       weights = (4*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

non.age.aids.diagnoses.likelihood.instructions.full =
  create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                       outcome.for.sim = "aids.diagnoses",
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 1985,
                                       to.year = 1993,
                                       correlation.different.years = 0.3,
                                       #observation.correlation.form = 'compound.symmetry',
                                       observation.correlation.form = 'autoregressive.1',
                                       error.variance.term = DIAGNOSES.ERROR.TERM,
                                       error.variance.type = 'cv',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

# state-level aids diagnoses - through 1994
non.age.aids.diagnoses.likelihood.instructions.full.state = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1994, # to 1994
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.ERROR.TERM,
                                         error.variance.type = 'cv',
                                         weights = (1*FULL.WEIGHT),
                                         equalize.weight.by.year = T
    )

non.age.aids.diagnoses.cv.expv.likelihood.instructions.full.state = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1994, # to 1994
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         weights = (4),
                                         equalize.weight.by.year = T
    )

non.age.aids.diagnoses.4x.cv.expv.likelihood.instructions.full.state = 
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1994, # to 1994
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         weights = (4),
                                         equalize.weight.by.year = T
    )
# not used? 
non.age.aids.diagnoses.cv.and.exp.v.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                       outcome.for.sim = "aids.diagnoses",
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 1985,
                                       to.year = 1993,
                                       correlation.different.years = 0.3,
                                       #observation.correlation.form = 'compound.symmetry',
                                       observation.correlation.form = 'autoregressive.1',
                                       error.variance.term = list(DIAGNOSES.CV, DIAGNOSES.EXP.OF.VAR), 
                                       error.variance.type = c('cv','exp.of.variance'),
                                       weights = (1),
                                       equalize.weight.by.year = T
  )

non.age.aids.diagnoses.16x.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1995,
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         weights = (16),
                                         equalize.weight.by.year = T
    )

non.age.aids.diagnoses.16x.likelihood.instructions.state =
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = c("sex","race","risk"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 1985,
                                         to.year = 1995,
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         weights = (16),
                                         equalize.weight.by.year = T
    )

total.aids.diagnoses.cv.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = character(),
                                         levels.of.stratification = c(0),
                                         from.year = 1985,
                                         to.year = 1995,
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         weights = (1),
                                         equalize.weight.by.year = T
    )

total.aids.diagnoses.cv.likelihood.instructions.state =
    create.basic.likelihood.instructions(outcome.for.data = "aids.diagnoses",
                                         outcome.for.sim = "aids.diagnoses",
                                         dimensions = character(),
                                         levels.of.stratification = c(0),
                                         from.year = 1985,
                                         to.year = 1995,
                                         correlation.different.years = 0.3,
                                         #observation.correlation.form = 'compound.symmetry',
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = DIAGNOSES.CV.STATE, 
                                         error.variance.type = 'cv',
                                         weights = (1),
                                         equalize.weight.by.year = T
    )


#-- HIV-MORTALITY  ----
# all-cause mortality among pwh
hiv.mortality.likelihood.instructions.trans = 
  create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                       outcome.for.sim = "hiv.mortality", 
                                       dimensions = c("sex"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       to.year = 2019, # added 4/11
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = PREVALENCE.ERROR.TERM, 
                                       error.variance.type = 'cv',
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

hiv.mortality.likelihood.instructions.trans.state = 
    create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                         outcome.for.sim = "hiv.mortality", 
                                         dimensions = c("sex"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         to.year = 2019, # added 4/11
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = PREVALENCE.ERROR.TERM, 
                                         error.variance.type = 'cv',
                                         weights = ((1/16)*TRANSMISSION.WEIGHT),
                                         equalize.weight.by.year = T
    )

hiv.mortality.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                       outcome.for.sim = "hiv.mortality", 
                                       dimensions = c("sex"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       to.year = 2019, # added 4/11
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = PREVALENCE.ERROR.TERM, 
                                       error.variance.type = 'cv',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

MORTALITY.CV = sqrt(PREVALENCE.CV^2 + ( (458 / (1926 + 12219)) / 2 )^2)
MORTALITY.EXP.OF.VAR = PREVALENCE.EXP.OF.VAR
biased.hiv.mortality.likelihood.instructions.full = 
    create.basic.likelihood.instructions(outcome.for.data = "hiv.deaths",
                                         outcome.for.sim = "hiv.mortality", 
                                         dimensions = c("sex"),
                                         levels.of.stratification = c(0,1), 
                                         from.year = 2008, 
                                         to.year = 2019, # added 4/11
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = list(PREVALENCE.CV, PREVALENCE.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         weights = (1),
                                         equalize.weight.by.year = T
    )

#-- GENERAL MORTALITY  ----
# everyone in the population, regardless of HIV 
general.mortality.likelihood.instructions.pop = 
  create.basic.likelihood.instructions(outcome.for.data = "deaths",
                                       outcome.for.sim = "total.mortality", 
                                       dimensions = character(),
                                       levels.of.stratification = c(0), 
                                       from.year = 2007, 
                                       to.year = 2019,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.03, # look into source and see if they have estimate 
                                       error.variance.type = 'cv',
                                       weights = (POPULATION.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

# state-level: downweighted (1/8) for pop only
general.mortality.likelihood.instructions.pop.state = 
    create.basic.likelihood.instructions(outcome.for.data = "deaths",
                                         outcome.for.sim = "total.mortality", 
                                         dimensions = character(),
                                         levels.of.stratification = c(0), 
                                         from.year = 2007, 
                                         to.year = 2019,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = 0.03, # look into source and see if they have estimate 
                                         error.variance.type = 'cv',
                                         weights = ((1/8)*POPULATION.WEIGHT), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

general.mortality.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "deaths",
                                       outcome.for.sim = "total.mortality", 
                                       dimensions = character(),
                                       levels.of.stratification = c(0), 
                                       from.year = 2007, 
                                       to.year = 2019,
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.03, # look into source and see if they have estimate 
                                       error.variance.type = 'cv',
                                       weights = (FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

#-- SUPPRESSION  ----
suppression.basic.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "suppression",
                                       outcome.for.sim = "suppression",
                                       
                                       dimensions = c("age","sex","race","risk"),
                                       
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'sd',
                                       
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

suppression.nested.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('CBSA', 'STATE', 'COUNTY'), 
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
                                                   p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*FULL.WEIGHT),
                                                   equalize.weight.by.year = T
  )

suppression.likelihood.instructions = 
  create.location.based.ifelse.likelihood.instructions(
    suppression.basic.likelihood.instructions,
    suppression.nested.likelihood.instructions,
    locations.list = list(c(locations::get.all.for.type("state"),
                            RIVERSIDE.MSA,
                            MIAMI.MSA,
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA)) # anything not in this list will use second instructions
    
  )

#-- AIDS DEATHS  ----
# in the data, this is the cumulative estimate of aids.diagnoses.deceased.by.2001 from 1980-2001 
# e.g., 1995 aids.diagnoses.deceased.by.2001 gives everyone diagnosed in 1995 who is deceased by 2001 (NOT that they died in 1995)
# so cumulative total will be helpful to get totals by sex/race/risk
aids.deaths.likelihood.instructions.trans = 
  create.basic.likelihood.instructions(outcome.for.data = "aids.deaths", 
                                       outcome.for.sim = "aids.deaths", 
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 1981, 
                                       to.year = 2001,
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.2277531, # using aids diagnoses estimate from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

aids.deaths.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "aids.deaths", 
                                       outcome.for.sim = "aids.deaths", 
                                       dimensions = c("sex","race","risk"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 1981, 
                                       to.year = 2001,
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.2277531, # using aids diagnoses estimate from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

#-- PREP UPTAKE  ----
prep.uptake.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "prep",
                                       outcome.for.sim = "prep.uptake", 
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2007,
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.01239159, # from calculating_error_terms_for_ehe_likelihoods.R
                                       error.variance.type = 'cv',
                                       weights = (0.3*FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

#-- PREP INDICATIONS  ----
# this is an absolute count, not a proportion 
prep.indications.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "prep.indications",
                                       outcome.for.sim = "prep.indications",
                                       dimensions = c("age","sex"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2017, 
                                       to.year = 2018, # they carried forward 2018 numbers 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.5, # high uncertainty,
                                       error.variance.type = 'cv',
                                       # ^ this means you can range from 0 to 2x the number of prep indications
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T 
  )

#-- AWARENESS ----
awareness.basic.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "awareness",
                                       outcome.for.sim = "awareness",
                                       
                                       dimensions = character(), 
                                       levels.of.stratification = 0, 
                                       
                                       from.year = 2008,
                                       
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = NULL, 
                                       error.variance.type = 'data.cv', 
                                      
                                       weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

awareness.nested.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "awareness",
                                                   outcome.for.sim = "awareness",
                                                   denominator.outcome.for.data = "total.prevalence",
                                                   outcome.for.n.multipliers = "diagnosed.prevalence",
                                                   
                                                   location.types = c('STATE','CBSA','COUNTY'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = character(),
                                                   levels.of.stratification = 0,
                                                   
                                                   from.year = 2008,
                                                   
                                                   p.bias.inside.location = awareness.bias.estimates$in.mean,
                                                   p.bias.outside.location = awareness.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = awareness.bias.estimates$in.sd, 
                                                   p.bias.sd.outside.location = awareness.bias.estimates$out.sd, 
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = NULL, 
                                                   p.error.variance.type = 'data.cv', 
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                                   equalize.weight.by.year = T
  )


awareness.likelihood.instructions = 
  create.location.based.ifelse.likelihood.instructions(
    awareness.basic.likelihood.instructions,
    awareness.nested.likelihood.instructions,
    locations.list = list(c(locations::get.all.for.type("state"),
                            RIVERSIDE.MSA,
                            MIAMI.MSA,
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA))  # anything not in this list will use second instructions
  )

#-- HEROIN  ----
heroin.basic.likelihood.instructions.trans = 
  create.basic.likelihood.instructions(outcome.for.data = "heroin",
                                       outcome.for.sim = "proportion.using.heroin",
                                       
                                       dimensions = c("age"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.54, # NSDUH calcs; doubled value (0.27); see NSDUH IDU Data_updated.xlsx in input_managers
                                       error.variance.type = "cv",
                                       
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

heroin.nested.likelihood.instructions.trans = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "heroin",
                                                   outcome.for.sim = "proportion.using.heroin",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = heroin.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = heroin.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = heroin.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = 0.54, # NSDUH calcs; doubled value (0.27); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TRANSMISSION.WEIGHT),
                                                   equalize.weight.by.year = T
  )


heroin.likelihood.instructions.trans = 
  create.location.based.ifelse.likelihood.instructions(
    heroin.basic.likelihood.instructions.trans,
    heroin.nested.likelihood.instructions.trans,
    locations.list = list(c(locations::get.all.for.type("state"),
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA))  # anything not in this list will use second instructions
  )

heroin.basic.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "heroin",
                                       outcome.for.sim = "proportion.using.heroin",
                                       
                                       dimensions = c("age"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = 0.54, # NSDUH calcs; doubled value (0.27); see NSDUH IDU Data_updated.xlsx in input_managers
                                       error.variance.type = "cv",
                                       
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

heroin.nested.likelihood.instructions.full = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "heroin",
                                                   outcome.for.sim = "proportion.using.heroin",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = heroin.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = heroin.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = heroin.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = 0.54, # NSDUH calcs; doubled value (0.27); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*FULL.WEIGHT),
                                                   equalize.weight.by.year = T
  )


heroin.likelihood.instructions.full = 
  create.location.based.ifelse.likelihood.instructions(
    heroin.basic.likelihood.instructions.full,
    heroin.nested.likelihood.instructions.full,
    locations.list = list(c(locations::get.all.for.type("state"),
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA))  # anything not in this list will use second instructions
  )

#-- COCAINE  ----
cocaine.basic.likelihood.instructions.trans = 
  create.basic.likelihood.instructions(outcome.for.data = "cocaine",
                                       outcome.for.sim = "proportion.using.cocaine",
                                       
                                       dimensions = c("age"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.42, # NSDUH calcs doubled value (0.21); see NSDUH IDU Data_updated.xlsx in input_managers
                                       error.variance.type = "cv",
                                       
                                       weights = (1*TRANSMISSION.WEIGHT),
                                       equalize.weight.by.year = T
  )

cocaine.nested.likelihood.instructions.trans = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "cocaine",
                                                   outcome.for.sim = "proportion.using.cocaine",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = cocaine.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = cocaine.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = cocaine.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.42, # NSDUH calcs doubled value (0.21); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*TRANSMISSION.WEIGHT),
                                                   equalize.weight.by.year = T
  )


cocaine.likelihood.instructions.trans = 
  create.location.based.ifelse.likelihood.instructions(
    cocaine.basic.likelihood.instructions.trans,
    cocaine.nested.likelihood.instructions.trans,
    locations.list = list(c(locations::get.all.for.type("state"),
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA)) # anything not in this list will use second instructions
  )

cocaine.basic.likelihood.instructions.full = 
  create.basic.likelihood.instructions(outcome.for.data = "cocaine",
                                       outcome.for.sim = "proportion.using.cocaine",
                                       
                                       dimensions = c("age"),
                                       levels.of.stratification = c(0,1), 
                                       from.year = 2008, 
                                       
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.42, # NSDUH calcs doubled value (0.21); see NSDUH IDU Data_updated.xlsx in input_managers
                                       error.variance.type = "cv",
                                       
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

cocaine.nested.likelihood.instructions.full = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "cocaine",
                                                   outcome.for.sim = "proportion.using.cocaine",
                                                   denominator.outcome.for.data = 'adult.population',
                                                   
                                                   location.types = c('STATE','NSDUH'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = cocaine.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = cocaine.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = cocaine.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.42, # NSDUH calcs doubled value (0.21); see NSDUH IDU Data_updated.xlsx in input_managers
                                                   p.error.variance.type = "cv",
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*FULL.WEIGHT),
                                                   equalize.weight.by.year = T
  )


cocaine.likelihood.instructions.full = 
  create.location.based.ifelse.likelihood.instructions(
    cocaine.basic.likelihood.instructions.full,
    cocaine.nested.likelihood.instructions.full,
    locations.list = list(c(locations::get.all.for.type("state"),
                            LA.MSA,
                            VEGAS.MSA,
                            SAN.DIEGO.MSA))  # anything not in this list will use second instructions
  )

#-- PROPORTION TESTED ----
proportion.tested.basic.likelihood.instructions =
  create.basic.likelihood.instructions(outcome.for.data = "proportion.tested",
                                       outcome.for.sim = "testing",
                                       
                                       dimensions = c("age","sex","race","risk"),
                                       levels.of.stratification = c(0,1),
                                       from.year = 2010,
                                       
                                       observation.correlation.form = 'compound.symmetry',
                                       error.variance.term = NULL,
                                       error.variance.type = 'data.variance', # sd or data.variance 
                                       
                                       weights = (1*FULL.WEIGHT),
                                       equalize.weight.by.year = T
  )

proportion.tested.nested.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested",
                                                   outcome.for.sim = "testing",
                                                   denominator.outcome.for.data = "adult.population",
                                                   
                                                   location.types = c('STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   levels.of.stratification = c(0,1),
                                                   from.year = 2010,
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = NULL,
                                                   p.error.variance.type = 'data.variance', # sd or data.variance 
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (1*FULL.WEIGHT),
                                                   equalize.weight.by.year = T
  )

proportion.tested.likelihood.instructions = 
  create.location.based.ifelse.likelihood.instructions(
    proportion.tested.basic.likelihood.instructions,
    proportion.tested.nested.likelihood.instructions,
    locations.list = list(c(locations::get.all.for.type("state")))  # anything not in this list will use second instructions
  )

#-- HIV TEST POSITIVITY ----
hiv.test.positivity.basic.likelihood.instructions = 
  create.basic.likelihood.instructions.with.included.multiplier(outcome.for.data = "cdc.hiv.test.positivity",
                                                                outcome.for.sim = "cdc.hiv.test.positivity",
                                                                
                                                                dimensions = character(),
                                                                levels.of.stratification = c(0),
                                                                from.year = 2014,
                                                                to.year = 2020, 
                                                                
                                                                observation.correlation.form = 'compound.symmetry',
                                                                
                                                                included.multiplier = 2.810587, # from cdc_positivity.bias.R
                                                                included.multiplier.sd = sqrt(0.1391234), # from cdc_positivity.bias.R
                                                                included.multiplier.correlation = 0.5,
                                                                
                                                                error.variance.term = 0.5, # can be off by two fold; guessing this 
                                                                error.variance.type = 'cv',
                                                                
                                                                weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                                                equalize.weight.by.year = T
  )

hiv.test.positivity.nested.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions.with.included.multiplier(outcome.for.data = "cdc.hiv.test.positivity",
                                                   outcome.for.sim = "cdc.hiv.test.positivity",
                                                   denominator.outcome.for.data = "hiv.tests",
                                                   outcome.for.n.multipliers = 'adult.population',
                                                   #"total.hiv.tests",
                                                   
                                                   location.types = c('STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = character(),
                                                   levels.of.stratification = c(0),
                                                   from.year = 2014,
                                                   to.year = 2020, # REMOVE WHEN ADULT POPULATION IS READY
                                                   location.stratum.keep.threshold = 0,
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = hiv.test.positivity.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                   p.bias.sd.outside.location = hiv.test.positivity.bias.estimates$out.sd,
                                                   maximum.locations.per.type = 4,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry',
                                                   
                                                   included.multiplier = 2.810587, # from cdc_positivity.bias.R
                                                   included.multiplier.sd = sqrt(0.1391234), # from cdc_positivity.bias.R
                                                   included.multiplier.correlation = 0.5,
                                                   
                                                   p.error.variance.term = 0.5, # can be off by two fold; guessing this 
                                                   p.error.variance.type = 'cv',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R 
                                                   equalize.weight.by.year = T
  )

hiv.test.positivity.likelihood.instructions = 
  create.ifelse.likelihood.instructions(
    hiv.test.positivity.nested.likelihood.instructions,
    hiv.test.positivity.basic.likelihood.instructions
  )

#-- YEAR-ON-YEAR TESTS CHANGE ----

# nested version 
number.of.tests.year.on.year.change.nested.likelihood.instructions = 
  create.time.lagged.comparison.nested.proportion.likelihood.instructions(
    outcome.for.data = "hiv.tests.per.population",
    outcome.for.sim = "total.hiv.tests.per.population",
    
    denominator.outcome.for.data = "adult.population",
    
    location.types = c('STATE','CBSA'),
    minimum.geographic.resolution.type = 'COUNTY',
    
    levels.of.stratification = c(0),
    from.year = 2008,
    to.year = 2020, # REMOVE WHEN ADULT POPULATION IS READY
    
    p.bias.inside.location = 0, 
    p.bias.outside.location = hiv.tests.per.population.bias.estimates$out.mean,
    p.bias.sd.inside.location = hiv.tests.per.population.bias.estimates$out.sd,
    p.bias.sd.outside.location = hiv.tests.per.population.bias.estimates$out.sd,
    
    within.location.p.error.correlation = 0.5,
    within.location.n.error.correlation = 0.5,
    
    observation.correlation.form = 'compound.symmetry',
    p.error.variance.term = 0.03, # guessed this
    p.error.variance.type = 'cv',
    ratio.cv = 1.2,
    
    partitioning.function = EHE.PARTITIONING.FUNCTION, 
    
    weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R 
    equalize.weight.by.year = T,
    use.lognormal.approximation = T
  )

# basic version 
number.of.tests.year.on.year.change.basic.likelihood.instructions =
  create.time.lagged.comparison.likelihood.instructions(
    outcome.for.data = "hiv.tests.per.population",
    outcome.for.sim = "total.hiv.tests.per.population",
    
    levels.of.stratification = c(0),
    from.year = 2008,
    
    observation.correlation.form = 'compound.symmetry',
    error.variance.term = 0.03, # guessed this
    error.variance.type = 'cv',
    
    ratio.cv = 1.2,
    
    weights = (18*FULL.WEIGHT), # see prev_new_aware_weighting.R
    equalize.weight.by.year = T,
    use.lognormal.approximation = T
  )

# ifelse to try both versions 
number.of.tests.year.on.year.change.likelihood.instructions = 
  create.ifelse.likelihood.instructions(
    number.of.tests.year.on.year.change.basic.likelihood.instructions,
    number.of.tests.year.on.year.change.nested.likelihood.instructions
  )


#-- YEAR-ON-YEAR GONORRHEA CHANGE ----
# gonorrhea.year.on.year.change.likelihood.instructions = 
#   create.basic.ratio.likelihood.instructions(outcome.for.data = "gonorrhea.ratio",  # can add name = gonorrhea here
#                                              outcome.for.sim = "sexual.transmission.rates", 
#                                              # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
#                                              # (2020 sexual transmission/2019 sexual transmission)
#                                              levels.of.stratification = c(0,1), 
#                                              dimensions = c("sex","race","age"),
#                                              from.year = 2018, 
#                                              to.year = 2022,
#                                              observation.correlation.form = 'compound.symmetry', 
#                                              error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
#                                              error.variance.type = 'cv',
#                                              correlation.different.years = 0.5,
#                                              
#                                              ratio.cv = 1.5,
#                                              # ratio.correlation = , # NULL will enter default of 0
#                                              
#                                              weights = (1*FULL.WEIGHT),
#                                              equalize.weight.by.year = T 
#   )
gonorrhea.year.on.year.change.likelihood.instructions = 
  create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "gonorrhea.ratio",  # can add name = gonorrhea here
                                             outcome.for.sim = "sexual.transmission.rates", 
                                             # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
                                             # (2020 sexual transmission/2019 sexual transmission)
                                             levels.of.stratification = c(0,1), 
                                             dimensions = c("sex","race","age"),
                                             from.year = 2018, 
                                             to.year = 2022,
                                             observation.correlation.form = 'compound.symmetry', 
                                             error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
                                             error.variance.type = 'cv',
                                             correlation.different.years = 0.5,
                                             
                                             use.lognormal.approximation = T,
                                             ratio.cv = 1.35,
                                             # ratio.correlation = , # NULL will enter default of 0
                                             
                                             weights = (1*FULL.WEIGHT),
                                             equalize.weight.by.year = T 
  )
#-- YEAR-ON-YEAR SYPHILIS CHANGE ----
# ps.syphilis.year.on.year.change.likelihood.instructions = 
#   create.basic.ratio.likelihood.instructions(outcome.for.data = "ps.syphilis.ratio", # can add name = syphilis here
#                                              outcome.for.sim = "sexual.transmission.rates", 
#                                              # (2020 ps diagnoses / 2019 ps diagnoses) proportional to 
#                                              # (2020 sexual transmisson/2019 sexual transmission)
#                                              levels.of.stratification = c(0,1), 
#                                              dimensions = c("sex","race","age"),
#                                              from.year = 2018, 
#                                              to.year = 2022,
#                                              observation.correlation.form = 'compound.symmetry', 
#                                              error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
#                                              error.variance.type = 'cv',
#                                              correlation.different.years = 0.5,
#                                              
#                                              ratio.cv = 1.5,
#                                              # ratio.correlation = , # NULL will enter default of 0
#                                              
#                                              weights = (1*FULL.WEIGHT),
#                                              equalize.weight.by.year = T 
#   )
ps.syphilis.year.on.year.change.likelihood.instructions = 
  create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "ps.syphilis.ratio", # can add name = syphilis here
                                                        outcome.for.sim = "sexual.transmission.rates", 
                                                        # (2020 ps diagnoses / 2019 ps diagnoses) proportional to 
                                                        # (2020 sexual transmisson/2019 sexual transmission)
                                                        levels.of.stratification = c(0,1), 
                                                        dimensions = c("sex","race","age"),
                                                        from.year = 2018, 
                                                        to.year = 2022,
                                                        observation.correlation.form = 'compound.symmetry', 
                                                        error.variance.term = 0.03, # if we can't find something better, use diagnoses estimate from above (0.07425679)
                                                        error.variance.type = 'cv',
                                                        correlation.different.years = 0.5,
                                                        
                                                        use.lognormal.approximation = T,
                                                        ratio.cv = 1.35,
                                                        # ratio.correlation = , # NULL will enter default of 0
                                                        
                                                        weights = (1*FULL.WEIGHT),
                                                        equalize.weight.by.year = T 
  )


#-- FUTURE CHANGE PENALTY LIKELIHOOD (to prevent new diagnoses from taking off in the future) ---- 
future.change.penalty.fn = function(sim,log=T){
  
  # 2009, 2019, 2029 diagnoses
  diagnoses = sim$get(outcomes = 'new',keep.dimensions = "year",dimension.values = list(year=c(2009,2019,2029)))
  
  pre.change = (diagnoses[2]-diagnoses[1])/diagnoses[1]
  post.change = (diagnoses[3]-diagnoses[2])/diagnoses[2]
  
  spread = 0.2 # parameter to tune how wide distribution is 
  
  lik = 0.5*dnorm(post.change,mean = (pre.change - spread), sd = spread) + 
    0.5*dnorm(post.change,mean = (pre.change + spread), sd = spread)
  
  if(log){
    rv = log(lik)
  } else{
    rv = lik
  }
  rv 
  
}

future.change.penalty.likelihood.instructions = 
  create.custom.likelihood.instructions(name = "future.change.penalty", # default will be outcome for sim 
                                        #outcome.for.sim = NULL, # placeholder, want this to be NULL
                                        compute.function = future.change.penalty.fn)


#-- INCIDENCE FUTURE CHANGE - going to penalize sharp rises in incidence among any stratum --#

# NEW.FOLD.CHANGE.14.19 = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk['2019',,,,,] / SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk['2014',,,,,]
# P.NEW.FOLD.CHANGE.GT.3 = mean(NEW.FOLD.CHANGE.14.19[!is.na(NEW.FOLD.CHANGE.14.19) & !is.infinite(NEW.FOLD.CHANGE.14.19)] > 3)
P.NEW.FOLD.CHANGE.GT.3 = 0.02778896

future.incidence.change.likelihood.instructions = 
    create.custom.likelihood.instructions(
        name = 'future.incidence.change',
        compute.function = function(sim, data, log=T){
            
            inc = sim$optimized.get(data$optimized.get.instr)
            
            fold.change.inc = inc[as.character(2025:2030),,,,,] / inc[as.character(2020:2025),,,,,]
            # fold.change.inc = sim$get('incidence', year=2025:2030, keep.dimensions=c('year','age','race','sex','risk')) / sim$get('incidence', year=2020:2025, keep.dimensions=c('year','age','race','sex','risk'))
            fold.change.inc[is.na(fold.change.inc)] = 100
            max.fold.change.inc = apply(fold.change.inc, 2:5, max)
            
            n.fold.change.gt.3 = sum(max.fold.change.inc>3)
            n.fold.change.lte.3 = length(max.fold.change.inc) - n.fold.change.gt.3
            
            rv = sum(log(P.NEW.FOLD.CHANGE.GT.3)*n.fold.change.gt.3 + log(1-P.NEW.FOLD.CHANGE.GT.3)*n.fold.change.lte.3)
            
            if (!log)
                exp(rv)
            else
                rv
        },
        get.data.function = function(version, location)
        {
            sim.metadata = get.simulation.metadata(version=version, location=location)
            optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
                outcomes = 'incidence', 
                dimension.values = list(year=2020:2030),
                keep.dimensions = c('year','age','race','sex','risk')
            )
            
            list(
                optimized.get.instr = optimized.get.instr
            )
        }
        )


#-- IDU Active/Prior Ratio --#

idu.active.prior.ratio.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'idu.active.prior.ratio',
    
    compute.function = function(sim, data, log=T)
    {
        pop = sim$optimized.get(data$optimized.get.instr)
        
        active.prior.ratios.by.age = colSums(pop[,,'active_IDU']) /colSums(pop[,,'IDU_in_remission'])
        
        # active.prior.ratios.by.age = sim$get('population',
        #                                      dimension.values=list(year = data$active.to.remission.ratios$years,
        #                                                            risk = 'active_IDU'),
        #                                      keep.dimensions = 'age',
        #                                      drop.single.sim.dimension = T) /
        #     sim$get('population',
        #             dimension.values=list(year = data$active.to.remission.ratios$years,
        #                                   risk = 'IDU_in_remission'),
        #             keep.dimensions = 'age',
        #             drop.single.sim.dimension = T)
       
       d = dlnorm(x = active.prior.ratios.by.age,
                  meanlog = base::log(as.numeric(data$active.to.remission.ratios$age)),
                  sdlog = log(1.25) / 2,
                  log = log)
       
       if (log)
           sum(d)
       else
           prod(d)
    },
    
    get.data.function = function(version, location)
    {
        active.to.remission.ratios = get.cached.object.for.version('active.to.remission.ratios', version=version)
        
        sim.metadata = get.simulation.metadata(version=version, location=location)
        optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
            'population', 
            dimension.values=list(year = active.to.remission.ratios$years,
                                  risk = c('active_IDU','IDU_in_remission')),
            keep.dimensions = c('year','age','risk'),
            drop.single.sim.dimension = T
        )
        
        list(
            active.to.remission.ratios = active.to.remission.ratios,
            optimized.get.instr = optimized.get.instr
        )
    }
)

#-- JOIN THE POPULATION-RELATED LIKELIHOODS --#  ----
joint.pop.migration.total.trans.likelihood.instructions = 
  join.likelihood.instructions(population.likelihood.instructions.pop,
                               immigration.likelihood.instructions.pop, 
                               emigration.likelihood.instructions.pop,
                               general.mortality.likelihood.instructions.pop,
                               total.prevalence.likelihood.instructions, 
                               total.new.diagnoses.likelihood.instructions #,
                               #weight = POPULATION.WEIGHT
                               ) 

# state-level pop likelihood
pop.state.likelihood.instructions = 
    join.likelihood.instructions(population.likelihood.instructions.pop.state,
                                 immigration.likelihood.instructions.pop.state, 
                                 emigration.likelihood.instructions.pop.state,
                                 general.mortality.likelihood.instructions.pop.state,
                                 total.prevalence.cv.likelihood.instructions, 
                                 total.new.diagnoses.cv.likelihood.instructions,
                                 total.aids.diagnoses.cv.likelihood.instructions
                                 #weight = POPULATION.WEIGHT
    ) 

pop.state.likelihood.instructions.2 = 
    join.likelihood.instructions(population.likelihood.instructions.pop.state,
                                 immigration.likelihood.instructions.pop.state, 
                                 emigration.likelihood.instructions.pop.state,
                                 general.mortality.likelihood.instructions.pop.state,
                                 total.prevalence.cv.likelihood.instructions.state, 
                                 total.new.diagnoses.cv.likelihood.instructions.state,
                                 total.aids.diagnoses.cv.likelihood.instructions.state
                                 #weight = POPULATION.WEIGHT
    ) 

#-- JOIN THE TRANSMISSION-RELATED AND POPULATION LIKELIHOODS --#  ----
transmission.pop.idu.aware.aids.testing.likelihood.instructions = 
  join.likelihood.instructions(race.risk.halfx.cv.expv.new.diagnoses.likelihood.instructions, 
                               race.risk.halfx.cv.expv.prevalence.likelihood.instructions, 
                               total.new.diagnoses.8x.cv.expv.likelihood.instructions,
                               total.prevalence.8x.cv.expv.likelihood.instructions,
                               non.age.aids.diagnoses.likelihood.instructions.trans,
                               population.likelihood.instructions.trans,
                               heroin.likelihood.instructions.trans,
                               cocaine.likelihood.instructions.trans,
                               aids.deaths.likelihood.instructions.trans,
                               hiv.mortality.likelihood.instructions.trans,
                               idu.active.prior.ratio.likelihood.instructions#,
                               #weight = TRANSMISSION.WEIGHT

  )

# special case for C.31080
transmission.pop.idu.aware.aids.testing.likelihood.instructions.4x.aids = 
  join.likelihood.instructions(race.risk.new.diagnoses.likelihood.instructions, 
                               race.risk.prevalence.likelihood.instructions, 
                               non.age.aids.diagnoses.4x.likelihood.instructions.trans,
                               population.likelihood.instructions.trans,
                               heroin.likelihood.instructions.trans,
                               cocaine.likelihood.instructions.trans,
                               aids.deaths.likelihood.instructions.trans,
                               hiv.mortality.likelihood.instructions.trans,
                               idu.active.prior.ratio.likelihood.instructions#,
                               #weight = TRANSMISSION.WEIGHT
                               
  )

# state-level transmission calibration 
trans.state.likelihood.instructions = 
    join.likelihood.instructions(race.risk.halfx.cv.new.diagnoses.likelihood.instructions,
                                 race.risk.halfx.cv.prevalence.likelihood.instructions,
                                 total.new.diagnoses.10x.cv.likelihood.instructions,
                                 total.prevalence.10x.cv.instructions,
                                 non.age.aids.diagnoses.16x.likelihood.instructions,
                                 population.likelihood.instructions.trans,
                                 heroin.likelihood.instructions.trans,
                                 cocaine.likelihood.instructions.trans,
                                 biased.hiv.mortality.likelihood.instructions.full,
                                 future.incidence.change.likelihood.instructions,
                                 idu.active.prior.ratio.likelihood.instructions
                                 #state.aids.diagnoses.proportions.instructions
                                 #weight = TRANSMISSION.WEIGHT
                                 
    )

trans.state.likelihood.instructions.2 = 
    join.likelihood.instructions(race.risk.halfx.cv.new.diagnoses.likelihood.instructions.state,
                                 race.risk.halfx.cv.prevalence.likelihood.instructions.state,
                                 total.new.diagnoses.10x.cv.likelihood.instructions.state,
                                 total.prevalence.10x.cv.instructions.state,
                                 non.age.aids.diagnoses.16x.likelihood.instructions.state,
                                 population.likelihood.instructions.trans,
                                 heroin.likelihood.instructions.trans,
                                 cocaine.likelihood.instructions.trans,
                                 biased.hiv.mortality.likelihood.instructions.full,
                                 future.incidence.change.likelihood.instructions,
                                 idu.active.prior.ratio.likelihood.instructions
                                 #state.aids.diagnoses.proportions.instructions
                                 #weight = TRANSMISSION.WEIGHT
                                 
    )

trans.state.likelihood.instructions.B = 
    join.likelihood.instructions(race.risk.new.diagnoses.likelihood.instructions.state,
                                 race.risk.prevalence.likelihood.instructions.state,
                                 non.age.aids.diagnoses.16x.likelihood.instructions,
                                 population.likelihood.instructions.trans,
                                 heroin.likelihood.instructions.trans,
                                 cocaine.likelihood.instructions.trans,
                                 biased.hiv.mortality.likelihood.instructions.full,
                                 future.incidence.change.likelihood.instructions,
                                 idu.active.prior.ratio.likelihood.instructions
                                 #state.aids.diagnoses.proportions.instructions
                                 #weight = TRANSMISSION.WEIGHT
                                 
    )


#-- FULL LIKELIHOOD WITH THREE COVID LIKELIHOODS --# ---- 
FULL.likelihood.instructions.32x.new.prev = join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions.full, 
  immigration.likelihood.instructions.full, 
  emigration.likelihood.instructions.full,
  
  # TRANSMISSION LIKELIHOODS
  total.new.diagnoses.16x.cv.expv.likelihood.instructions,
  new.diagnoses.halfx.cv.expv.likelihood.instructions,
  total.prevalence.16x.cv.expv.likelihood.instructions,
  prevalence.halfx.cv.and.exp.v.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  hiv.mortality.likelihood.instructions.full,
  general.mortality.likelihood.instructions.full,
  aids.deaths.likelihood.instructions.full,
  
  # AIDS DIAGNOSES LIKELIHOOD
  non.age.aids.diagnoses.likelihood.instructions.full,
  
  # CONTINUUM LIKELIHOODS
  proportion.tested.likelihood.instructions,
  hiv.test.positivity.likelihood.instructions, 
  awareness.likelihood.instructions,
  suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  prep.uptake.likelihood.instructions,
  prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions.full,
  cocaine.likelihood.instructions.full,
  idu.active.prior.ratio.likelihood.instructions,
  
  # COVID LIKELIHOODS
  number.of.tests.year.on.year.change.likelihood.instructions,
  gonorrhea.year.on.year.change.likelihood.instructions,
  ps.syphilis.year.on.year.change.likelihood.instructions,
  
  # FUTURE INCIDENCE PENALTY
  future.incidence.change.likelihood.instructions
)

# state-level full calibration 
full.state.likelihood.instructions = join.likelihood.instructions(
    # POPULATION LIKELIHOODS
    population.likelihood.instructions.full, 
    immigration.likelihood.instructions.full, 
    emigration.likelihood.instructions.full,
    
    # TRANSMISSION LIKELIHOODS
    total.new.diagnoses.16x.cv.expv.likelihood.instructions,
    new.diagnoses.2x.one.way.cv.expv.likelihood.instructions,
    new.diagnoses.halfx.cv.expv.likelihood.instructions,
    
    total.prevalence.16x.cv.expv.likelihood.instructions,
    prevalence.2x.one.way.cv.and.exp.v.likelihood.instructions,
    prevalence.halfx.cv.and.exp.v.likelihood.instructions,
    
    # MORTALITY LIKELIHOODS
    biased.hiv.mortality.likelihood.instructions.full,
    general.mortality.likelihood.instructions.full,
    
    # AIDS DIAGNOSES LIKELIHOOD
    non.age.aids.diagnoses.16x.likelihood.instructions,
    
    # CONTINUUM LIKELIHOODS
    proportion.tested.likelihood.instructions,
    hiv.test.positivity.likelihood.instructions, 
    awareness.likelihood.instructions,
    suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    prep.uptake.likelihood.instructions,
    prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    heroin.likelihood.instructions.full,
    cocaine.likelihood.instructions.full,
    idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    number.of.tests.year.on.year.change.likelihood.instructions,
    gonorrhea.year.on.year.change.likelihood.instructions,
    ps.syphilis.year.on.year.change.likelihood.instructions,
    
    # FUTURE INCIDENCE PENALTY
    future.incidence.change.likelihood.instructions
)

full.state.plus.aids.prop.likelihood.instructions = join.likelihood.instructions(
    full.state.likelihood.instructions,
    state.aids.diagnoses.proportions.instructions
)

full.state.weighted.likelihood.instructions = join.likelihood.instructions(
    # POPULATION LIKELIHOODS
    join.likelihood.instructions(
        population.likelihood.instructions.full, 
        additional.weights = 1/16),
    
    join.likelihood.instructions(
        immigration.likelihood.instructions.full, 
        emigration.likelihood.instructions.full,
        additional.weights = 1/4),
    
    # TRANSMISSION LIKELIHOODS
    total.new.diagnoses.24x.cv.likelihood.instructions,
    new.diagnoses.1.5x.one.way.cv.likelihood.instructions,
    new.diagnoses.halfx.cv.likelihood.instructions,

    total.prevalence.24x.cv.likelihood.instructions,
    prevalence.1.5x.one.way.cv.likelihood.instructions,
    prevalence.halfx.cv.likelihood.instructions,
    
    # MORTALITY LIKELIHOODS
    biased.hiv.mortality.likelihood.instructions.full,
    general.mortality.likelihood.instructions.full,
    
    # AIDS DIAGNOSES LIKELIHOOD
    non.age.aids.diagnoses.16x.likelihood.instructions,
    
    # CONTINUUM LIKELIHOODS
    proportion.tested.likelihood.instructions,
    hiv.test.positivity.likelihood.instructions, 
    awareness.likelihood.instructions,
    suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    prep.uptake.likelihood.instructions,
    prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    heroin.likelihood.instructions.full,
    cocaine.likelihood.instructions.full,
    idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    number.of.tests.year.on.year.change.likelihood.instructions,
    gonorrhea.year.on.year.change.likelihood.instructions,
    ps.syphilis.year.on.year.change.likelihood.instructions,
    
    # FUTURE INCIDENCE PENALTY
    future.incidence.change.likelihood.instructions
)

full.state.weighted.likelihood.instructions.B = join.likelihood.instructions(
    # POPULATION LIKELIHOODS
    join.likelihood.instructions(
        population.likelihood.instructions.full, 
        additional.weights = 1/16),
    
    join.likelihood.instructions(
        immigration.likelihood.instructions.full, 
        emigration.likelihood.instructions.full,
        additional.weights = 1/4),
    
    # TRANSMISSION LIKELIHOODS
    new.diagnoses.cv.likelihood.instructions.state,
    prevalence.cv.likelihood.instructions.state,
    
    # MORTALITY LIKELIHOODS
    biased.hiv.mortality.likelihood.instructions.full,
    general.mortality.likelihood.instructions.full,
    
    # AIDS DIAGNOSES LIKELIHOOD
    non.age.aids.diagnoses.16x.likelihood.instructions,
    
    # CONTINUUM LIKELIHOODS
    proportion.tested.likelihood.instructions,
    hiv.test.positivity.likelihood.instructions, 
    awareness.likelihood.instructions,
    suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    prep.uptake.likelihood.instructions,
    prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    heroin.likelihood.instructions.full,
    cocaine.likelihood.instructions.full,
    idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    number.of.tests.year.on.year.change.likelihood.instructions,
    gonorrhea.year.on.year.change.likelihood.instructions,
    ps.syphilis.year.on.year.change.likelihood.instructions,
    
    # FUTURE INCIDENCE PENALTY
    future.incidence.change.likelihood.instructions
)

full.state.weighted.likelihood.instructions.2 = join.likelihood.instructions(
    # POPULATION LIKELIHOODS
    join.likelihood.instructions(
        population.likelihood.instructions.full, 
        additional.weights = 1/16),
    
    join.likelihood.instructions(
        immigration.likelihood.instructions.full, 
        emigration.likelihood.instructions.full,
        additional.weights = 1/4),
    
    # TRANSMISSION LIKELIHOODS
    total.new.diagnoses.84x.cv.likelihood.instructions.state,
    new.diagnoses.halfx.cv.likelihood.instructions.state,
    
    total.prevalence.84x.cv.likelihood.instructions.state,
    prevalence.halfx.cv.likelihood.instructions.state,
    
    # MORTALITY LIKELIHOODS
    biased.hiv.mortality.likelihood.instructions.full,
    general.mortality.likelihood.instructions.full,
    
    # AIDS DIAGNOSES LIKELIHOOD
    non.age.aids.diagnoses.16x.likelihood.instructions.state,
    # state.aids.diagnoses.proportions.instructions,
    
    # CONTINUUM LIKELIHOODS
    proportion.tested.likelihood.instructions,
    hiv.test.positivity.likelihood.instructions, 
    awareness.likelihood.instructions,
    suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    prep.uptake.likelihood.instructions,
    prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    heroin.likelihood.instructions.full,
    cocaine.likelihood.instructions.full,
    idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    number.of.tests.year.on.year.change.likelihood.instructions,
    gonorrhea.year.on.year.change.likelihood.instructions,
    ps.syphilis.year.on.year.change.likelihood.instructions,
    
    # FUTURE INCIDENCE PENALTY
    future.incidence.change.likelihood.instructions
)

full.state.likelihood.instructions.2.half.weight = join.likelihood.instructions(
    full.state.weighted.likelihood.instructions.2, 
    additional.weights = 1/2)

full.state.weighted.likelihood.instructions.2.fl.half = join.likelihood.instructions(
    full.state.weighted.likelihood.instructions.2, 
    additional.weights = 1/2)

FULL.likelihood.instructions.8x.new.prev = join.likelihood.instructions(
  # POPULATION LIKELIHOODS
  population.likelihood.instructions.full, 
  immigration.likelihood.instructions.full, 
  emigration.likelihood.instructions.full,
  
  # TRANSMISSION LIKELIHOODS
  total.new.diagnoses.4x.cv.expv.likelihood.instructions,
  new.diagnoses.7.8x.cv.expv.likelihood.instructions,
  total.prevalence.4x.cv.expv.likelihood.instructions,
  prevalence.7.8x.cv.and.exp.v.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  hiv.mortality.likelihood.instructions.full,
  general.mortality.likelihood.instructions.full,
  aids.deaths.likelihood.instructions.full,
  
  # AIDS DIAGNOSES LIKELIHOOD
  non.age.aids.diagnoses.likelihood.instructions.full,
  
  # CONTINUUM LIKELIHOODS
  proportion.tested.likelihood.instructions,
  hiv.test.positivity.likelihood.instructions, 
  awareness.likelihood.instructions,
  suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  prep.uptake.likelihood.instructions,
  prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions.full,
  cocaine.likelihood.instructions.full,
  idu.active.prior.ratio.likelihood.instructions,
  
  # COVID LIKELIHOODS
  number.of.tests.year.on.year.change.likelihood.instructions,
  gonorrhea.year.on.year.change.likelihood.instructions,
  ps.syphilis.year.on.year.change.likelihood.instructions
)

# state-level final calibration - UPDATE WITH WHATEVER WE USE FOR FULL
FULL.likelihood.instructions.8x.new.prev.state = join.likelihood.instructions(
    # POPULATION LIKELIHOODS
    population.likelihood.instructions.full, 
    immigration.likelihood.instructions.full, 
    emigration.likelihood.instructions.full,
    
    # TRANSMISSION LIKELIHOODS
    total.new.diagnoses.4x.cv.expv.likelihood.instructions,
    new.diagnoses.7.8x.cv.expv.likelihood.instructions,
    total.prevalence.4x.cv.expv.likelihood.instructions,
    prevalence.7.8x.cv.and.exp.v.likelihood.instructions,
    
    # MORTALITY LIKELIHOODS
    hiv.mortality.likelihood.instructions.full,
    general.mortality.likelihood.instructions.full,
    
    # AIDS DIAGNOSES LIKELIHOOD
    non.age.aids.diagnoses.likelihood.instructions.full.state, # state-level: through 2000
    
    # CONTINUUM LIKELIHOODS
    proportion.tested.likelihood.instructions,
    hiv.test.positivity.likelihood.instructions, 
    awareness.likelihood.instructions,
    suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    prep.uptake.likelihood.instructions,
    prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    heroin.likelihood.instructions.full,
    cocaine.likelihood.instructions.full,
    idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    number.of.tests.year.on.year.change.likelihood.instructions,
    gonorrhea.year.on.year.change.likelihood.instructions,
    ps.syphilis.year.on.year.change.likelihood.instructions
)