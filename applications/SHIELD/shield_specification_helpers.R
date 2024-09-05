# get.base.initial.male.population and get.base.initial.female.population look redundant. why not just use get.base.initial.population.for.sex?


#' get.base.initial.male.population
#'
#' @description genrates the size of 'male' population for given years
#' @param location specific location for which the population is needed
#' @param specification.metadata metadata for the specification
#' @param years years for which the population is needed 
#' @return vector of 'male' population for given years
#' @export
get.base.initial.male.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
  get.base.initial.population.for.sex(location,
                                      specification.metadata = specification.metadata,
                                      sex = 'male',
                                      years = years)
}

get.base.initial.female.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
  get.base.initial.population.for.sex(location,
                                      specification.metadata = specification.metadata,
                                      sex = 'female',
                                      years = years)
}

get.base.initial.population.for.sex <- function(location, specification.metadata, sex, years=DEFAULT.POPULATION.YEARS)
{
  if (length(specification.metadata$dim.names$location)>1)
    stop("We need to specify what to do with more than one location")
  
  if(location=='US') 
    counties='US'
  else
    counties = locations::get.contained.locations(location, 'county')
  pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=years, location=counties, sex=sex),
                            keep.dimensions = c('age','race','ethnicity','sex'),
                            from.ontology.names = 'census') / length(years)
  if (is.null(pop))
    stop(paste0("We couldnt find any population data in the census manager"))
  
  
  mapping = get.ontology.mapping(from.ontology = dimnames(pop),
                                 to.ontology = specification.metadata$dim.names[c('age','race')])
  
  if (is.null(mapping))
    stop(paste0("Cannot get.base.initial.population.for.sex('",
                sex,
                "') - unable to find a mapping from the census to the specification's age and race categorizations"))
  
  mapping$apply(pop, to.dim.names = specification.metadata$dim.names[c('age','race')])
}

#####
#returns a list of age contact proportions for females
get.female.sexual.age.contact.proportions <- function(age.mixing.sd.mult, #multiplier of contact matrix sd
                                                      single.year.female.age.counts,#N
                                                      single.year.age.sexual.availability,#what proportions sexually active
                                                      specification.metadata)
{
  do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                           location=location,
                                           age.mixing.sd.mult = age.mixing.sd.mult,
                                           age.model = PAIRING.INPUT.MANAGER$sex.age.models$female,
                                           age.counts = single.year.female.age.counts,
                                           availability = single.year.age.sexual.availability)
}

get.msm.sexual.age.contact.proportions <- function(age.mixing.sd.mult,
                                                   single.year.msm.age.counts,
                                                   single.year.age.sexual.availability,
                                                   specification.metadata)
{
  do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                           location=location,
                                           age.mixing.sd.mult = age.mixing.sd.mult,
                                           age.model = PAIRING.INPUT.MANAGER$sex.age.models$msm,
                                           age.counts = single.year.msm.age.counts,
                                           availability = single.year.age.sexual.availability)
}

get.heterosexual.male.sexual.age.contact.proportions <- function(age.mixing.sd.mult, 
                                                                 single.year.heterosexual.male.age.counts,
                                                                 single.year.age.sexual.availability,
                                                                 specification.metadata)
{
  do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                           location=location,
                                           age.mixing.sd.mult = age.mixing.sd.mult,
                                           age.model = PAIRING.INPUT.MANAGER$sex.age.models$heterosexual_male,
                                           age.counts = single.year.heterosexual.male.age.counts,
                                           availability = single.year.age.sexual.availability)
}
####
# returns
do.get.age.contact.proportions.for.model <- function(specification.metadata,
                                                     location,
                                                     age.mixing.sd.mult,
                                                     age.model,
                                                     age.counts,
                                                     availability)
  
{
  #-- Call the function --#
  age.cutoffs = specification.metadata$age.endpoints
  age.cutoffs[length(age.cutoffs)] = min(age.cutoffs[length(age.cutoffs)], 
                                         max(as.numeric(names(age.counts))))
  #returns contact matrix by age
  get.age.mixing.proportions(age.delta.intercept.mean=age.model['mean.intercept'],
                             age.delta.slope.mean=age.model['mean.slope'],
                             age.delta.intercept.sd=age.model['sd.intercept'],
                             age.delta.slope.sd=age.model['sd.slope'],
                             age.cutoffs=age.cutoffs,
                             age.labels=specification.metadata$dim.names$age,
                             single.year.age.counts=age.counts*availability,
                             sd.multiplier=age.mixing.sd.mult)
}

# return counts of subgroups in a single year 
get.female.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
  counties = get.contained.locations(location, 'county')
  pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='female'),
                            keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'census') / length(population.years)
  
  array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

get.male.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
  counties = get.contained.locations(location, 'county')
  pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='male'),
                            keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'census') / length(population.years)
  
  array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

get.msm.single.year.age.counts <- function(location, specification.metadata,
                                           population.years=DEFAULT.POPULATION.YEARS)
{

  rv = get.best.guess.msm.proportions(location,
                                      specification.metadata = specification.metadata,
                                      years=population.years,
                                      keep.age = T,
                                      keep.race = F,
                                      return.proportions = F,
                                      ages = NULL)
  ages = parse.age.strata.names(dimnames(rv)$age)$lower
  dimnames(rv)$age = as.character(ages)
  
  rv
}

get.heterosexual.male.single.year.age.counts <- function(location, specification.metadata,
                                                         population.years=DEFAULT.POPULATION.YEARS)
{
  get.male.single.year.age.counts(location = location, population.years = population.years)
  get.msm.single.year.age.counts(location=location, specification.metadata=specification.metadata,
                                 population.years=population.years)
}


# what proportion of younger ages are sexually available
get.sexual.availability <- function()
{
  rv = rep(1, length(CENSUS.AGES))
  names(rv) = CENSUS.AGES
  
  # Assume no sex under 13
  rv[as.character(0:12)] = 0
  
  # From Abma 2017
  availability.13.19 = c('13'=.076, #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6547075/
                         '14'=.108, #halfway between
                         '15'=.13,
                         '16'=.26,
                         '17'=.41,
                         '18'=.55,
                         '19'=.68) / .75
  rv[names(availability.13.19)] = availability.13.19
  
  #from Tessler 2008
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426743/
  rv[as.character(65:74)] = mean(c(67.0/83.7,39.5/61.6))
  rv[as.character(75:as.numeric(names(rv)[length(rv)]))] = mean(c(38.5/83.7,16.7/61.6))
  
  # Return
  array(rv, dim=c(age=length(rv)), dimnames=list(age=names(rv)))
}