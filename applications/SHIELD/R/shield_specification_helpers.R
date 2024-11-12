DEFAULT.POPULATION.YEARS=2010 #used for generating the initial population and sexual contact oes (observed/estimated race estimates)
DEFAULT.MORTALITY.RATE.YEARS=c('2001-2010','2011-2020') #2001:2020
DEFAULT.FERTILITY.RATE.YEARS=c(2007:2023)

# Documentation
# devtools::document("../jheem_analyses/applications/SHIELD/")
# devtools::build_manual("../jheem_analyses/applications/SHIELD/")

# 1. Use @rdname to Group Related Functions
# By using the @rdname tag, you can group multiple functions under the same Rd file.
#This allows you to control the order of documentation by deciding which function goes under which
#@rdname. The documentation of all functions with the same @rdname will be combined under one manual page.


# BASE INITIAL POPULATION SIZE ----
## get base initial populations sizes for different groups ----
#' @title get.base.initial.female.population
#' @description Generates the size of the 'female' population for the given years by calling
#' \code{get.base.initial.population.for.sex} for sex-specific population data.
#' @param location The location for which the population data is being retrieved.
#' @param specification.metadata Metadata for specification.
#' @param years Vector of years for which to retrieve population data. Default is `DEFAULT.POPULATION.YEARS`.
#' @return A 2D matrix showing the number of persons broken down by race (columns) within each age group (rows).
#' @examples
#' get.base.initial.female.population("C.12580", specification.metadata)
#' where: specification.metadata=get.specification.metadata("shield", "C.12580")
get.base.initial.female.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
  get.base.initial.population.for.sex(location,
                                      specification.metadata = specification.metadata,
                                      sex = 'female',
                                      years = years)
}

#'
#' @title get.base.initial.male.population
#' @description Generates the size of the 'male' population for the given years by calling
#' \code{get.base.initial.population.for.sex} for sex-specific population data.
#' @inheritParams get.base.initial.female.population
#' @inherit get.base.initial.female.population return
get.base.initial.male.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
  get.base.initial.population.for.sex(location,
                                      specification.metadata = specification.metadata,
                                      sex = 'male',
                                      years = years)
}

#'
#' @title get.base.initial.population.for.sex
#' @description Generates the size of the population for the given years based on sex.
#' @inheritParams get.base.initial.male.population
#' @param sex The sex of the designated population ('male' or 'female').
#' @return A vector of population for the given years.
get.base.initial.population.for.sex <- function(location, specification.metadata, sex, years=DEFAULT.POPULATION.YEARS)
{
  if (length(specification.metadata$dim.names$location) > 1)
    stop("We need to specify what to do with more than one location")
  
  if (location == 'US'){
    counties = 'US'
  }else{
    counties = locations::get.contained.locations(location, 'county')
  }
  
  pop = CENSUS.MANAGER$pull(outcome = 'population', 
                            dimension.values = list(year = years, location = counties, sex = sex),
                            keep.dimensions = c('age', 'race', 'ethnicity', 'sex'),
                            from.ontology.names = 'stratified.census') / length(years)
  
  if (length(pop)==0)
    stop("We couldn't find any population data in the census manager")
  
  mapping = get.ontology.mapping(from.ontology = dimnames(pop),
                                 to.ontology = specification.metadata$dim.names[c('age', 'race')])
  
  if (is.null(mapping))
    stop(paste0("Cannot get.base.initial.population.for.sex('",
                sex,
                "') - unable to find a mapping from the census to the specification's age and race categorizations"))
  
  mapping$apply(pop, to.dim.names = specification.metadata$dim.names[c('age', 'race')])
}

## get msm popualtion proportion ----
#'
#' @title get.proportion.msm.of.male.by.race.functional.form
#' @description Generates proportion of male who are msm by race
#' @param location location
#' @param specification.metadata specification.metadata
#' @return returns a static functional form for the proportion
get.proportion.msm.of.male.by.race.functional.form <- function(location, specification.metadata)
{
  best.guess.proportions = get.best.guess.msm.proportions(location = location,
                                                          specification.metadata = specification.metadata,
                                                          years = DEFAULT.POPULATION.YEARS,
                                                          keep.race = T,
                                                          keep.age = F)
  create.static.functional.form(best.guess.proportions,
                                link = 'log',
                                value.is.on.transformed.scale = F)
}
#'
#' @title get.best.guess.msm.proportions
#' @description generates proportion of male who are msm 
#' @param location location
#' @param specification.metadata specification.metadata
#' @param years years  
#' @param ages agegroups read from specification.metadata$dim.names$age  
#' @param keep.age keep age  
#' @param keep.race keep race  
#' @param return.proportions return proportions or frequency
#' @return a 2D matrix showing the proportion of MSM by age (rows) and race (columns)
get.best.guess.msm.proportions <- function(location,
                                           specification.metadata,
                                           years = 2013,
                                           ages = CENSUS.AGES,#specification.metadata$dim.names$age,
                                           keep.age = T,
                                           keep.race = T,
                                           return.proportions = T)
{
  if (location=='US'){
    counties='US'
    states='US'
  }else{
    counties = locations::get.contained.locations(location, 'county')
    states = locations::get.overlapping.locations(location, 'state')
  }
  #step1: Get county-level proportions of male who are MSM (available from EMORY)
  # this is the total count for each county-not broken down by age/race
  proportion.msm.by.county = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                                       dimension.values = list(location=counties,
                                                                               sex='male'),
                                                       sources = 'emory')
  
  if (is.null(proportion.msm.by.county) || !setequal(counties, dimnames(proportion.msm.by.county)$location))
    stop(paste0("Cannot get best-guess msm proportions: we don't have data on proportion msm for all counties in location '", location, "'"))
  
  proportion.msm.by.county = apply(proportion.msm.by.county, 'location', mean, na.rm=T)
  if (any(is.na(proportion.msm.by.county)))
    stop(paste0("Cannot get best-guess msm proportions: we don't have data on proportion msm for all counties in location '", location, "' (we get some NAs)"))
  
  # step2: get total population of male 
  # Get number male and flatten race/ethnicity
  males = CENSUS.MANAGER$pull(outcome = 'population',
                              keep.dimensions = c('location', 'age','race','ethnicity'),
                              dimension.values = list(location = counties,
                                                      year = years,
                                                      sex = 'male'),
                              source="census.population",
                              from.ontology.names = 'stratified.census')[,,,,1,drop=FALSE]
  dim.names=dimnames(males)[-length(dimnames(males))]
  dim(males)=sapply(dim.names,length)
  dimnames(males)=dim.names
  
  males = restratify.age.counts(males,
                                desired.age.brackets = CENSUS.AGES,
                                smooth.infinite.age.to = 101)
  
  if (is.null(males))
    stop("Cannot get best-guess msm proportions: we are unable to pull any census data on the number of males")
  if (is.null(ages))
    ages = dimnames(males)$age
  
  #step 3: define the onthology mapping to remove ethnicity (we are trying to align data with proportion.msm below)
  #converts race/ethnicity to race only
  flat.census.reth = c(dimnames(males)$race, 'hispanic')
  flat.census.reth.mapping = create.ontology.mapping(
    from.dimensions = c('race','ethnicity'),
    to.dimensions = 'race',
    mappings = rbind(
      cbind(dimnames(males)$race,
            'not hispanic',
            dimnames(males)$race),
      cbind(dimnames(males)$race,
            'hispanic',
            'hispanic')
    ))
  males = flat.census.reth.mapping$apply(males)
  males[males==0] = 1
  
  # step4: read proportion of msm by race from BRFSS
  # Estimate a proportion msm for each race
  raw.proportion.msm.by.race = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                                         keep.dimensions = c('year','location','race'),
                                                         dimension.values = list(location = states,
                                                                                 sex = 'male'))
  raw.proportion.msm.by.race = apply(raw.proportion.msm.by.race, 'race', mean, na.rm=T)
  raw.races = names(raw.proportion.msm.by.race)
  
  race.mapping = get.ontology.mapping(from.ontology = list(race=raw.races),
                                      to.ontology = dimnames(males)['race'])
  
  if (is.null(race.mapping))
    stop("Cannot map from the BRFSS race ontology for proportion msm to the census ontology")
  
  dim(raw.proportion.msm.by.race) = c(race=length(raw.proportion.msm.by.race))
  dimnames(raw.proportion.msm.by.race) = list(race=raw.races)
  proportions.msm.by.race = race.mapping$apply(raw.proportion.msm.by.race)
  
  if (all(is.na(proportions.msm.by.race)))
    stop("Cannot get best-guess msm proportions: we are getting NA proportions MSM by race at the state level (from BRFSS)")
  
  proportions.msm.by.race[is.na(proportions.msm.by.race)] = mean(proportions.msm.by.race[setdiff(names(proportions.msm.by.race), 'american indian or alaska native')], na.rm=T)
  
  # step6: now that we have sorted out the dimensions, we can use the race.specific estiamtes from BRFSS and apply them to the male population in each county
  # then we compare the calculated proportion of msm with the original proportions reported from EMORY and scale it so that they agree
  # First guess at the number of MSM in each county:
  first.guess.n.msm = sapply(dimnames(males)$race, function(r){
    if (length(dim(males)) == 2) 
      males[,r] * proportions.msm.by.race[r]# for the national model, we have 2 dimentions, age and race
    else 
      males[,,r] * proportions.msm.by.race[r]  # For local models, we have 3 dimensions, age, location, race
  })# '@TODD: to be reviewed
  dim.names = dimnames(males)
  dim(first.guess.n.msm) = sapply(dim.names, length)
  dimnames(first.guess.n.msm) = dim.names
  
  # Scale it to hit the overall proportions in each msm 
  parsed.census.ages = parse.age.strata.names(dimnames(first.guess.n.msm)$age)
  adult.mask = parsed.census.ages$lower >=13 #emory only reports proporiton of msm among adult male
  
  
  first.guess.p.by.county = rowSums(first.guess.n.msm[,adult.mask,,drop=F]) / rowSums(males[,adult.mask,,drop=F])
  
  scale.factor.by.county = proportion.msm.by.county / first.guess.p.by.county 
  
  fitted.n.msm = first.guess.n.msm * scale.factor.by.county 
  # This method ensures that the final estimates align with both the overall MSM proportion from Emory and the racial distribution from BRFSS, 
  # capturing both racial heterogeneity and county-level variation
  
  # step7: reporting: 
  if (keep.race)
  {
    race.mapping = get.ontology.mapping(from.ontology = dimnames(males)['race'],
                                        to.ontology = specification.metadata$dim.names['race'])
    if (is.null(race.mapping))
      stop("Cannot get best-guess msm proportions: we don't have a mapping from census races to the specification's races")
    fitted.n.msm = race.mapping$apply(fitted.n.msm)
    if (return.proportions)
      males = race.mapping$apply(males)
  }
  
  # Map ages
  age.mapping = get.ontology.mapping(from.ontology = dimnames(males)['age'],
                                     to.ontology = list(age=ages))
  if (is.null(age.mapping))
    stop("Cannot get best-guess msm proportions: we don't have a mapping from census ages to the specification's age brackets")
  
  fitted.n.msm = age.mapping$apply(fitted.n.msm)
  if (return.proportions)
    males = age.mapping$apply(males)
  
  # Marginalize to get the probabilities
  keep.dimensions = c('age','race')[c(keep.age, keep.race)]
  
  if (length(keep.dimensions)==0)
  {
    if (return.proportions)
      sum(fitted.n.msm) / sum(males)
    else
      sum(fitted.n.msm)
  }
  else
  {
    if (return.proportions)
      rv = apply(fitted.n.msm, keep.dimensions, sum) / apply(males, keep.dimensions, sum)
    else
      rv = apply(fitted.n.msm, keep.dimensions, sum)
    
    dim.names = dimnames(fitted.n.msm)[keep.dimensions]
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    rv
  }
}






# FERTILITY ----
#' @title get.fertility.rates.functional.form
#' @description generating a functional form for fertility rates based on census data (see inputs/estimate_fertility_rate.R)
#' @param location location
#' @param specification.metadata specification.metadata
#' @param population.years years for which data is available
#' @return a spline functional form for fertility rates by age, race, year 
get.fertility.rates.functional.form<-function(location, specification.metadata, population.years=DEFAULT.FERTILITY.RATE.YEARS){ 
  # pull fertility rates for location
  
  mapped.fertility.rates=get.fertility.rates.from.census(location, specification.metadata,population.years) 
  #
  if (length(mapped.fertility.rates)==0)
    stop(paste0("Cannot get.fertility.rates.from.census() - no 'fertility' data are available in the CENSUS.MANAGER for the counties in location '", location, "' (",
                locations::get.location.name(location), ")"))
  #fit a gausian model to the data; we will use this model to estimate fertility.rate at the knots 
  #alternatively, we could use an average of 5 years around each knot
  # reshape into a datafram
  df= reshape2::melt(mapped.fertility.rates, varnames = c("age", "race", "year"), value.name = "fertility.rate")
  #fit a 3 way model of age race year:
  fit=glm(fertility.rate ~ age * race*year, data = df, family ='gaussian' )
  target.dims=dim(mapped.fertility.rates)[1:2]
  target.dimnames=dimnames(mapped.fertility.rates)[1:2]
  
  # function to predict the fertility rate in a given year
  predict.fertility.rate<-function(fit,time,target.dims,target.dimnames){
    # predict it for the given time point
    predicted.values <- predict(fit, type = "response",newdata=df[df$year==time,])
    # mapp the dimensions
    predicted.values =array(predicted.values,dim =target.dims,dimnames =  target.dimnames) 
  } 
  # predict values for each knot:
  knot1=predict.fertility.rate(fit,2010,target.dims,target.dimnames)
  knot2=predict.fertility.rate(fit,2020,target.dims,target.dimnames)
  
  #define a spline function with 2 knots, and use a modifier to project forward
  ff=create.natural.spline.functional.form(knot.times = c(time1=2010, time2=2020),
                                           knot.values = list(time1=knot1,time2=knot2), #estimated from linear regression above
                                           knot.link = 'log',
                                           knots.are.on.transformed.scale = F,
                                           #how to project forward:
                                           #since linear projections are too extreme, we multiply future prediction by a modifier set at 0.5
                                           after.time = 2030,
                                           after.modifier = 0.5,#modifier.min and .max could sample
                                           modifiers.apply.to.change = T, # if True, modifier is multiplied into diff between knot1 and knot2 values; if False, modifier is multiplied into knot2 value
                                           min = 0 # this is to prevent values from falling below 0
                                           #@TODD: how does this modify the behavior
  )
  return(ff)
  # we can run this to see the example plot:
  #ff=create.natural.spline.functional.form(knot.times = c(time1=2010, time2=2020),
  # knot.values = list(time1=knot1,time2=knot2), #estimated from linear regression above
  # #how to project forward:
  # #since linear projections are too extereme, we multiply future prediction by a modifier set at 0.5
  # after.time = 2030,
  # after.modifier = 0.5,#modifier.min and .max could sample
  # modifiers.apply.to.change = T, # if True, modifier is multiplied into diff between knot1 and knot2 values; if False, modifier is multiplied into knot2 value
  # min = 0 # this is to prevent values from falling below 0
  # )
  # qplot(2007:2040,sapply(ff$project(2007:2040),function(x){x[[12]]}))
}

#' @title get.fertility.rates.from.census
#' @description reading the fertility rates from the census manager
#' @param location location
#' @param specification.metadata specification.metadata
#' @param population.years population.years
#' @return returning the fertility rates in the correct dimension
get.fertility.rates.from.census<-function(location, specification.metadata, population.years=DEFAULT.FERTILITY.RATE.YEARS){
  #1- extract data from the census manager
  if (location=='US'){
    counties='US'
  }else{
    counties=locations::get.contained.locations(location, 'county') #extract the counties for the given location
    allCounties=(dimnames(CENSUS.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)$location)
    #remove counties that are missing
    counties=intersect(counties,allCounties)
  }
  if(length(counties)==0)
    stop(paste0("Cannot get.fertility.rates.from.census() - no 'fertility' data are available in the CENSUS.MANAGER for the counties in location '", location, "' (",
                locations::get.location.name(location), ")"))
  
  fertility.rate = CENSUS.MANAGER$pull(outcome='fertility.rate',
                                       location = counties,
                                       year= population.years,
                                       keep.dimensions = c('location','age','race', 'ethnicity','year'),  
                                       na.rm=TRUE)
  if (is.null(fertility.rate))
    stop(paste0("Cannot get.fertility.rates.from.census() - no 'fertility' data are available in the CENSUS.MANAGER for the counties in location '", location, "' (",
                locations::get.location.name(location), ")"))
  female.population=CENSUS.MANAGER$pull(outcome='female.population',
                                        location = counties,
                                        year= population.years,
                                        keep.dimensions = c('location','age','race', 'ethnicity','year'),  
                                        na.rm=TRUE)
  
  female.population[is.na(fertility.rate)]<-NA #we assume anywhere that fertility rate is NA, female population should be NA (ptherwise we count them in denom without accessing their birth ifo)
  births=fertility.rate*female.population
  
  #2-map the dimensions to the target dimensions
  # target.dimnames: we set this manually to include female of childbearing ages and correct years
  #@PK: I need to make this more generalizable 
  target.dimnames=target.dimnames <- list(
    age = c(  "15-19 years", "20-24 years", "25-29 years", "30-34 years",
              "35-39 years", "40-44 years"  ),
    race = c("black", "hispanic", "other"),
    year = as.character(population.years)
  )
  
  mapped.births=map.value.ontology(births, 
                                   target.dim.names = target.dimnames,
                                   na.rm = TRUE)
  mapped.female.population=map.value.ontology(female.population, 
                                              target.dim.names = target.dimnames,
                                              na.rm = TRUE)
  mapped.fertility.rate=mapped.births/mapped.female.population
  
  return(mapped.fertility.rate)
}

# MORTALITY ----
#' @title get.location.mortality.rates.functional.form
#' @description generating a functional form for mortality rates based on census data
#' @param location location
#' @param specification.metadata specification.metadata
#' @param population.years population.years
#' @return a functional form for mortality rates to be used in the specification
get.location.mortality.rates.functional.form = function(location, specification.metadata, population.years=DEFAULT.MORTALITY.RATE.YEARS)
{
  rates = get.location.mortality.rates(location=location,
                                       specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
}
#' @title get.location.mortality.rates
#' @description reading the mortality rates from the census manager (approximating them off state-level data)
#' @param location location
#' @param specification.metadata specification.metadata
#' @param year.ranges year.ranges  
#' @return returning the mortality rates for each MSA (location) in the correct dimension
get.location.mortality.rates <- function(location,
                                         specification.metadata,
                                         year.ranges = DEFAULT.MORTALITY.RATE.YEARS){
  # The code is designed for modeling MSAs, where each MSA consists of a collection of counties.
  # County-level mortality data is not fully stratified, but state-level data is available with stratification.
  # Note: Some MSAs span over multiple states (e.g., Washington DC  ).
  # To estimate the MSA-level mortality rate, we extract the counties within each MSA, map these counties to their corresponding states,
  # and then take a weighted average of the state-level rates to approximate the MSA-level mortality rate.
  
  if (location=='US'){
    counties='US'
    states='US'
    #
    deaths = CENSUS.MANAGER$pull(outcome = 'deaths',
                                         location = counties,
                                         from.ontology.names = "census.cdc.wonder.births.deaths",
                                         keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
    if (is.null(deaths))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
    
    # Pull the death denominator:
    population = CENSUS.MANAGER$pull(outcome = 'deaths.denominator', 
                                     location = states, 
                                     keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
    if (is.null(population))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
    population[is.na(deaths)] = NA #remove the stratifications with no denominator
    
    # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification # then divide the two
    target.dim.names = c(list(location=states), specification.metadata$dim.names[c('age','race','sex')])
    national.mortality.rate = map.value.ontology(deaths, target.dim.names=target.dim.names, na.rm = T) / 
      map.value.ontology(population, target.dim.names=target.dim.names, na.rm = T)
    # dimnames(national.mortality.rate)
    return(national.mortality.rate)
    
  }else{
    counties=locations::get.contained.locations(location, 'county') #extract the counties for the given location
    states = unique(locations::get.containing.locations(counties, 'state')) # construct the states: 
    # Pull the deaths - I expect this will be indexed by year, county, race, ethnicity, and sex (not necessarily in that order)
    deaths = CENSUS.MANAGER$pull(outcome = 'metro.deaths', 
                                 location = states, 
                                 year= year.ranges, 
                                 keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
    
    if (is.null(deaths))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
    
    # Pull the population - I expect this will be similarly index by year, county, race, ethnicity, and sex
    population = CENSUS.MANAGER$pull(outcome = 'metro.deaths.denominator', 
                                     location = states, 
                                     year= year.ranges, 
                                     keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
    if (is.null(population))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
    population[is.na(deaths)] = NA
    
    # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
    # then divide the two
    target.dim.names = c(list(location=states), specification.metadata$dim.names[c('age','race','sex')])
    rates.by.state = map.value.ontology(deaths, target.dim.names=target.dim.names, na.rm = T) / 
      map.value.ontology(population, target.dim.names=target.dim.names, na.rm = T)
    
    if (any(is.na(rates.by.state)))
      stop("getting NA values in rates.by.state in get.location.mortality.rates()")
    
    if (length(states)==1)
      rates.by.state[1,,,]
    else
    {
      county.populations = CENSUS.MANAGER$pull(outcome = 'population',
                                               dimension.values = list(location=counties,
                                                                       year=year.ranges),
                                               from.ontology.names = 'stratified.census')
      
      if (is.null(county.populations))
        stop("Error in get.location.mortality.rates(): cannot get populations for the component counties")
      county.populations = apply(county.populations,
                                 'location', mean, na.rm=T)
      total.population = sum(county.populations, na.rm=T)
      
      state.weights = sapply(states, function(st){
        counties.in.state.and.loc = intersect(counties,
                                              locations::get.contained.locations(st, 'county'))
        
        sum(county.populations[counties.in.state.and.loc], na.rm=T)/total.population
      })
      
      apply(state.weights * rates.by.state, c('age','race','sex'), sum)
    }
  }
}







##-----------##
# AGING ----
#' #' @title get.empiric.aging.rates
#' #' @description  creates a spline function for aging rates
#' #' @param location location
#' #' @param specification.metadata specification.metadata
#' #' @param years knots
#' #' @return returns a vector of aging rates that is proportional to each compartment size
get.empiric.aging.rates <- function(location, specification.metadata,
                                    years=c('time1'=2010,'time2'=2020) )
{
  aging.rates = do.get.empiric.aging.rates(location = location,
                                           specification.metadata = specification.metadata,
                                           years = years)
  
  create.natural.spline.functional.form(knot.times = years,
                                        knot.values = aging.rates,
                                        link = 'log',
                                        knots.are.on.transformed.scale = F)
}
#' #' @title do.get.empiric.aging.rates
#' #' @description  retrieves the data required for computing emperic aging rates
#' #' @param location location
#' #' @param specification.metadata specification.metadata
#' #' @param years knots
#' #' @return returns a list of aging rates that is proportional to each compartment size between the years identified
do.get.empiric.aging.rates <- function(location, 
                                       specification.metadata,
                                       years=c('time1'=2010,'time2'=2020)
                                       #                                       force.match.age.brackets.to.before.smoothing = NULL #if we wanted to use diff age brackets
)
{
  if (location=='US')
    counties='US'
  else
    counties = locations::get.contained.locations(location, 'county')
  # pull data
  years.to.pull = min(years):max(years)
  pop = CENSUS.MANAGER$pull(outcome='population',
                            dimension.values = list(location = counties,
                                                    year = as.character(years.to.pull)),
                            keep.dimensions = c('year','age','race','ethnicity','sex'),
                            from.ontology.names = 'stratified.census')
  if (is.null(pop))
    stop("There was no population data at all for location ", location, " between ", min(years), " and ", max(years))
  #
  max.year = max(as.numeric(dimnames(pop)$year))
  min.year = min(as.numeric(dimnames(pop)$year))
  
  #1- map RACE ontology:
  race.mapping = get.ontology.mapping(dimnames(pop)[c('race','ethnicity')],
                                      specification.metadata$dim.names['race'])
  pop = apply(pop, c('year','age','race','ethnicity','sex'), sum, na.rm=T)
  pop = race.mapping$apply(pop)
  # dimnames(pop)
  #@TODD: how can we make sure that age is reported as yearly numerical values and what to do if it's not
  
  # if (!is.null(force.match.age.brackets.to.before.smoothing))
  # {
  #   age.mapping = get.ontology.mapping(from.ontology = dimnames(pop)['age'],
  #                                      to.ontology = list(age=force.match.age.brackets.to.before.smoothing))
  #   
  #   if (is.null(age.mapping))
  #     stop("Cannot infer empiric aging rates: don't know how to map age to the brackets given in 'force.match.age.brackets.to.before.smoothing'")
  #   pop = age.mapping$apply(pop)
  # }
  
  min.specification.age = specification.metadata$age.lower.bounds[1]
  max.specification.age = max(specification.metadata$age.upper.bounds[specification.metadata$age.upper.bounds<Inf])
  
  pop = restratify.age.counts(pop,
                              desired.age.brackets = min.specification.age:max.specification.age,
                              smooth.infinite.age.to = 101,
                              allow.extrapolation = T)
  
  # Compute aging rates:
  aging.rates = lapply(years, function(year){
    year = as.character(min(max(year,min.year),max.year)) #!onces the census data end, it applies the last year of data and projects that forward  (e.g., for 2030, it uses data in 2017)
    raw.rates = sapply(1:(specification.metadata$n.ages-1), function(age.index){
      age.upper.bound = specification.metadata$age.upper.bounds[age.index] - 1
      age.lower.bound = specification.metadata$age.lower.bounds[age.index]
      age.bracket.ages = paste0(age.lower.bound:age.upper.bound, ' years')
      age.bracket.ages[age.bracket.ages == '1 years'] = '1 year'
      
      #@Todd:we have an issue here with <1 year and 1 year agegroups. and also 85+ years
      # if (age.bracket.ages[1]=="0 years") age.bracket.ages[1]<-"< 1 year"
      # if (age.bracket.ages[2]=="1 years") age.bracket.ages[2]<-"1 year"
      #@Todd: what about those over 64 years?
      
      # aging rate for the last agegroup in each braket
      pop[year, paste0(age.upper.bound, ' years'),,] /
        colSums(pop[year,age.bracket.ages,,], dims=1)
    })
    dim.names = c(dimnames(pop)[c('race','sex')], list(age=specification.metadata$dim.names$age[-1]))
    dim(raw.rates) = sapply(dim.names, length)
    dimnames(raw.rates) = dim.names
    
    # reconfiguring the dimensions:
    desired.dim.names = specification.metadata$dim.names[c('age','race','sex')]
    desired.dim.names$age = desired.dim.names$age[-length(desired.dim.names$age)] #TODD: why are we excluding the last agegroup here?
    
    # mapping to our sex groups
    sex.mapping = c(msm='male', heterosexual_male='male', female='female')
    rates.by.sex = sapply(desired.dim.names$sex, function(sex){
      sex.from = sex.mapping[sex]
      if (is.na(sex.from))
        stop("'get.empiric.aging.rates' in the ehe_specification_helpers is hard-coded for sex = [msm, heterosexual_male, female]. You will need to modify this function to accomodate additional sex categories")
      raw.rates[,sex.from,]
    })
    dim.names = desired.dim.names[c('race','age','sex')]
    dim(rates.by.sex) = sapply(dim.names, length)
    dimnames(rates.by.sex) = dim.names
    #
    rv = expand.array(rates.by.sex, target.dim.names = desired.dim.names)
    dim(rv)
    rv
  })
  names(aging.rates) = names(years)
  aging.rates
}
# SEXUAL CONTACT BY AGE ----
#' @title functions.sexual.contact.model
#' @description sexual contacts are charactrized via 4 components: 1)transmission probability, 2)age mixing, 3)sex mixing, and 4)race mixing
#' @references \file{inst/docs/sexual_contacts.docx}
functions.sexual.contact.model<-function(){}

#' @title get.female.sexual.age.contact.proportions
#' @description returns a list of age contact proportions for females
#' @param age.mixing.sd.mult multiplier of the standard deviation of the age mixing model (diff_ages_partners ~ Normal(mu,sd))
#' used for calibration by calling \code{do.get.age.contact.proportions.for.model}
#' @param single.year.female.age.counts number of individuals within each nominal age year for each age group
#' @param single.year.age.sexual.availability proportion of individuals within each nominal age year available engaged in sexual activity
#' @param specification.metadata specification.metadata
#' @return OUTPUT_DESCRIPTION
#' @references \file{inst/docs/sexual_contacts.docx}
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

#' @title get.msm.sexual.age.contact.proportions
#' @description returns a list of age contact proportions for msm
#' @inheritParams get.female.sexual.age.contact.proportions
#' @inherit get.female.sexual.age.contact.proportions return
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

#' @title get.heterosexual.male.sexual.age.contact.proportions
#' @description returns a list of age contact proportions for het male
#' @inheritParams get.female.sexual.age.contact.proportions
#' @inherit get.female.sexual.age.contact.proportions return
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


#' @title do.get.age.contact.proportions.for.model
#' @description returns a list of age contact proportions for designated group
#' @param location location
#' @param age.model specific age.model used to inform new partnership probabilities
#' @inheritParams get.female.sexual.age.contact.proportions
#' @return OUTPUT_DESCRIPTION
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
                             single.year.age.counts=age.counts[names(availability)]*availability,
                             sd.multiplier=age.mixing.sd.mult)
}

#' @title get.female.single.year.age.counts
#' @description return counts of female in a single year
#' @param location location
#' @param population.years population.years
#' @return returns the number of females in single year ages
get.female.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
  if (location=='US')
    counties='US'
  else 
    counties = get.contained.locations(location, 'county')
  pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='female'),
                            keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'stratified.census') / length(population.years)
  pop = restratify.age.counts(pop,
                              desired.age.brackets = CENSUS.AGES,
                              smooth.infinite.age.to = 101,
                              allow.extrapolation = T)
  
  array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

#' @title get.male.single.year.age.counts
#' @description return counts of male in a single year
#' @inheritParams get.female.single.year.age.counts
#' @inherit get.female.single.year.age.counts return
get.male.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
  if (location=='US')
    counties='US'
  else 
    counties = get.contained.locations(location, 'county')
  pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='male'),
                            keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'stratified.census') / length(population.years)
  pop = restratify.age.counts(pop,
                              desired.age.brackets = CENSUS.AGES,
                              smooth.infinite.age.to = 101,
                              allow.extrapolation = T)
  
  array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

#' @title get.msm.single.year.age.counts
#' @description return counts of msm in a single year
#' @inheritParams get.female.single.year.age.counts
#' @inherit get.female.single.year.age.counts return
get.msm.single.year.age.counts <- function(location, specification.metadata,
                                           population.years=DEFAULT.POPULATION.YEARS)
{
  
  rv = get.best.guess.msm.proportions(location,
                                      specification.metadata = specification.metadata,
                                      years=population.years,
                                      keep.age = T,
                                      keep.race = F,
                                      return.proportions = F,
                                      ages = CENSUS.AGES)
  ages = parse.age.strata.names(dimnames(rv)$age)$lower
  dimnames(rv)$age = as.character(ages)
  
  rv
}

#' @title get.heterosexual.male.single.year.age.counts
#' @description To determine the proportion of the population that falls into specific age buckets
#' @param location location
#' @param specification.metadata specification.metadata
#' @param population.years population.years
#' @return returns the number of heterosexual males in single year ages
get.heterosexual.male.single.year.age.counts <- function(location, specification.metadata,
                                                         population.years=DEFAULT.POPULATION.YEARS)
{
  get.male.single.year.age.counts(location = location, population.years = population.years)
  get.msm.single.year.age.counts(location=location, specification.metadata=specification.metadata,
                                 population.years=population.years)
}


#' @title 1-get.sexual.availability
#' @description Determines the proportion of people in each age bucket that are sexually available
#' @return 1D vector with proportion of people in each age bucket that are sexually available
#' @details The model reflects an increase in sexual activity starting from age 13, reaching 100% at
#'  ages 20 to 64, and gradually tapering off until age 85, the final age group.
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

get.race.population.counts <- function(location,
                                       specification.metadata,
                                       years=DEFAULT.POPULATION.YEARS)
{
  if(location=='US')
    counties='US'
  else
  counties = locations::get.contained.locations(location, 'county')
  population = CENSUS.MANAGER$pull(outcome='population',
                                   dimension.values = list(location=counties, year=years),
                                   keep.dimensions = c('location','age','race','ethnicity', 'sex'),
                                   from.ontology.names = 'stratified.census')
  
  if (is.null(population))
    stop(paste0("Cannot get.race.population.counts() - no census data were available for the counties of '", location, "'"))
  
  parsed.ages = parse.age.strata.names(dimnames(population)$age)
  age.mask = parsed.ages$lower >= specification.metadata$age.lower.bounds[1] &
    parsed.ages$upper <= specification.metadata$age.upper.bounds[specification.metadata$n.ages]
  
  race.mapping = get.ontology.mapping(from.ontology = dimnames(population)[c('race','ethnicity')],
                                      to.ontology = specification.metadata$dim.names['race'])
  
  if (is.null(race.mapping))
    stop("Cannot get.race.population.counts() - don't know how to map race to the desired ontology for the specification")
  
  race.mapping$apply(population[,age.mask,,,,], to.dim.names = specification.metadata$dim.names['race'])
}

# SEXUAL CONRACT BY RACE ----
##-------------##
##-- Pairing --##
##-------------##
#
#
#' @title get.geographically.aggregated.race.oes
#' @description within.county.race.oes[to,from] is how much more likely someone it is for a person of race to to have a partner of race from, relative to race from's population prevalence
#' @param location location
#' @param specification.metadata specification.metadata
#' @param within.county.race.oes observed to expected ration for mixing by age within each county
#' @param years years
#' @param as.functional.form Should return a functional form instead of values? Default: T
#' @return returns observed to expected (OES) ratio for race mixing 
get.geographically.aggregated.race.oes <- function(location,
                                                   specification.metadata,
                                                   within.county.race.oes,
                                                   years = DEFAULT.POPULATION.YEARS,
                                                   as.functional.form=T)
{
  if (!is.numeric(within.county.race.oes) || is.null(dim(within.county.race.oes)) || length(dim(within.county.race.oes))!=2)
    stop("Cannot get.geographically.aggregated.race.oes() - within.county.race.oes must be a 2-dimensional matrix")
  
  if (is.null(dimnames(within.county.race.oes)) ||
      !setequal(dimnames(within.county.race.oes)[[1]], specification.metadata$dim.names$race) ||
      !setequal(dimnames(within.county.race.oes)[[2]], specification.metadata$dim.names$race))
    stop("Cannot get.geographically.aggregated.race.oes() - within.county.race.oes must have dimnames set, and the names of both dimensions must match the specification's race categories")
  
  if(location=='US')
    counties='US'
  else
    counties = locations::get.contained.locations(location, 'county')
  population = CENSUS.MANAGER$pull(outcome='population',
                                   dimension.values = list(location=counties, year=years),
                                   keep.dimensions = c('location','age','race','ethnicity', 'sex'),
                                   from.ontology.names = 'stratified.census')
  
  if (is.null(population))
    stop(paste0("Cannot get.geographically.aggregated.race.oes() - no census data were available for the counties of '", location, "'"))
  
  parsed.ages = parse.age.strata.names(dimnames(population)$age)
  age.mask = parsed.ages$lower >= specification.metadata$age.lower.bounds[1] &
    parsed.ages$upper <= specification.metadata$age.upper.bounds[specification.metadata$n.ages]
  
  race.mapping = get.ontology.mapping(from.ontology = dimnames(population)[c('race','ethnicity')],
                                      to.ontology = specification.metadata$dim.names['race'])
  
  
  if (is.null(race.mapping))
    stop("Cannot get.geographically.aggregated.race.oes() - don't know how to map race to the desired ontology for the specification")
  
  mapped.dim.names = c(list(location=counties),
                       specification.metadata$dim.names['race'])
  mapped.population = race.mapping$apply(population[,age.mask,,,,,drop=F], to.dim.names = mapped.dim.names)
  
  #population.race.fractions = mapped.population / rowSums(mapped.population)
  
  races = specification.metadata$dim.names$race
  projected.n.partners = sapply(races, function(race.from){
    sapply(races, function(race.to){
      sum(within.county.race.oes[race.to, race.from] * mapped.population[,race.from] * mapped.population[,race.to] / rowSums(mapped.population))
    })
  })
  
  race.counts = colSums(mapped.population)
  expected.race.proportions = race.counts / sum(race.counts)
  
  projected.race.proportions = projected.n.partners / rowSums(projected.n.partners)
  
  oes = projected.race.proportions / rep(expected.race.proportions, each=length(races))
  
  dim.names = list(race.to = races,
                   race.from = races)
  dim(oes) = sapply(dim.names, length)
  dimnames(oes) = dim.names
  
  if (as.functional.form)
    create.static.functional.form(value = oes, link='log', value.is.on.transformed.scale = F)
  else
    oes
}

#' @title sexual.oes.to.contact.proportions
#' @description trasforming oe values to proportions of mixing with other races (sum to 1)
#' calling code \code{oes.to.proportions}
#' @param race.sexual.oes oes
#' @param race.population.counts number of people by race
#' @return transforms observed to expected (oe) mixing ratios to proportions of contact with each race
sexual.oes.to.contact.proportions <- function(race.sexual.oes,
                                              race.population.counts)
{
  oes.to.proportions(oes = race.sexual.oes, population = race.population.counts)
}

#' @title oes.to.proportions
#' @description trasforming oe values to proportions of mixing with other groups (sum to 1)
#' @param oes oes betwen groups
#' @param population popualtion count in each group
#' @return transforms observed to expected (oe) mixing ratios to proportions of contact with each race
oes.to.proportions <- function(oes, population)
{
  raw = oes * rep(population[ dimnames(oes)[[1]] ], each=length(population))
  raw / rowSums(raw)
}

