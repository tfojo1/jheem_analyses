

# TEST CODE
if (1==2)
{
    source('applications/EHE/ehe_specification.R')
    specification.metadata = get.specification.metadata('ehe', location = "C.12580")
}

# THE ACTUAL FUNCTIONS TO IMPLEMENT

# Should return an array with one dimension, 'race', with a birth rate for each race
get.location.birth.rates <- function(location,
                                     specification.metadata,
                                     years = 2007:2017,
                                     census.manager = CENSUS.MANAGER)
{
    counties = locations::get.contained.locations(location, 'county')
  
    # Pull the births into an array
    # I imagine this should be indexed [year, county, race, ethnicity] - not necessarily in that order
  
    births
  
    # Pull population into an array
    # I imagine this should be indexed [year, county, race, ethnicity] - not necessarily in that order
    population
    
    # Map the ontologies
    birth.ontology.mapping = get.ontology.mapping(from.ontology = dimnames(births)[c('year','race','ethnicity')])
    if (is.null(birth.ontology.mapping))
        stop("We could not find a mapping to take the races in births in the census/Wonder and map them to the races for the '", specification.metadata$version, "' specification")
    
    population.ontology.mapping = get.ontology.mapping(from.ontology = dimnames(population)[c('year','race','ethnicity')])
    if (is.null(birth.ontology.mapping))
      stop("We could not find a mapping to take the races in population in the census/Wonder and map them to the races for the '", specification.metadata$version, "' specification")
    
    births = birth.ontology.mapping$apply(births)
    population = population.ontology.mapping$apply(population)
    
    # Sum up over counties/years and divide births by pop
    apply(births, 'race', sum) / apply(population, 'race', sum)
}

# Should return an array with dimensions 'age', 'race', 'sex'
#  with mortality rate for each stratum
get.location.mortality.rates <- function(location,
                                         specification.metadata,
                                         years = 2007:2017,
                                         census.manager = CENSUS.MANAGER)
{
    # Pull the deaths
    # Pull the population
  
    # Map to the age, race, and sex 
}