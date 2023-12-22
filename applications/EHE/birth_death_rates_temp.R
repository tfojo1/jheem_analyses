
load.data.manager("cached/census.manager.rdata") 

# TEST CODE
if (1==2)
{
    source('applications/EHE/ehe_specification.R')
    specification.metadata = get.specification.metadata('ehe', location = "C.12580")
    get.location.birth.rates(location= "C.12580", specification.metadata = specification.metadata)
}

# THE ACTUAL FUNCTIONS TO IMPLEMENT

# Should return an array with one dimension, 'race', with a birth rate for each race
get.location.birth.rates <- function(location,
                                     specification.metadata,
                                     years = 2007,
                                     census.manager = CENSUS.MANAGER,
                                     surveillance.manager = SURVEILLANCE.MANAGER)
{
    counties = locations::get.contained.locations(location, 'county')
  
    # Pull the births into an array
    # I imagine this should be indexed [year, county, race, ethnicity] - not necessarily in that order
    births=census.manager$pull(outcome = 'births', location = counties, year= years, keep.dimensions = c('race', 'ethnicity', 'location'), na.rm=TRUE)[,,,1] #keep all race, all ethnicities, first source#
  
    # Pull population into an array
    # I imagine this should be indexed [year, county, race, ethnicity] - not necessarily in that order
    population=surveillance.manager$pull(outcome = 'adult.population', location = location, year= years, keep.dimensions = c('race', 'ethnicity'), na.rm=TRUE)[,,1]
    
    # Map the ontologies
    target.dim.names = specification.metadata$dim.names[c('race')]
    map.value.ontology(births, target.dim.names=target.dim.names) / 
      map.value.ontology(population, target.dim.names=target.dim.names)
}

# Should return an array with dimensions 'age', 'race', 'sex'
#  with mortality rate for each stratum
get.location.mortality.rates <- function(location,
                                         specification.metadata,
                                         years = 2007:2017,
                                         census.manager = CENSUS.MANAGER)

{
  states = locations::get.containing.locations(location, "state")
    # Pull the deaths - I expect this will be indexed by year, county, race, ethnicity, and sex (not necessarily in that order)
    deaths = census.manager$pull(outcome = 'metro.deaths', location = states, year= years, keep.dimensions = c('age','race', 'ethnicity', 'sex', 'location'))
  
    # Pull the population - I expect this will be similarly index by year, county, race, ethnicity, and sex
    population = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= years, keep.dimensions = c('age','race', 'ethnicity', 'sex', 'location'))
  
    #You have this denominator it should align wit the mtro deaths
    
    # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
    # then divide the two
    target.dim.names = specification.metadata$dim.names[c('age','race','sex')]
    map.value.ontology(deaths, target.dim.names=target.dim.names) / 
      map.value.ontology(population, target.dim.names=target.dim.names)
}
