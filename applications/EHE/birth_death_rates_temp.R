 
# TEST CODE
if (1==2)
{
    source('applications/EHE/ehe_specification.R')
    specification.metadata = get.specification.metadata('ehe', location = "C.12580")
    x = get.location.birth.rates(location = "C.12580", specification.metadata = specification.metadata)
    y = get.location.mortality.rates(location = "C.12580", specification.metadata = specification.metadata)
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
                                         year.ranges = c('2006-2010','2011-2015'),
                                         census.manager = CENSUS.MANAGER)

{
  states = locations::get.containing.locations(location, "state")
    # Pull the deaths - I expect this will be indexed by year, county, race, ethnicity, and sex (not necessarily in that order)
    deaths = census.manager$pull(outcome = 'metro.deaths', location = states, year= year.ranges, keep.dimensions = c('age','race', 'ethnicity', 'sex', 'location'))
  
    if (is.null(deaths))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
    
    # Pull the population - I expect this will be similarly index by year, county, race, ethnicity, and sex
    population = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year.ranges, keep.dimensions = c('age','race', 'ethnicity', 'sex', 'location'))
    
    if (is.null(population))
      stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
    
    #You have this denominator it should align wit the metro deaths
    
    # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
    # then divide the two
    target.dim.names = specification.metadata$dim.names[c('age','race','sex')]
    map.value.ontology(deaths, target.dim.names=target.dim.names) / 
      map.value.ontology(population, target.dim.names=target.dim.names)
}


###############################################################################
#Adjusting to account for MSAs that span multiple states
#Also adjusting for mortality = 0
###############################################################################

get.location.mortality.rates <- function(location,
                                         specification.metadata,
                                         year.ranges = c('2006-2010','2011-2015'),
                                         census.manager = CENSUS.MANAGER)
  
{
  states = locations::get.containing.locations(location, "state")
  counties = locations::get.contained.locations(location, 'county')
  
  #Pull state metro deaths (numerator)
  state.metro.deaths = census.manager$pull(outcome = 'metro.deaths', location = states, year= year, keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
  
  #Set up for if deaths = 0, need to set to the minimum value calculated in other stratum
  
  #Question: How to have the function apply the minimum value calculated here?
  
  if (is.null(deaths))
    stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
  
  #Pull state metro population (denominator)
  state.metro.pop = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year, keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
  if (is.null(population))
    stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
  
  #Divide to get state metro mortality rate *But you could have multiple of these if the MSA is in multiple states*
  state.metro.death.rate = (state.metro.deaths/state.metro.pop) #this will be used in the final equation
  
  #Question: How to get the proportion each state contributes to the MSA?
  #Question: How to account for not knowing how many states could be covered in a particular MSA? 
  
  #First get the population of the MSA (by summing county population values from census)
    msa.population = census.manager$pull(outcome = 'population', location = counties, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex')) #this would add up all populations from counties#
 
  #Question: How to apply this in a useful way?
    
  #Use locations package to determine counties in each MSA and which MSA are in multiple states
  #get.overlapping.locations("C.35620", "state")
  counties.in.each.msa = locations::get.contained.locations(MSAS.OF.INTEREST, "county", return.list = T)

  #Determine what proportion of the MSA is in State One
  state.1.msa.pop = census.manager$pull(outcome='population', location=counties.in.each.msa, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex'))
  state.1.proportion = (state.1.msa.pop/msa.population)  #This the proportion value the state contributes to the MSA
  
  #Determine what proportion of the MSA is in State Two
  state.2.msa.pop = census.manager$pull(outcome='population', location=counties.in.each.msa, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex'))
  state.2.proportion = (state.2.msa.pop/msa.population)  #This the proportion value the state contributes to the MSA
  
 #Apply weights to create a final scaled mortality rate
  final.msa.death.rate = (proportion.state.1 * state.metro.death.rate.1) + (proportion.state.2 * state.metro.death.rate.2) + (proportion.state.3 * state.metro.death.rate.3)
  
  
#Question: How to have the function return an array of values?
  
  # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
  # then divide the two
  target.dim.names = specification.metadata$dim.names[c('age','race','sex')]
  map.value.ontology(deaths, target.dim.names=target.dim.names) / 
    map.value.ontology(population, target.dim.names=target.dim.names)
  
}