# load.data.manager(name="smaller.census.manager", file="cached/smaller.census.manager.rdata")
# library(jheem2)
# library(locations)

source('../jheem2/R/HELPERS_array_helpers.R')

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
###############################################################################
#This is my current working function as of 2-202-24
###############################################################################
###############################################################################

test <- function(msa.name,
                 year.ranges = c('2006-2010','2011-2015'),
                                 census.manager = CENSUS.MANAGER){
  
  #Identify states and counties in the MSA
  states = locations::get.overlapping.locations(msa.name, "state") 
  counties = locations::get.contained.locations(msa.name, 'county')
  
  #Pull numerator and denominator to calculate the State Metro Death Rate
  state.metro.deaths = census.manager$pull(outcome = 'metro.deaths', dimension.values = list(location = states, year= year.ranges), keep.dimensions = c('location', 'year', 'age', 'race', 'ethnicity', 'sex'))
  state.metro.pop = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year.ranges, keep.dimensions = c('location','year', 'age', 'race', 'ethnicity', 'sex'))
  state.metro.death.rate = (state.metro.deaths/state.metro.pop) 

  #Returns the population of the MSA
    #Eventually need to fix year 
  msa.population = census.manager$pull(outcome = 'population', dimension.values = list(location = counties, year= 2006:2015), keep.dimensions = c()) 
  
  #Return what counties are in the MSA
  counties.in.this.msa = locations::get.contained.locations(msa.name, "county")
  #Returns the counties in each state
  counties.in.states = lapply(states, function(state){
    counties.in.this.state = locations::get.contained.locations(state, "county")
  })
  #Returns the overlap of what counties are in the MSA and in each state
  counties.in.states.and.msa = lapply(counties.in.states, function(counties.in.this.state){
    intersect(counties.in.this.msa, counties.in.this.state)
  })

  browser()

  #Need to find the proportion that each state contributes to the MSA
  list.of.states.proportions.in.msa = lapply(counties.in.states.and.msa, function(msa.counties.in.each.state){
    states.population = census.manager$pull(outcome='population', location=msa.counties.in.each.state, year = 2006:2015, keep.dimensions = c("year")) #2-20-24 this isn't running
    states.proportion = states.population/msa.population 
  }) 
  
####################################################
  #I dont think you need this section anymore:
  # #Returns list of one array per state
  #   current.dimnames = dimnames(list.of.states.proportions.in.msa[[1]])
  #   new.dimnames = c(current.dimnames, list(location = c("MA", "NH")))
  # 
  #   combined.state.proportion.array = array(c(list.of.states.proportions.in.msa[[1]], list.of.states.proportions.in.msa[[2]]), dim=sapply(new.dimnames, length), dimnames= new.dimnames)
#####################################################
    
  
  #Apply weights to create a final scaled mortality rate
  #Update for 2.20-proportion state 1 will become just a number not an array
  
    #final.msa.death.rate = (state.proportion.in.msa * state.metro.death.rate) #need to be in same order

}
