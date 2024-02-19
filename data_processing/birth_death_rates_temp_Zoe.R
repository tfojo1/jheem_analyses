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
#THIS IS WHERE I'M WORKING 2-1-24
###############################################################################
###############################################################################

test <- function(msa.name,
                 year.ranges = c('2006-2010','2011-2015'),
                                 census.manager = CENSUS.MANAGER){
  
  states = locations::get.overlapping.locations(msa.name, "state") 
  counties = locations::get.contained.locations(msa.name, 'county')
  
  state.metro.deaths = census.manager$pull(outcome = 'metro.deaths', dimension.values = list(location = states, year= year.ranges), keep.dimensions = c('location', 'year', 'age', 'race', 'ethnicity', 'sex'))
  state.metro.pop = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year.ranges, keep.dimensions = c('location','year', 'age', 'race', 'ethnicity', 'sex'))
  state.metro.death.rate = (state.metro.deaths/state.metro.pop) 

  #Need to fix 'year' here
  #QUESTION: Is all I need to do here now update to year ranges and it'll aggregate over the years?  (same for line 110?)
  msa.population = census.manager$pull(outcome = 'population', dimension.values = list(location = counties, year= 2006:2015), keep.dimensions = c('age', 'race', 'ethnicity', 'sex')) #this would add up all populations from counties; ideally want this to aggregate across year ranges even though data isnt in year ranges#
  
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

  #Creates a list of the proportion that each state contributes to the MSA
  #Then we turn this list into an array of states and their proportions
  list.of.states.proportions.in.msa = lapply(counties.in.states.and.msa, function(msa.counties.in.each.state){
    states.population = census.manager$pull(outcome='population', location=msa.counties.in.each.state, year = 2006:2015, keep.dimensions = c('age', 'race', 'ethnicity', 'sex'))
    states.proportion = states.population/msa.population 
  }) #Returns list of one array per state
    current.dimnames = dimnames(list.of.states.proportions.in.msa[[1]])
    new.dimnames = c(current.dimnames, list(location = c("MA", "NH")))

    combined.state.proportion.array = array(c(list.of.states.proportions.in.msa[[1]], list.of.states.proportions.in.msa[[2]]), dim=sapply(new.dimnames, length), dimnames= new.dimnames)
    #QUESTION: How to properly use this function Todd wrote?
    # combined.state.proportion.array = expand.array(to.expand=(list.of.states.proportions.in.msa[[1]], list.of.states.proportions.in.msa[[2]]), 
    #                                                target.dim.names = new.dimnames)
    

  #Next issue: you are going to attempt to multiply arrays but they have different categories for their dimensions
    final.msa.death.rate = (combined.state.proportion.array * state.metro.death.rate) #need to be the same size with dimnames in the same order
    
    
  #Apply weights to create a final scaled mortality rate
  #final will be array stratified by age, race, eth, sex
  #Could have different array for each states and multiple it by the metro death rate array (then add all together)
  final.msa.death.rate = (proportion.state.1 * state.metro.death.rate.1) + (proportion.state.2 * state.metro.death.rate.2) #need to be the same size with dimnames in the same order
  
}


##############################################################################
# #NOTE FROM 2-1-24 THIS IS OUT OF DATE##
# 
# #Working with one MSA at a time
# 
# get.location.mortality.rates.new <- function(msa.name,
#                                              #specification.metadata,
#                                              year.ranges = c('2006-2010','2011-2015'),
#                                              census.manager = smaller.census.manager)
#   
# {
# 
#   states = locations::get.containing.locations(msa.name, "state")
#   counties = locations::get.contained.locations(msa.name, 'county')
#   
#   #Pull state metro deaths (numerator)
#   state.metro.deaths = CENSUS.MANAGER$pull(outcome = 'metro.deaths', dimension.values = list(location = states, year= year.ranges), keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
#   
#   #Set up for if deaths = 0, need to set to the minimum value calculated in other stratum
#   
#   #Question: How to have the function apply the minimum value calculated here?
#   
#   if (is.null(deaths))
#     stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
#   
#   #Pull state metro population (denominator)
#   state.metro.pop = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year, keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
#   if (is.null(population))
#     stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
#   
#   #Divide to get state metro mortality rate *But you could have multiple of these if the MSA is in multiple states*
#   state.metro.death.rate = (state.metro.deaths/state.metro.pop) #this will be used in the final equation
#   
#   #Question: How to get the proportion each state contributes to the MSA?
#   #Question: How to account for not knowing how many states could be covered in a particular MSA? 
#   
#   #First get the population of the MSA (by summing county population values from census)
#   msa.population = census.manager$pull(outcome = 'population', location = counties, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex')) #this would add up all populations from counties#
#   
#   #Question: How to apply this in a useful way?
#   
#   #Use locations package to determine counties in each MSA and which MSA are in multiple states
#   #get.overlapping.locations("C.35620", "state")
#   #This function will only apply to one MSA at a time so this won't be MSAs of Interest it'll be the singular MSA (aka location)
#   counties.in.this.msa = locations::get.contained.locations(msa.name, "county")
#   browser()
#   counties.in.state.1 = locations::get.contained.locations(states[[1]], "county") #get the counties in the state (then can compare this to vector above for counties in MSA)
#   counties.in.state.1.and.msa = union(counties.in.this.msa, counties.in.state.1) #use this value as location below to get pop of all relevant counties in msa in state
#   
#   #Determine what proportion of the MSA is in State One
#   state.1.msa.pop = census.manager$pull(outcome='population', location=counties.in.state.1.and.msa, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex'))
#   state.1.proportion = (state.1.msa.pop/msa.population)  #This the proportion value the state contributes to the MSA
#   
#   #Determine what proportion of the MSA is in State Two
#   state.2.msa.pop = census.manager$pull(outcome='population', location=counties.in.each.msa, year = year, keep.dimensions = c('age', 'race', 'ethnicity', 'sex'))
#   state.2.proportion = (state.2.msa.pop/msa.population)  #This the proportion value the state contributes to the MSA
#   
#   #Apply weights to create a final scaled mortality rate
#   final.msa.death.rate = (proportion.state.1 * state.metro.death.rate) + (proportion.state.2 * state.metro.death.rate) + (proportion.state.3 * state.metro.death.rate) #use apply here; create vector of proportion for example
#   
#   
#   #Question: How to have the function return an array of values?
#   
#   # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
#   # then divide the two
#   target.dim.names = specification.metadata$dim.names[c('age','race','sex')]
#   map.value.ontology(deaths, target.dim.names=target.dim.names) / 
#     map.value.ontology(population, target.dim.names=target.dim.names)
#   
# }
