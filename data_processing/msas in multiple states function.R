source('commoncode/locations_of_interest.R')

get.msa.mortality.rates <- function(location, #currently represents 1 msa
                                    year= "2011-2015",
                                    census.manager = CENSUS.MANAGER)
  
{  browser()
  #How to determine the locations- this isn't working
  states = locations::get.containing.locations(location, "state")
  counties = locations::get.contained.locations(location, 'county')
  
  state.metro.deaths = census.manager$pull(outcome = 'metro.deaths', location = states, year= year, keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
  
  if (is.null(deaths))
    stop("Error in get.location.mortality.rates() - unable to pull  deaths data for the requested years")
  
  state.metro.pop = census.manager$pull(outcome = 'metro.deaths.denominator', location = states, year= year, keep.dimensions = c('location', 'age', 'race', 'ethnicity', 'sex'))
  browser()
  if (is.null(population))
    stop("Error in get.location.mortality.rates() - unable to pull any population data for the requested years")
  

  # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
  # then divide the two
  state.metro.death.rate = state.metro.deaths/state.metro.pop
  maryland.counties = "msa.counties.md"
  maryland.county.population = census.manager$pull(outcome = 'population', location = maryland.counties, year = year, keep.dimensions = c('age')) #this would add up all populations from md counties#
  virgina.county.population = "population"
  msa.population = "msa.population"
  fraction.md = (maryland.county.population/msa.population) #this is the 60%; 40%
  fraction.va = (virgina.county.population/msa.population)
  state.metro.death.rate.md = "rate"
  state.metro.death.rate.va = "other.rate"
  final.death.rate.for.msa = (fraction.va*state.metro.death.rate.va) + (fraction.md*state.metro.death.rate.md)
  #state.metro.death.rate= ((msa.death.rate * msa.population) + (state.death.rate * state.population))
  
  #Do a put statement here- put an msa death rate into the manager

    
}
