#Sourcing this at the end of the HIV surveillance manager code because in order to aggregate emory.proportion.msm we need the denominator value
#adult.population which is found in a separate section.

# Aggregate proportion.msm to state ---------------------------------------

# source('../jheem2/R/HELPERS_array_helpers.R')
 source('commoncode/locations_of_interest.R')

state.vector = state.abb
state.vector = state.vector[!state.vector == "CT"] #CT is creating an issue - so until you sort that just remove it and see if this works.  #The Emory data has the CT codes from before 2020

for(state in state.vector){

    counties.in.the.state = locations::get.contained.locations(state, "COUNTY")
    #print(state)
    counties.we.have.data.for = counties.in.the.state[counties.in.the.state %in% dimnames(surveillance.manager$data$emory.proportion.msm$estimate$emory$emory$year__location__sex)$location] #only pull what we have data for

    aggregated.data <- surveillance.manager$pull('emory.proportion.msm',
                                                 sources='emory',
                                                 keep.dimensions = c('year', 'sex'), #removing location so we can aggregate
                                                 dimension.values=list(location=counties.we.have.data.for))

    aggregated.data <- apply(aggregated.data, c('year', 'sex'), function(x) {x}) #this is because source is kept as a dimension so we need to remove that

    surveillance.manager$put(aggregated.data,
                             outcome = 'emory.proportion.msm',
                             source = 'emory.aggregated',
                             ontology.name = 'emory',
                             dimension.values = list(location=state), #one put per state so use state not the state vector
                             url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
                             details='Emory University MSM Research from American Community Survey')

}

# Aggregate proportion.msm to MSA ---------------------------------------

for(msa in MSAS.OF.INTEREST){

    counties.in.the.msa = locations::get.contained.locations(msa, "COUNTY")
    #print(msa)
    counties.we.have.data.for = counties.in.the.msa[counties.in.the.msa %in% dimnames(surveillance.manager$data$emory.proportion.msm$estimate$emory$emory$year__location__sex)$location] #only pull what we have data for

    aggregated.data <- surveillance.manager$pull('emory.proportion.msm',
                                                 sources='emory',
                                                 keep.dimensions = c('year', 'sex'),
                                                 dimension.values=list(location=counties.we.have.data.for))

    aggregated.data <- apply(aggregated.data, c('year', 'sex'), function(x) {x})

    surveillance.manager$put(aggregated.data,
                             outcome = 'emory.proportion.msm',
                             source = 'emory.aggregated',
                             ontology.name = 'emory',
                             dimension.values = list(location=msa),
                             url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
                             details='Emory University MSM Research from American Community Survey')
}