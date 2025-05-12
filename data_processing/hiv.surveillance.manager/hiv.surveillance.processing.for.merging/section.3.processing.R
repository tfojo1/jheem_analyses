
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 3#####
#Proportion.tested; proportion.msm (mainly data from BRFSS)

###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

#Register outcomes:
data.manager$register.outcome(
  'adult.population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Population',
    axis.name = 'Adult Population',
    units = 'population',
    description = "Adult Population Estimate, Ages 13 and over"))

data.manager$register.outcome(
  'proportion.tested.n',           #Will have option in code to make this only people at risk or everyone#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Denominator value for proportion tested in past year',
    axis.name = 'Denominator value for proportion tested in past year',
    units = '%',
    description = "Denominator value for proportion tested in past year"))

data.manager$register.outcome(
  'proportion.tested', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion Tested in Past Year',
    axis.name = 'Proportion Tested in Past Year',
    units = '%',
    description = "Proportion of People who have received an HIV test in the last year"), denominator.outcome = 'proportion.tested.n')

data.manager$register.outcome(
    'proportion.msm.n',           
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Denominator value for proportion msm',
        axis.name = 'Denominator value for proportion msm',
        units = '%',
        description = "Denominator value for proportion msm"))

data.manager$register.outcome(
  'proportion.msm', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion of MSM',
    axis.name = 'Proportion of MSM',
    units = '%',
    description = "Proportion of Men who have sex with Men"), denominator.outcome = 'proportion.msm.n')

data.manager$register.outcome(
  'unweighted.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'unweighted.denominator',
    axis.name = 'unweighted.denominator)',
    units = 'cases',
    description = "BRFSS Unweighted Denominator Value"))

#Register Sources:
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS") #parent
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS") #parent

data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss') #child
data.manager$register.source('emory', parent.source= "ACS", full.name = "Emory University", short.name='emory') #child
surveillance.manager$register.source('emory.aggregated', parent.source= "ACS", full.name = "Emory University- Aggregated", short.name='emory.aggregated') 

#Register Ontologies:
data.manager$register.ontology(
  'brfss',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
    race=c('white', 'black', 'american indian/alaska native', 'asian', 'native hawaiian/other pacific islander', 'hispanic', 'other race'),
    sex=c('male','female'),
    risk=c('msm', 'not_msm')
  ))

data.manager$register.ontology(
  'emory',
  ont = ontology(
    year= NULL,
    location= NULL,
    sex=c('male', 'female') #needs to have male and female here to represent all possible outcomes
  ))

#Codes:
source('data_processing/hiv.surveillance.manager/brfss_state_weighted.R') #Source BRFSS testing data
source('data_processing/hiv.surveillance.manager/brfss_msa_weighted.R') #Source BRFSS testing data
source('data_processing/hiv.surveillance.manager/msm.R') #Source msm
source('data_processing/hiv.surveillance.manager/brfss_unweighted_denominators.R') #Source BRFSS Unweighted Denominator values

surveillance.manager = data.manager

# Aggregate proportion.msm to state ---------------------------------------

source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations
source('../jheem2/R/HELPERS_array_helpers.R')
source('commoncode/locations_of_interest.R')

state.vector = state.abb 
state.vector = state.vector[!state.vector == "CT"] #CT is creating an issue - so until you sort that just remove it and see if this works.  #The Emory data has the CT codes from before 2020

for(state in state.vector){
    
    counties.in.the.state = locations::get.contained.locations(state, "COUNTY")
    #print(state)
    counties.we.have.data.for = counties.in.the.state[counties.in.the.state %in% dimnames(surveillance.manager$data$proportion.msm$estimate$emory$emory$year__location__sex)$location] #only pull what we have data for
    
    aggregated.data <- surveillance.manager$pull('proportion.msm',
                                                 sources='emory',
                                                 keep.dimensions = c('year', 'sex'), #removing location so we can aggregate
                                                 dimension.values=list(location=counties.we.have.data.for)) 
    
    aggregated.data <- apply(aggregated.data, c('year', 'sex'), function(x) {x}) #this is because source is kept as a dimension so we need to remove that
    
    surveillance.manager$put(aggregated.data,
                             outcome = 'proportion.msm',
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
    counties.we.have.data.for = counties.in.the.msa[counties.in.the.msa %in% dimnames(surveillance.manager$data$proportion.msm$estimate$emory$emory$year__location__sex)$location] #only pull what we have data for
    
    aggregated.data <- surveillance.manager$pull('proportion.msm',
                                                 sources='emory',
                                                 keep.dimensions = c('year', 'sex'),
                                                 dimension.values=list(location=counties.we.have.data.for)) 
    
    aggregated.data <- apply(aggregated.data, c('year', 'sex'), function(x) {x}) 
    
    surveillance.manager$put(aggregated.data,
                             outcome = 'proportion.msm',
                             source = 'emory.aggregated',
                             ontology.name = 'emory',
                             dimension.values = list(location=msa), 
                             url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
                             details='Emory University MSM Research from American Community Survey')
    

#Save:
save(surveillance.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section3.rdata")