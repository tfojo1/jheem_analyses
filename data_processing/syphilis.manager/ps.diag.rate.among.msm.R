#This code is used to calculate the outcome 'ps.diag.rate.among.msm'
#Which is calculated as: ps.diag.rate.among.msm = MSM diagnoses ÷ MSM population size

#To get the msm population size, we used Emory proportion MSM data by county to estimate MSM by MSA and state

#This will need to happen within the 'syphilis.manager.merge' code since MSM and syphilis data come from different sections of the manager

#I'm going to calculate the count by county and then aggregate that to state and MSA#
#==================================================================
#Apply Emory's Proportion MSM Estimate to MSA/State Data

#==================================================================

#there is proportion.msm data for more locations but i'm filtering for only county:
proportion.msm.county <- as.data.frame.table(syphilis.manager$data$proportion.msm$estimate$emory$emory$year__location__sex)%>% filter(sex == 'male')%>% rename(proprtion.msm = Freq)%>% select(-year)%>% filter(str_detect(location, "^\\d{5}$"))

#2010-2019
male.population.county.1 <- as.data.frame.table(syphilis.manager$data$population$estimate$census.population$stratified.census$year__location__sex) %>% filter(sex == 'male')%>% rename(male.population.count = Freq)

#2020-2023
male.population.county.2 <- as.data.frame.table(syphilis.manager$data$population$estimate$census.population$census$year__location__sex) %>% filter(sex == 'male')%>% rename(male.population.count = Freq)

#Combine separate years of population data into one data frame:
male.population.data <- rbind(male.population.county.1, male.population.county.2)%>% filter(str_detect(location, "^\\d{5}$"))

#Combine population data with MSM proportion data:
all.male.data <- inner_join(male.population.data, proportion.msm.county, by=c( "location", "sex"))
#NOTE: there are a few instances where there is either population or MSM data missing.  The inner join removes this but in the full join you can see it.

#Calculate the Count of MSM:
all.male.data <- all.male.data%>%
    mutate(estimated.count.msm = round(male.population.count * proprtion.msm))%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(sex = as.character(sex))%>%
    rename(value = estimated.count.msm)%>%
    mutate(outcome = "estimated.count.msm")%>%
    select(year, location, outcome, sex, value)

#Register the outcome and Source:
syphilis.manager$register.outcome(
    'estimated.count.msm',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Estimated Count of MSM',
        axis.name = 'Estimated Count of MSM',
        units = 'count',
        description = "Estimated Count of MSM"))

syphilis.manager$register.source('emory.aggregated', parent.source= "ACS", full.name = "Aggregated Data from Emory Proportion Estimates", short.name='emory') #child


#Put count of MSM:
    syphilis.manager$put.long.form(
        data = all.male.data,
        ontology.name = 'emory',
        source = 'emory',
        dimension.values = list(sex = "male"),
        url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
        details = 'Estimated Count of MSM Using Emorys Proportion of MSM data from 2013')

#Then Aggregate:
    #MSA
put.msa.data.as.new.source.NEW(outcome = 'estimated.count.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               to.locations =  ALL.MSAS,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')
    #State
STATE <- state.abb
put.msa.data.as.new.source.NEW(outcome = 'estimated.count.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               to.locations =  STATE,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'STATE',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')