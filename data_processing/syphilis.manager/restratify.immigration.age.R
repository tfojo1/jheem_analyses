surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")

#This restructures the age groups from the immigration data to align with those used in SHIELD

desired.ages.for.immigration.data <- c('0-14 years', '15-19 years',  '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                                       '40-44 years', '45-49 years', '50-54 years', '55-64 years', '65+ years')


# MSA Level Immigration by Age --------------------------------------------

msa.immigration.age= surveillance.manager$data$immigration$estimate$census.population$census.immigration$year__location__age

restratified.msa.immigration.age <- jheem2::restratify.age.counts(msa.immigration.age, desired.age.brackets= desired.ages.for.immigration.data, smooth.infinite.age.to =100, allow.extrapolation = T)


restratified.msa.immigration.age <- as.data.frame.table(restratified.msa.immigration.age)%>%
    mutate(value = round(Freq))%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(age = as.character(age))%>%
    mutate(outcome = "immigration")%>%
    select(-Freq)

# MSA Level Emigration by Age ---------------------------------------------
msa.emigration.age = surveillance.manager$data$emigration$estimate$census.population$census.immigration$year__location__age 

restratified.msa.emigration.age <- jheem2::restratify.age.counts(msa.emigration.age, desired.age.brackets= desired.ages.for.immigration.data, smooth.infinite.age.to =100, allow.extrapolation = T)

restratified.msa.emigration.age <- as.data.frame.table(restratified.msa.emigration.age)%>%
    mutate(value = round(Freq))%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(age = as.character(age))%>%
    mutate(outcome = "emigration")%>%
    select(-Freq)


# National Immigration by Age ---------------------------------------------

national.immigration.age = surveillance.manager$data$immigration$estimate$census.population$census.immigration.national$year__location__age

restratified.national.immigration.age <- jheem2::restratify.age.counts(national.immigration.age, desired.age.brackets= desired.ages.for.immigration.data, smooth.infinite.age.to =100, allow.extrapolation = T)

restratified.national.immigration.age <- as.data.frame.table(restratified.national.immigration.age)%>%
    mutate(value = round(Freq))%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(age = as.character(age))%>%
    mutate(outcome = "immigration")%>%
    select(-Freq)



# Put restratified age data ----------------------------------------------
data.manager$put.long.form(
    data = restratified.msa.immigration.age,
    ontology.name = 'census.immigration',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')

data.manager$put.long.form(
    data = restratified.msa.emigration.age,
    ontology.name = 'census.immigration',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')


data.manager$put.long.form(
    data = restratified.national.immigration.age,
    ontology.name = 'census.immigration.national',
    source = 'census.population',  
    dimension.values = list(),
    url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
    details = 'Geographic Mobility by Selected Characteristics in the United States')

