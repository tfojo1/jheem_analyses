#===============================================================================

#This code aggregates county level population data from the Census from
#county to national level using our aggregation function.

#This is for years 2000-2025.

#===============================================================================

#Source aggregation related codes:
source('data_processing/aggregate_county_to_msa_new.R') #This aggregates county level data to other locations (updated 2026)

source('../jheem2/R/HELPERS_array_helpers.R')

#===============================================================================

#Aggregate Total Level from County to National

#===============================================================================

census.manager$register.source('census.aggregated.population', parent.source= "census", full.name = 'Census Aggregated Population', short.name = 'census.agg.pop')

national <- c("US")


#County --> National 
put.msa.data.as.new.source.NEW(outcome = 'test.poulation',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.population',
                               to.locations =  national, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'COUNTRY',
                               details.for.new.data = 'estimated from county data',
                               data.manager = census.manager,
                               required.coverage=0.95,
                               #skip.coverage.condition=T,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')


put.msa.data.as.new.source.NEW(outcome = 'poulation',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.population',
                               to.locations =  national, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'COUNTRY',
                               details.for.new.data = 'estimated from county data',
                               data.manager = census.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')








#===============================================================================


#Updating and using this older code until I can get the function to work:

#This code pulls population data from the census and aggregates the county level data to the national level 
#To be put into the CENSUS manager as national population data

#Data is from 2000-2025

#===============================================================================


# ONE WAY STRATA ----------------------------------------------------------

# Sex alone  ----------------------------------------------------

counties = dimnames(census.mansexr$data$population$estimate$census.population$census$year__location__sex)$location #3133 counties

sex.alone.00.25 = census.mansexr$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex'),
    na.rm = T) 

sex.alone.00.25 = as.data.frame.table(sex.alone.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    select(-Freq, -source)%>%
    mutate(sex = tolower(sex))

# Age alone  ----------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age)$location #3133 counties

age.alone.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'age'),
    na.rm = T) 

age.alone.00.25 = as.data.frame.table(age.alone.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)%>%
    mutate(age = tolower(age))


# Race alone  ----------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__race)$location #3133 counties

race.alone.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'race'),
    na.rm = T) 

race.alone.00.25 = as.data.frame.table(race.alone.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(race = as.character(race))%>%
    select(-Freq, -source)%>%
    mutate(race = tolower(race))

# Ethnicity Alone  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__ethnicity)$location #3133 counties

eth.alone.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'ethnicity'),
    na.rm = T) 

eth.alone.00.25 = as.data.frame.table(eth.alone.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))

# TWO WAY STRATA ----------------------------------------------------------

# Age + Race  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__race)$location #3133 counties

age.race.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'age', 'race'),
    na.rm = T) 

age.race.00.25 = as.data.frame.table(age.race.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(age = as.character(age))%>%
    mutate(race = as.character(race))%>%
    select(-Freq, -source)%>%
    mutate(race = tolower(race))


# Age + Ethnicity -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__ethnicity)$location #3133 counties

age.ethnicity.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'age', 'ethnicity'),
    na.rm = T) 

age.ethnicity.00.25 = as.data.frame.table(age.ethnicity.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(age = as.character(age))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))


# Race + Ethnicity  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__race__ethnicity)$location #3133 counties

race.ethnicity.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'race', 'ethnicity'),
    na.rm = T) 

race.ethnicity.00.25 = as.data.frame.table(race.ethnicity.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(race = as.character(race))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(race = tolower(race))

# Sex + Age  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__sex)$location #3133 counties

sex.age.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex', 'age'),
    na.rm = T) 

sex.age.00.25 = as.data.frame.table(sex.age.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)%>%
    mutate(age = tolower(age))%>%
    mutate(sex = tolower(sex))


# Sex + Race  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__race__sex)$location #3133 counties

sex.race.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex', 'race'),
    na.rm = T) 

sex.race.00.25 = as.data.frame.table(sex.race.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    mutate(race = as.character(race))%>%
    select(-Freq, -source)%>%
    mutate(race = tolower(race))%>%
    mutate(sex = tolower(sex))

# Sex + Ethnicity  -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__ethnicity__sex)$location #3133 counties

sex.ethnicity.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex', 'ethnicity'),
    na.rm = T) 

sex.ethnicity.00.25 = as.data.frame.table(sex.ethnicity.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(sex = tolower(sex))


# THREE WAY STRATA --------------------------------------------------------

# Race + Ethnicity + Sex  --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__race__ethnicity__sex)$location #3133 counties

race.ethnicity.sex.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'race', 'ethnicity', 'sex'),
    na.rm = T) 

race.ethnicity.sex.00.25 = as.data.frame.table(race.ethnicity.sex.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(race = as.character(race))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    mutate(sex = as.character(sex))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(race = tolower(race))

# Race + Ethnicity + Age  ----------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity)$location #3133 counties

race.ethnicity.age.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'race', 'ethnicity', 'age'),
    na.rm = T) 

race.ethnicity.age.00.25 = as.data.frame.table(race.ethnicity.age.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(race = as.character(race))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(race = tolower(race))

# Sex + Ethnicity + Age  ----------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__ethnicity__sex)$location #3133 counties

sex.ethnicity.age.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex', 'ethnicity', 'age'),
    na.rm = T) 

sex.ethnicity.age.00.25 = as.data.frame.table(sex.ethnicity.age.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(sex = tolower(sex))

# Sex + Race + Age  ----------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__race__sex)$location #3133 counties

sex.race.age.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'sex', 'race', 'age'),
    na.rm = T) 

sex.race.age.00.25 = as.data.frame.table(sex.race.age.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(sex = as.character(sex))%>%
    mutate(race = as.character(race))%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)%>%
    mutate(race = tolower(race))%>%
    mutate(sex = tolower(sex))


# FOUR WAY STRATA ---------------------------------------------------------

# Age + Race + Ethnicity + Sex --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex)$location #3133 counties

race.ethnicity.age.sex.00.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population', 
    from.ontology.names = "census", 
    dimension.values = list(location = counties), 
    keep.dimensions = c('year', 'race', 'ethnicity', 'age', 'sex'),
    na.rm = T) 

race.ethnicity.age.sex.00.25 = as.data.frame.table(race.ethnicity.age.sex.00.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(race = as.character(race))%>%
    mutate(ethnicity = as.character(ethnicity))%>%
    mutate(age = as.character(age))%>%
    mutate(sex = as.character(sex))%>%
    select(-Freq, -source)%>%
    mutate(ethnicity = tolower(ethnicity))%>%
    mutate(race = tolower(race))

#PUT: ---------------------------------------------------------------------
#Separate by intercensal and post censal for details of put:

national.population.stratified = list(
    
    eth.alone.00.25,
    race.alone.00.25,
    age.alone.00.25,
    sex.alone.00.25,
    
    sex.ethnicity.00.25,
    sex.race.00.25,
    sex.age.00.25,
    race.ethnicity.00.25,
    age.ethnicity.00.25,
    age.race.00.25,
    
    sex.race.age.00.25,
    sex.ethnicity.age.00.25,
    race.ethnicity.age.00.25,
    race.ethnicity.sex.00.25,
    
    race.ethnicity.age.sex.00.25
)

#INTERCENSAL 2000-2019:
national.population.stratified.intercensal <- lapply(
    national.population.stratified,
    function(df) {
        df %>%
            filter(year >= 2000, year <= 2019)
    }
)

#POSTCENSAL (VINTAGE: 2020-2025)
national.population.stratified.vintage <- lapply(
    national.population.stratified,
    function(df) {
        df %>%
            filter(year >= 2020, year <= 2025)
    }
)


#PUT:
for (data in national.population.stratified.intercensal) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Data- Manually Aggregated from Intercensal Estimates')
}

for (data in national.population.stratified.vintage) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Data- Manually Aggregated from Postcensal estimates (vintage')
}






# SINGE YEAR AGE GROUPS ---------------------------------------------------


 
# Age 2020-2025 -----------------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$single.year.age.census$year__location__age)$location #3134 counties

age.alone.20.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population',
    from.ontology.names = "single.year.age.census",
    dimension.values = list(location = counties),
    keep.dimensions = c('year', 'age'),
    na.rm = T)

age.alone.20.25 = as.data.frame.table(age.alone.20.25)%>% 
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(age = as.character(age))%>%
    select(-Freq, -source)



# Age + Sex  2020-2025 ----------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$single.year.age.census$year__location__age__sex)$location #3134 counties

age.sex.20.25 = census.manager$pull(
    outcome = "population",
    source = 'census.population',
    from.ontology.names = "single.year.age.census",
    dimension.values = list(location = counties),
    keep.dimensions = c('year', 'age', 'sex'),
    na.rm = T)

age.sex.20.25 = as.data.frame.table(age.sex.20.25)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    mutate(age = as.character(age))%>%
    mutate(sex = as.character(sex))%>%
    select(-Freq, -source)


# Put ---------------------------------------------------------------------

national.population.census.single.year.age = list(
    age.alone.20.25,
    age.sex.20.25
)

for (data in national.population.census.single.year.age) {

    census.manager$put.long.form(
        data = data,
        ontology.name = 'single.year.age.census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Data- Manually Aggregated from Vintage Data (postcensal)')
}

