#This code applies estimates of racial groups from CDC Wonder to the existing racial categories from Census'
#State to State migration data.

#Estimates are from CDC Wonder's 2020-2022 Single Race population estimates
#I pulled national 2022 data

#This needs to apply to immigration race and adult.immigration race (BUT be mindful of if adult pop or race is calculated first)!!!


# Establish Proportions from Wonder ---------------------------------------

#BLACK
#Proportion of Black who are Hispanic:  3329272/45399743 = 0.073
#Proportion of Hispanic who are Black:  3329272/63664346 = 0.052

prop.black.hisp <- 0.073
prop.hisp.black <- 0.052

#AMERICAN INDIAN
#Proportion of AI who are Hispanic: 0.448
#Proportion of Hispanic who are AI: 0.031

#Decided to assume that all American Indian and Alaska Native are NH (because applying the calculation resulted in negative values)

# prop.american.indian.hisp <- 0.448
# prop.hisp.american.indian <- 0.031

#ASIAN
#Proportion of Asian who are Hispanic:  0.032 
#Proportion of Hispanic who are Asian: 0.011

prop.asian.hisp <- 0.032
prop.hisp.asian <- 0.011

#NATIVE HAWAIIAN
#Proportion of NH who are Hispanic:  0.276
#Proportion of Hispanic who are NH: 0.004
    
prop.native.hawaiian.hisp <- 0.276
prop.hisp.native.hawaiian <- 0.004
    

#OTHER 
#Proportion of Other who are Hispanic:  0.220
#Proportion of Hispanic who are Other: 0.870
    
prop.other.hisp <- 0.220
prop.hisp.other <- 0.870
    


# IMMIGRATION BY RACE -----------------------------------------------------

#Take the immigration stratified by race list and turn into a df:
state.immigration.race = lapply(state.immigration.race, `[[`, 2)
state.immigration.race<- bind_rows(state.immigration.race, .id = "column_label")

#Then reconfigure to make the future calculation easier:
race.df <- state.immigration.race%>%
    pivot_wider(names_from = 'race',
                values_from = 'value')%>%
    rename('hispanic' = 'hispanic or latino origin (of any race)')%>%
    rename('black' = 'black or african american')%>%
    rename('white NH' =  'white alone, not hispanic or latino')%>%
    rename(american.indian.nh = `american indian and alaska native`)

#You're going to need a total immigration value by state for the calculation; pull that from the data manager:
#total.imm.df = as.data.frame.table(surveillance.manager$data$immigration$estimate$census.population$census$year__location)%>% rename(total.immigration.value = Freq)

#Join the total immigration value with the data by race:
#state.immigration.combo <- merge(race.df, total.imm.df, by=c("location", "year"))

#Now apply the race calculation formulas:
#Decided to assume that all American Indian and Alaska Native are NH (because applying the calculation resulted in negative values)
state.immigration.combo.reconfigured <- race.df%>%
    mutate(black.nh = round(`black`-(sqrt(prop.black.hisp*prop.hisp.black*`hispanic`*`black`))))%>%
    #mutate(american.indian.nh = round(`american indian and alaska native`-(sqrt(prop.american.indian.hisp*prop.hisp.american.indian*`hispanic`*`american indian and alaska native`))))%>%
    mutate(asian.nh = round(`asian`-(sqrt(prop.asian.hisp*prop.hisp.asian*`hispanic`*`asian`))))%>%
    mutate(native.hawaiian.nh = round(`native hawaiian and other pacific islander`-(sqrt(prop.native.hawaiian.hisp*prop.hisp.native.hawaiian*`hispanic`*`native hawaiian and other pacific islander`))))%>%
    mutate(other.nh = round(`some other race`-(sqrt(prop.black.hisp*prop.hisp.black*`hispanic`*`some other race`))))
    
reconfigured.race.put <- state.immigration.combo.reconfigured%>%
    select(year, location, outcome, hispanic, `white NH`, black.nh, american.indian.nh, native.hawaiian.nh, other.nh, asian.nh)%>%
    pivot_longer(cols = c("hispanic", `white NH`, "black.nh", "american.indian.nh", "native.hawaiian.nh", "other.nh", "asian.nh"),
                 names_to = "race",
                 values_to = "value")%>%
mutate(race.new = case_when(race == "white NH"~ "white non hispanic",
       race == "asian.nh"~ "asian non hispanic",
       race == "hispanic"~ "hispanic",
       race == "american.indian.nh" ~"american indian and alaska native non hispanic",
       race == "native.hawaiian.nh"~"native hawaiian and pacific islander non hispanic",
       race == "other.nh"~"other race non hispanic",
       race == "black.nh"~ "black non hispanic"))%>%
    select(-race)%>%
    rename(race = race.new)%>%
    mutate(value = as.numeric(value))

reconfigured.race.put = as.data.frame(reconfigured.race.put)

#put
data.manager$put.long.form(
    data = reconfigured.race.put,
    ontology.name = 'census.immigration.state.to.state', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')



# ADULT.IMMIGRATION BY RACE -----------------------------------------------
#adult.immgration.by.race 
#This is a data frme created in the adult.state.to.state.migration code

adult.immigration.by.race <- as.data.frame(adult.immgration.by.race)

#Then reconfigure to make the future calculation easier:
adult.race.df <- adult.immigration.by.race%>%
    pivot_wider(names_from = 'race',
                values_from = 'value')%>%
    rename('hispanic' = 'hispanic or latino origin (of any race)')%>%
    rename('black' = 'black or african american')%>%
    rename('white NH' =  'white alone, not hispanic or latino')%>%
    rename(american.indian.nh = `american indian and alaska native`)


#Now apply the race calculation formulas:
#Decided to assume that all American Indian and Alaska Native are NH (because applying the calculation resulted in negative values)
adult.state.immigration.combo.reconfigured <- adult.race.df%>%
    mutate(black.nh = round(`black`-(sqrt(prop.black.hisp*prop.hisp.black*`hispanic`*`black`))))%>%
    #mutate(american.indian.nh = round(`american indian and alaska native`-(sqrt(prop.american.indian.hisp*prop.hisp.american.indian*`hispanic`*`american indian and alaska native`))))%>%
    mutate(asian.nh = round(`asian`-(sqrt(prop.asian.hisp*prop.hisp.asian*`hispanic`*`asian`))))%>%
    mutate(native.hawaiian.nh = round(`native hawaiian and other pacific islander`-(sqrt(prop.native.hawaiian.hisp*prop.hisp.native.hawaiian*`hispanic`*`native hawaiian and other pacific islander`))))%>%
    mutate(other.nh = round(`some other race`-(sqrt(prop.black.hisp*prop.hisp.black*`hispanic`*`some other race`))))

adult.reconfigured.race.put <- adult.state.immigration.combo.reconfigured%>%
    select(year, location, outcome, hispanic, `white NH`, black.nh, american.indian.nh, native.hawaiian.nh, other.nh, asian.nh)%>%
    pivot_longer(cols = c("hispanic", `white NH`, "black.nh", "american.indian.nh", "native.hawaiian.nh", "other.nh", "asian.nh"),
                 names_to = "race",
                 values_to = "value")%>%
    mutate(race.new = case_when(race == "white NH"~ "white non hispanic",
                                race == "asian.nh"~ "asian non hispanic",
                                race == "hispanic"~ "hispanic",
                                race == "american.indian.nh" ~"american indian and alaska native non hispanic",
                                race == "native.hawaiian.nh"~"native hawaiian and pacific islander non hispanic",
                                race == "other.nh"~"other race non hispanic",
                                race == "black.nh"~ "black non hispanic"))%>%
    select(-race)%>%
    rename(race = race.new)%>%
    mutate(value = as.numeric(value))

adult.reconfigured.race.put = as.data.frame(adult.reconfigured.race.put)

data.manager$put.long.form(
    data = adult.reconfigured.race.put,
    ontology.name = 'census.immigration.state.to.state.adults', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')

