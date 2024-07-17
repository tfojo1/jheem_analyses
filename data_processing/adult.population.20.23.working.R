#This is stratified data, county level, census data, 2020-2023

#Estimate the adult.population from age grouped data
#Then find adult.population from the single year ages

census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

#///NOTE YOU NEED TO SOURCE THE START OF THE RESTRUCTURE RECENT AGE GROUPS CODE//////#


# ESTIMATED DATA ----------------------------------------------------------
#ESTIMATED DATA: adult.population 2020-2023 by TOTAL
desired.ages.for.census <- c('0-4 years', '5-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                             '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
                             '75-79 years', '80-84 years', '85+ years')

population.array.from.census = census.manager$data$population$estimate$census.population$stratified.census$year__location__age

restratify.age.from.census <- restratify.age.counts(population.array.from.census, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

adult.age.groups.census.20.23 = restratify.age.from.census[ , , 3:17] #subset by only adult age groups
adult.population.census.20.23 = apply(adult.age.groups.census.20.23, MARGIN = c("year","location"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.total.20.23 <- as.data.frame.table(adult.population.census.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  select(-Freq)

#ESTIMATED DATA: adult.population 2020-2023 by SEX

population.by.sex.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex

restratify.age.sex <- restratify.age.counts(population.by.sex.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.sex.20.23 = restratify.age.sex[ , , 3:17,] #subset by only adult age groups
adult.pop.sex.20.23 = apply(restratify.adult.pop.sex.20.23, MARGIN = c("year","location", "sex"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.sex.20.23 <- as.data.frame.table(adult.pop.sex.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq)

#ESTIMATED DATA: adult.population 2020-2023 by AGE
population.by.age.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age

restratify.age.age <- restratify.age.counts(population.by.age.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.age.20.23 = restratify.age.age[ , , 3:17] #subset by only adult age groups
adult.pop.age.20.23 = apply(restratify.adult.pop.age.20.23, MARGIN = c("year","location", "age"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.age.20.23 <- as.data.frame.table(adult.pop.age.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(age = as.character(age))%>%
  select(-Freq)


#ESTIMATED DATA: adult.population 2020-2023 by RACE
population.by.race.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race

restratify.age.race <- restratify.age.counts(population.by.race.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.race.20.23 = restratify.age.race[ , , 3:17,] #subset by only adult age groups
adult.pop.race.20.23 = apply(restratify.adult.pop.race.20.23, MARGIN = c("year","location", "race"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.race.20.23 <- as.data.frame.table(adult.pop.race.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  select(-Freq)

#ESTIMATED DATA: adult.population 2020-2023 by ETHNICITY
population.by.ethnicity.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity

restratify.age.ethnicity <- restratify.age.counts(population.by.ethnicity.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.ethnicity.20.23 = restratify.age.ethnicity[ , , 3:17,] #subset by only adult age groups
adult.pop.ethnicity.20.23 = apply(restratify.adult.pop.ethnicity.20.23, MARGIN = c("year","location", "ethnicity"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.ethnicity.20.23 <- as.data.frame.table(adult.pop.ethnicity.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq)

##################################################################################

# PUT for all stratified, estimated data ----------------------------------

estimated.adult.pop.stratified.put = list(
  adult.pop.total.20.23,
  adult.pop.sex.20.23,
  adult.pop.age.20.23,
  adult.pop.race.20.23,
  adult.pop.ethnicity.20.23)


for (data in estimated.adult.pop.stratified.put) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census.estimated.adult.population', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
##################################################################################


# SINGLE YEAR AGE GROUP DATA ----------------------------------------------

#adult.pop by SINGLE YEAR AGE (for TOTAL, SEX, and AGE)
single.year.age = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age)
single.year.age.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age__sex)

#age
single.year.age <- single.year.age %>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(age = as.character(age))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(Freq))%>%
  select(-Freq)%>%
  mutate(outcome = "adult.population")

single.year.age<- single.year.age[!duplicated(single.year.age), ]


#total
single.year.total <- single.year.age%>%
  group_by(year, location)%>%
  mutate(value.fixed = sum(value))%>%
  select(-age, -value)%>%
  rename(value = value.fixed)

single.year.total<- single.year.total[!duplicated(single.year.total), ]

#sex
single.year.sex <- single.year.age.sex%>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(age = as.character(age))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(value = as.numeric(Freq))%>%
  select(-Freq)%>%
  mutate(outcome = "adult.population")%>%
  group_by(year, location, sex)%>%
  mutate(new.value = sum(value))%>%
  select(outcome, year, location, sex, new.value)%>%
  rename(value = new.value)

single.year.sex<- single.year.sex[!duplicated(single.year.sex), ]

##
single.year.total= as.data.frame(single.year.total)
single.year.age= as.data.frame(single.year.age)
single.year.sex= as.data.frame(single.year.sex)

#Put Single Year Age Group Data
adult.pop.by.single.year.age = list(
  single.year.total, #by total
  single.year.age, #by age
  single.year.sex #by sex
)

for (data in adult.pop.by.single.year.age) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}