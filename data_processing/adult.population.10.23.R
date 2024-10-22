#This creates adult.population for 2010-2023 stratified population data from the Census

#Estimate the adult.population from age grouped data
#Then find adult.population from the single year ages


# ESTIMATED DATA ----------------------------------------------------------

desired.ages.for.census <- c('0-4 years', '5-12 years', '13-17 years', '18-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                             '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
                             '75-79 years', '80-84 years', '85+ years')

#Update for 7-23-24: Creating a racial mapping to align this race data with the census ontology.
race.mappings.to.census = c('White' = 'white',
                            'Black' = 'black',
                            'American Indian and Alaska Native' = 'american indian or alaska native',
                            'Native Hawaiian and Other Pacific Islander' = 'asian or pacific islander',
                            'Asian' = 'asian or pacific islander') 

#ESTIMATED DATA: adult.population 2010-2023 by RACE
population.by.race.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race

restratify.age.race <- jheem2::restratify.age.counts(population.by.race.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.race.10.23 = restratify.age.race[ , ,3:18,] #subset by only adult age groups
adult.pop.race.10.23 = apply(restratify.adult.pop.race.10.23, MARGIN = c("year","location", "race"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.race.10.23 <- as.data.frame.table(adult.pop.race.10.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
adult.pop.race.10.23$race = race.mappings.to.census[adult.pop.race.10.23$race]
adult.pop.race.10.23<- adult.pop.race.10.23 %>%
  group_by(year, location, race)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)
adult.pop.race.10.23<- as.data.frame(adult.pop.race.10.23[!duplicated(adult.pop.race.10.23), ])


#ESTIMATED DATA: adult.population 2010-2023 by ETHNICITY
population.by.ethnicity.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity

restratify.age.ethnicity <- jheem2::restratify.age.counts(population.by.ethnicity.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.ethnicity.10.23 = restratify.age.ethnicity[ , , 3:18,] #subset by only adult age groups
adult.pop.ethnicity.10.23 = apply(restratify.adult.pop.ethnicity.10.23, MARGIN = c("year","location", "ethnicity"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.ethnicity.10.23 <- as.data.frame.table(adult.pop.ethnicity.10.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(ethnicity = as.character(tolower(ethnicity)))%>%
  select(-Freq)

#ESTIMATED DATA: adult.population 2010-2023 by RACE+ETHNICITY

array.race.eth = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity

restratify.race.eth <- jheem2::restratify.age.counts(array.race.eth, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restrat.race.eth = restratify.race.eth[ , ,3:18, ,] #subset by only adult age groups
fixed.race.eth = apply(restrat.race.eth, MARGIN = c("year","location", "race", 'ethnicity'), sum) #sum the adult age groups to get adult.population for 2020-2023


fixed.race.eth <- as.data.frame.table(fixed.race.eth)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
fixed.race.eth$race = race.mappings.to.census[fixed.race.eth$race]
fixed.race.eth<- fixed.race.eth %>%
  mutate(ethnicity = tolower(ethnicity))%>%
  group_by(year, location, race, ethnicity)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)

fixed.race.eth<- as.data.frame(fixed.race.eth[!duplicated(fixed.race.eth), ])

#ESTIMATED DATA: adult.population 2010-2023 by RACE+ETHNICITY+AGE
array.race.eth.age = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity

restratify.race.eth.age <- jheem2::restratify.age.counts(array.race.eth.age, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restrat.race.eth.age = restratify.race.eth.age[ , ,3:18, ,] #subset by only adult age groups
fixed.race.eth.age = apply(restrat.race.eth.age, MARGIN = c("year","location", "race", 'ethnicity', 'age'), sum) #sum the adult age groups to get adult.population for 2020-2023


fixed.race.eth.age <- as.data.frame.table(fixed.race.eth.age)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  mutate(age = as.character(age))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
fixed.race.eth.age$race = race.mappings.to.census[fixed.race.eth.age$race]

fixed.race.eth.age<- fixed.race.eth.age %>%
  mutate(ethnicity = tolower(ethnicity))%>%
  group_by(year, location, race, ethnicity, age)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)

fixed.race.eth.age<- as.data.frame(fixed.race.eth.age[!duplicated(fixed.race.eth.age), ])


# Race+Ethnicity+Sex 2010-2023--------------------------------------------------
array.race.eth.sex = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex

restratify.race.eth.sex <- jheem2::restratify.age.counts(array.race.eth.sex, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restrat.race.eth.sex = restratify.race.eth.sex[ , ,3:18, , ,] #subset by only adult age groups
fixed.race.eth.sex = apply(restrat.race.eth.sex, MARGIN = c("year","location", "race", 'ethnicity', 'sex'), sum) #sum the adult age groups to get adult.population for 2020-2023


fixed.race.eth.sex <- as.data.frame.table(fixed.race.eth.sex)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  mutate(sex = as.character(sex))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(ethnicity = tolower(ethnicity))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
fixed.race.eth.sex$race = race.mappings.to.census[fixed.race.eth.sex$race]

fixed.race.eth.sex<- fixed.race.eth.sex %>%
  group_by(year, location, race, ethnicity, sex)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)


fixed.race.eth.sex<- as.data.frame(fixed.race.eth.sex[!duplicated(fixed.race.eth.sex), ])

##################################################################################

# PUT for all stratified, estimated data ----------------------------------

estimated.adult.pop.stratified.put = list(
  #adult.pop.total.20.23,
  #adult.pop.sex.20.23,
  #adult.pop.age.20.23,
  adult.pop.race.10.23, #race only
  adult.pop.ethnicity.10.23, #eth only
  fixed.race.eth, #race+eth
  fixed.race.eth.age, #this is race +eth+age
  fixed.race.eth.sex) #this is race+eth+sex 


for (data in estimated.adult.pop.stratified.put) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census.grouped.age', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
##################################################################################

# SINGLE YEAR AGE GROUP DATA (This is for 2020-2023 where single year is available)----------------------------------------------

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

#Age + Sex
single.year.age.sex <- single.year.age.sex%>%
  rename(value = Freq)%>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(outcome = 'adult.population')%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(age = as.character(age))


##
single.year.total= as.data.frame(single.year.total)
single.year.age= as.data.frame(single.year.age)
single.year.sex= as.data.frame(single.year.sex)
single.year.age.sex = as.data.frame(single.year.age.sex)

#Put Single Year Age Group Data
adult.pop.by.single.year.age = list(
  single.year.total, #by total
  single.year.age, #by age
  single.year.sex, #by sex
  single.year.age.sex #by age+sex
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
###############################################################################

# Age Stratified Data by Group for 2010-2019 (where only grouped data is available) --------

#Total 2010-2019
total.array.10.19 = census.manager$data$population$estimate$census.population$stratified.census$year__location__age

total.10.19 <- jheem2::restratify.age.counts(total.array.10.19, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

fixed.total.10.19 = total.10.19[ , ,3:18] #subset by only adult age groups
fixed.total.10.19 = apply(fixed.total.10.19, MARGIN = c("year","location"), sum) 

fixed.total.10.19 <- as.data.frame.table(fixed.total.10.19)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  select(-Freq)

#Sex 2010-2019
sex.array.10.19 = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex

sex.10.19 <- jheem2::restratify.age.counts(sex.array.10.19, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

fixed.sex.10.19 = sex.10.19[ , ,3:18, ] #subset by only adult age groups
fixed.sex.10.19 = apply(fixed.sex.10.19, MARGIN = c("year","location", "sex"), sum) 

fixed.sex.10.19 <- as.data.frame.table(fixed.sex.10.19)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq)

#Age 2010-2019
age.array.10.19 = census.manager$data$population$estimate$census.population$stratified.census$year__location__age

age.10.19 <- jheem2::restratify.age.counts(age.array.10.19, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

fixed.age.10.19 = age.10.19[ , ,3:18] #subset by only adult age groups
fixed.age.10.19 = apply(fixed.age.10.19, MARGIN = c("year","location", "age"), sum) 

fixed.age.10.19 <- as.data.frame.table(fixed.age.10.19)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

#Age+Sex 2010-2019
age.sex.array.10.19 = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex

age.sex.10.19 <- jheem2::restratify.age.counts(age.sex.array.10.19, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

fixed.age.sex.10.19 = age.sex.10.19[ , ,3:18, ] #subset by only adult age groups
fixed.age.sex.10.19 = apply(fixed.age.sex.10.19, MARGIN = c("year","location", "sex", 'age'), sum) 

fixed.age.sex.10.19 <- as.data.frame.table(fixed.age.sex.10.19)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(sex = as.character(sex))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)


# Put 2010-2019 Estimated Adult Pop for Total, sex, age, sex+age ----------

est.adult.pop.10.19 = list(
  fixed.total.10.19,
  fixed.sex.10.19,
  fixed.age.10.19,
  fixed.age.sex.10.19) 


for (data in est.adult.pop.10.19) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census.grouped.age', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}