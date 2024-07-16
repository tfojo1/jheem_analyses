#This code is to add the newer (6-27-24) stratified population data by county from the census to the census manger
#This will replace the stratified census data and be used for adult.population in the surveillance manager

#For race I'm going to use: white alone, black or african american alone, 
#american indian or alaska native alone, asian alone, 
#native hawaiian or pacific islander alone

#For ethnicity: total NH male + NH female (NH_MALE, NH_FEMALE); 
#then Hispanic male + Hispanic female (H-MALE, H_FEMALE)

DATA.DIR.POP.20.23 = "../../data_raw/new_census_data_june_2024"

pop.20.23 <- Sys.glob(paste0(DATA.DIR.POP.20.23, '/*.csv'))

#creating a list with sublists of filename, data#
pop.20.23.data <- lapply(pop.20.23, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

county_agegr_sex <- pop.20.23.data[[1]][[2]]
county_agegr_sex_race_eth <- pop.20.23.data[[2]][[2]]

# Mappings ----------------------------------------------------------------

year.mappings = c('1' = 'remove',
                  '2' = "2020",
                  '3'= "2021",
                  '4'= "2022",
                  '5'= "2023")
census.race.mappings = c('WA' = 'White',
                           'BA' = 'Black',
                           'IA'= 'American Indian and Alaska Native',
                           'AA' = 'Asian',
                           'NA' = 'Native Hawaiian and Other Pacific Islander')
census.eth.mappings = c(  'H' = 'Hispanic',
                           'NH' = 'Not Hispanic')
census.age.mappings = c('1' = '0-4 years', 
                          '2' = '5-9 years', 
                          '3' = '10-14 years', 
                          '4' = '15-19 years', 
                          '5' = '20-24 years', 
                          '6' = '25-29 years', 
                          '7' =  '30-34 years', 
                          '8' = '35-39 years',
                          '9' = '40-44 years', 
                          '10' = '45-49 years', 
                          '11' = '50-54 years', 
                          '12' = '55-59 years', 
                          '13'= '60-64 years', 
                          '14' = '65-69 years', 
                          '15' = '70-74 years', 
                          '16' = '75-79 years', 
                          '17' = '80-84 years', 
                          '18' = '85+ years')
census.age.mappings.two = c('UNDER5' = '0-4 years', 
                        'AGE59' = '5-9 years', 
                        'AGE1014' = '10-14 years', 
                        'AGE1519' = '15-19 years', 
                        'AGE2024' = '20-24 years', 
                        'AGE2529' = '25-29 years', 
                        'AGE3034' =  '30-34 years', 
                        'AGE3539' = '35-39 years',
                        'AGE4044' = '40-44 years', 
                        'AGE4549' = '45-49 years', 
                        'AGE5054' = '50-54 years', 
                        'AGE5559' = '55-59 years', 
                        'AGE6064'= '60-64 years', 
                        'AGE6569' = '65-69 years', 
                        'AGE7074' = '70-74 years', 
                        'AGE7579' = '75-79 years', 
                        'AGE8084' = '80-84 years', 
                        'AGE85PLUS' = '85+ years')

# TOTAL -------------------------------------------------------------------

population.total <- county_agegr_sex %>%
  select(STATE, COUNTY, YEAR, POPESTIMATE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  rename(value = POPESTIMATE)%>%
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  mutate(outcome = "population")%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  select(outcome, year, location, value)

population.total= as.data.frame(population.total)

# SEX ---------------------------------------------------------------------

poulation.sex <- county_agegr_sex %>%
  select(STATE, COUNTY, YEAR, POPEST_MALE, POPEST_FEM)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c("POPEST_MALE", "POPEST_FEM"),
               names_to = c("outcome", "sex"),
               names_sep = "_",
               values_to = "value")%>%
  mutate(outcome = "population")%>%
  mutate(sex = ifelse( sex == "MALE", 'male', 'female'))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  select(year, location, outcome, sex, value)

poulation.sex= as.data.frame(poulation.sex)

# RACE --------------------------------------------------------------------

population.race <- county_agegr_sex_race_eth%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  select(location, year, race, sex, count.by.sex)%>%
  group_by(year, location, race)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.race= as.data.frame(population.race)


# ETHNICITY ---------------------------------------------------------------

population.ethnicity <- county_agegr_sex_race_eth%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NH_MALE, NH_FEMALE, H_MALE, H_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of( "NH_MALE", "NH_FEMALE", "H_MALE", "H_FEMALE")), 
               names_to = c("ethnicity", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethnicity = as.character(census.eth.mappings[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  select(location, year, ethnicity, sex, count.by.sex)%>%
  group_by(year, location, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.ethnicity= as.data.frame(population.ethnicity)

# AGE (groups) ---------------------------------------------------------------------

poulation.age.groups <- county_agegr_sex %>%
  select(STATE, COUNTY, YEAR, UNDER5_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, AGE2529_TOT, AGE3034_TOT,
         AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT, AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT,
         AGE7579_TOT, AGE8084_TOT, AGE85PLUS_TOT)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  
  pivot_longer(cols=c("UNDER5_TOT", "AGE59_TOT", "AGE1014_TOT", "AGE1519_TOT", "AGE2024_TOT", "AGE2529_TOT", "AGE3034_TOT",
                      "AGE3539_TOT", "AGE4044_TOT", "AGE4549_TOT", "AGE5054_TOT", "AGE5559_TOT", "AGE6064_TOT", "AGE6569_TOT", "AGE7074_TOT",
                      "AGE7579_TOT", "AGE8084_TOT", "AGE85PLUS_TOT"),
               names_to = c("age", "outcome"),
               names_sep = "_",
               values_to = "value")%>%
  mutate(outcome = "population")%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  mutate(age = as.character(census.age.mappings.two[age]))%>%
  select(year, location, outcome, age, value)

poulation.age.groups= as.data.frame(poulation.age.groups)

# RACE + AGE --------------------------------------------------------------

population.race.age <- county_agegr_sex_race_eth%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings[AGEGRP]))%>%
  select(location, year, age, race, count.by.sex)%>%
  group_by(year, location, age, race)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, race, value)

population.race.age= as.data.frame(population.race.age)


# ETHNICITY + AGE ---------------------------------------------------------
population.ethnicity.age <- county_agegr_sex_race_eth%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NH_MALE, NH_FEMALE, H_MALE, H_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("NH_MALE", "NH_FEMALE", "H_MALE", "H_FEMALE")), 
               names_to = c("ethnicity", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethnicity = as.character(census.eth.mappings[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings[AGEGRP]))%>%
  select(location, year, age, ethnicity, count.by.sex)%>%
  group_by(year, location, age, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, ethnicity, value)

population.ethnicity.age= as.data.frame(population.ethnicity.age)


# SEX + AGE ---------------------------------------------------------------

population.sex.age <- county_agegr_sex%>%
  select(STATE, COUNTY, YEAR, UNDER5_MALE, AGE59_MALE, AGE1014_MALE, AGE1519_MALE, AGE2024_MALE, AGE2529_MALE, AGE3034_MALE,
         AGE3539_MALE, AGE4044_MALE, AGE4549_MALE, AGE5054_MALE, AGE5559_MALE, AGE6064_MALE, AGE6569_MALE, AGE7074_MALE,
         AGE7579_MALE, AGE8084_MALE, AGE85PLUS_MALE, UNDER5_FEM, AGE59_FEM, AGE1014_FEM, AGE1519_FEM, AGE2024_FEM, AGE2529_FEM, AGE3034_FEM,
         AGE3539_FEM, AGE4044_FEM, AGE4549_FEM, AGE5054_FEM, AGE5559_FEM, AGE6064_FEM, AGE6569_FEM, AGE7074_FEM,
         AGE7579_FEM, AGE8084_FEM, AGE85PLUS_FEM)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("UNDER5_MALE", "AGE59_MALE", "AGE1014_MALE", "AGE1519_MALE", "AGE2024_MALE", "AGE2529_MALE", "AGE3034_MALE",
                             "AGE3539_MALE", "AGE4044_MALE", "AGE4549_MALE", "AGE5054_MALE", "AGE5559_MALE", "AGE6064_MALE", "AGE6569_MALE", "AGE7074_MALE",
                             "AGE7579_MALE", "AGE8084_MALE", "AGE85PLUS_MALE", "UNDER5_FEM", "AGE59_FEM", "AGE1014_FEM", "AGE1519_FEM", "AGE2024_FEM", "AGE2529_FEM", "AGE3034_FEM",
                             "AGE3539_FEM", "AGE4044_FEM", "AGE4549_FEM", "AGE5054_FEM", "AGE5559_FEM", "AGE6064_FEM", "AGE6569_FEM", "AGE7074_FEM",
                             "AGE7579_FEM", "AGE8084_FEM", "AGE85PLUS_FEM")), 
               names_to = c("age", "sex"),
               names_sep = "_",
               values_to = "value")%>%
  mutate(sex = ifelse( sex == "MALE", 'male', 'female'))%>%
  mutate(year = as.character(year.mappings[YEAR]))%>%
  mutate(age = as.character(census.age.mappings.two[age]))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, sex, value)

population.sex.age= as.data.frame(population.sex.age)

# Put into CENSUS MANAGER --------------------------

stratified.20.23.data = list(
  population.total,
  poulation.sex,
  population.race,
  population.ethnicity,
  poulation.age.groups,
  population.race.age,
  population.ethnicity.age,
  population.sex.age)

for (data in stratified.20.23.data) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


##############################################################################
#ADDING A SECTION FOR SINGLE YEAR AGES TO ASSESS HOW WE WANT TO DO THINGS
##############################################################################
DATA.DIR.POP.20.23.AGE = "../../data_raw/new_census_data_june_2024/single_year_ages"

pop.20.23.single.year.age <- Sys.glob(paste0(DATA.DIR.POP.20.23.AGE, '/*.csv'))

#creating a list with sublists of filename, data#
pop.20.23.single.year.age.data <- lapply(pop.20.23.single.year.age, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})


# AGE (SINGLE YEAR) ---------------------------------------------------------------------
single.year.age = lapply(pop.20.23.single.year.age.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$STATE= str_pad(data$STATE, width=2, side="left", pad="0")
  data$COUNTY = str_pad(data$COUNTY, width=3, side="left", pad="0")
  data$location = paste(data$STATE, data$COUNTY, sep="")

  data$year = as.character(year.mappings[data$YEAR])
  data$value = as.numeric(data$TOT_POP)
  data$outcome="population"
  data$age = paste(data$AGE, "years", sep=" ")
  data$age = ifelse(data$age == "0 years", '< 1 year', data$age)
  data$age = ifelse(data$age == "1 years", '1 year', data$age)
  data$age = ifelse(data$age == "85 years", '85+ years', data$age)
  
  data<-data %>% 
  filter(year != "1")%>%
  select(outcome, location, year, age, value)

  data= as.data.frame(data)
  list(filename, data)
  
})


# PUT AGE (SINGLE YEAR)  -------------------------------------------------

single.year.age.put  = lapply(single.year.age, `[[`, 2)
for (data in single.year.age.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


# AGE + SEX (SINGLE YEAR) -------------------------------------------------
single.year.age.sex = lapply(pop.20.23.single.year.age.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$STATE= str_pad(data$STATE, width=2, side="left", pad="0")
  data$COUNTY = str_pad(data$COUNTY, width=3, side="left", pad="0")
  data$location = paste(data$STATE, data$COUNTY, sep="")
  
  data$year = as.character(year.mappings[data$YEAR])

  data$age = paste(data$AGE, "years", sep=" ")
  data$age = ifelse(data$age == "0 years", '< 1 year', data$age)
  data$age = ifelse(data$age == "1 years", '1 year', data$age)
  data$age = ifelse(data$age == "85 years", '85+ years', data$age)
  
   data<-data %>% 
     filter(year != "remove")%>%
     select(location, year, age, TOT_MALE, TOT_FEMALE)%>%
     pivot_longer(cols=c("TOT_MALE", "TOT_FEMALE"), 
                  names_to = c("outcome", "sex"),
                  names_sep = "_",
                  values_to = "value")

  data$outcome = 'population'
  data$sex = tolower(data$sex)
  
  data= as.data.frame(data)
  list(filename, data)
  
})
# PUT AGE + SEX (SINGLE YEAR) -------------------------------------------------

single.year.age.sex.put  = lapply(single.year.age.sex, `[[`, 2)
for (data in single.year.age.sex.put) {

  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
