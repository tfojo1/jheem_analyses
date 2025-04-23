#August 8th, 2024
#This is stratified census data by county for 2010-2019
#This will eventually replace the cdc_wonder 2018-2019 data in the surveillance manager and the 
#census_sas_files 2005-2017 data for the surveillance manager. Everything will be kept in the census
#manager and this will just be an addition.

DATA.DIR.POP.10.19 = "Q:/data_raw/population/county_stratified_10.19"

pop.10.19 <- Sys.glob(paste0(DATA.DIR.POP.10.19, '/*.csv'))

#creating a list with sublists of filename, data#
pop.10.19.data <- lapply(pop.10.19, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

county_agegr_sex_race_eth_10.19 <- pop.10.19.data[[1]][[2]]

# Mappings ----------------------------------------------------------------

year.mappings.10.19 = c("3"="2010",
                  "4"="2011",
                  "5"="2012",
                  "6"="2013",
                  "7"="2014",
                  "8"="2015",
                  "9"="2016",
                  "10"="2017",
                  "11"="2018",
                  "12"="2019")
census.race.mappings.10.19 = c('WA' = 'white',
                         'BA' = 'black',
                         'IA'= 'american indian and alaska native',
                         'AA' = 'asian',
                         'NA' = 'native hawaiian and other pacific islander',
                         'TOM' = 'two or more races')
census.eth.mappings.10.19 = c(  'H' = 'hispanic',
                          'NH' = 'not hispanic')
census.age.mappings.10.19 = c('1' = '0-4 years', 
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
# RACE --------------------------------------------------------------------
#TAKE OUT YEAR = 1, 2
population.race <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE, TOM_MALE, TOM_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings.10.19[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  select(location, year, race, sex, count.by.sex)%>%
  group_by(year, location, race)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.race= as.data.frame(population.race)


# ETHNICITY ---------------------------------------------------------------
population.ethnicity <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NH_MALE, NH_FEMALE, H_MALE, H_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
    filter(YEAR != "1")%>%
      filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of( "NH_MALE", "NH_FEMALE", "H_MALE", "H_FEMALE")),
               names_to = c("ethnicity", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethnicity = as.character(census.eth.mappings.10.19[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  select(location, year, ethnicity, sex, count.by.sex)%>%
  group_by(year, location, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.ethnicity= as.data.frame(population.ethnicity)

# AGE (groups) ---------------------------------------------------------------------

 population.age.groups <- county_agegr_sex_race_eth_10.19 %>%
  select(STATE, COUNTY, YEAR, AGEGRP, TOT_POP)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(year = year.mappings.10.19[YEAR])%>%
  filter(AGEGRP != "0")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  mutate(age = census.age.mappings.10.19[AGEGRP])%>%
  mutate(outcome = "population")%>%
  rename(value = TOT_POP)%>%
  select(year, location, outcome, age, value)

# SEX ---------------------------------------------------------------------

population.sex<- county_agegr_sex_race_eth_10.19 %>%
  select(STATE, COUNTY, YEAR, AGEGRP, TOT_MALE, TOT_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(year = year.mappings.10.19[YEAR])%>%
  filter(AGEGRP == "0")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("TOT_MALE", "TOT_FEMALE")),
               names_to = c('pop', "sex"),
               names_sep = "_",
               values_to = "value")%>%
  mutate(outcome = "population")%>%
  mutate(sex = tolower(sex))%>%
  select(year, location, outcome, sex, value)

population.sex= as.data.frame(population.sex)

# SEX+AGE GROUPS ---------------------------------------------------------------------

population.sex.age<- county_agegr_sex_race_eth_10.19 %>%
  select(STATE, COUNTY, YEAR, AGEGRP, TOT_MALE, TOT_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(year = year.mappings.10.19[YEAR])%>%
  filter(AGEGRP != "0")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("TOT_MALE", "TOT_FEMALE")),
               names_to = c('pop', "sex"),
               names_sep = "_",
               values_to = "value")%>%
  mutate(outcome = "population")%>%
  mutate(sex = tolower(sex))%>%
  mutate(age = as.character(census.age.mappings.10.19[AGEGRP]))%>%
  select(year, location, outcome, sex, age, value)

population.sex.age= as.data.frame(population.sex.age)


# RACE + AGE --------------------------------------------------------------
population.race.age <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE, TOM_MALE, TOM_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE")),
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings.10.19[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings.10.19[AGEGRP]))%>%
  select(location, year, age, race, count.by.sex)%>%
  group_by(year, location, age, race)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, race, value)

population.race.age= as.data.frame(population.race.age)

 
# # ETHNICITY + AGE ---------------------------------------------------------
population.ethnicity.age <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NH_MALE, NH_FEMALE, H_MALE, H_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("NH_MALE", "NH_FEMALE", "H_MALE", "H_FEMALE")),
               names_to = c("ethnicity", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethnicity = as.character(census.eth.mappings.10.19[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings.10.19[AGEGRP]))%>%
  select(location, year, age, ethnicity, count.by.sex)%>%
  group_by(year, location, age, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, ethnicity, value)

population.ethnicity.age= as.data.frame(population.ethnicity.age)


# Race+Ethnicity+Sex ----------------------------------------------------------
population.race.eth.sex <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of("NHIA_FEMALE" , "NHIA_MALE" ,"HIA_MALE" , "HIA_FEMALE",
                             "HAA_MALE", "HAA_FEMALE", "NHAA_MALE", "NHAA_FEMALE",
                             "HBA_MALE", "HBA_FEMALE", "NHBA_MALE", "NHBA_FEMALE",
                             "HWA_MALE", "HWA_FEMALE", "NHWA_MALE", "NHWA_FEMALE",
                             "HNA_MALE", "HNA_FEMALE", "NHNA_MALE", "NHNA_FEMALE",
                             "NHTOM_MALE", "NHTOM_FEMALE", "HTOM_MALE", "HTOM_FEMALE")),
               names_to = c("combined.race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethniticy.indcator = substring(combined.race, 1, 1))%>%
  mutate(race = case_when(combined.race == "HAA" ~ "asian",
                          combined.race == "HBA" ~ "black",
                          combined.race == "HIA" ~ "american indian and alaska native",
                          combined.race == "HNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "HWA" ~ "white",
                          combined.race == "NHAA" ~ "asian",
                          combined.race == "NHBA" ~ "black",
                          combined.race == "NHIA" ~ "american indian and alaska native",
                          combined.race == "NHNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "NHWA" ~ "white",
                          combined.race == "HTOM" ~ "two or more races",
                          combined.race == "NHTOM" ~ "two or more races"))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  select(location, year, race, ethnicity, sex, count.by.sex)%>%
  mutate(outcome = "population")%>%
  mutate(value = count.by.sex)%>%
  select(-count.by.sex)

population.race.eth.sex= as.data.frame(population.race.eth.sex)
 

# # Age + Race + Ethnicity --------------------------------------------------
population.age.race.eth <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP != "0")%>% #Removing total age
  pivot_longer(cols=c(one_of("NHIA_FEMALE" , "NHIA_MALE" ,"HIA_MALE" , "HIA_FEMALE",
                             "HAA_MALE", "HAA_FEMALE", "NHAA_MALE", "NHAA_FEMALE",
                             "HBA_MALE", "HBA_FEMALE", "NHBA_MALE", "NHBA_FEMALE",
                             "HWA_MALE", "HWA_FEMALE", "NHWA_MALE", "NHWA_FEMALE",
                             "HNA_MALE", "HNA_FEMALE", "NHNA_MALE", "NHNA_FEMALE",
                             "NHTOM_MALE", "NHTOM_FEMALE", "HTOM_MALE", "HTOM_FEMALE")),
               names_to = c("combined.race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethniticy.indcator = substring(combined.race, 1, 1))%>%
  mutate(race = case_when(combined.race == "HAA" ~ "asian",
                          combined.race == "HBA" ~ "black",
                          combined.race == "HIA" ~ "american indian and alaska native",
                          combined.race == "HNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "HWA" ~ "white",
                          combined.race == "NHAA" ~ "asian",
                          combined.race == "NHBA" ~ "black",
                          combined.race == "NHIA" ~ "american indian and alaska native",
                          combined.race == "NHNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "NHWA" ~ "white",
                          combined.race == "HTOM" ~ "two or more races",
                          combined.race == "NHTOM" ~ "two or more races"))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  mutate(age = as.character(census.age.mappings.10.19[AGEGRP]))%>%
  mutate(outcome = "population")%>%
  select(outcome, location, year, sex, race, ethnicity, age, count.by.sex)%>%
  group_by(year, location, race, ethnicity, age)%>%
  mutate(value = sum(count.by.sex))%>%
  select(outcome, year, location, race, ethnicity, age, value)

population.age.race.eth<- population.age.race.eth[!duplicated(population.age.race.eth), ]

population.age.race.eth= as.data.frame(population.age.race.eth)

# # Race+Ethnicity ----------------------------------------------------------
population.race.eth <- population.race.eth.sex%>%
  group_by(year, location, race, ethnicity)%>%
  mutate(value.new = sum(value))%>%
  select(-sex, -value)%>%
  rename(value = value.new)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))

population.race.eth= as.data.frame(population.race.eth)

population.race.eth<- population.race.eth[!duplicated(population.race.eth), ]
 
 
# # Race + Ethnicity + Sex + Age --------------------------------------------

population.race.eth.sex.age <- county_agegr_sex_race_eth_10.19%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  mutate(YEAR = as.character(YEAR))%>%
  filter(YEAR != "1")%>% 
  filter(YEAR != "2")%>% 
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP != "0")%>% #Removing total age
  pivot_longer(cols=c(one_of("NHIA_FEMALE" , "NHIA_MALE" ,"HIA_MALE" , "HIA_FEMALE",
                             "HAA_MALE", "HAA_FEMALE", "NHAA_MALE", "NHAA_FEMALE",
                             "HBA_MALE", "HBA_FEMALE", "NHBA_MALE", "NHBA_FEMALE",
                             "HWA_MALE", "HWA_FEMALE", "NHWA_MALE", "NHWA_FEMALE",
                             "HNA_MALE", "HNA_FEMALE", "NHNA_MALE", "NHNA_FEMALE",
                             "NHTOM_MALE", "NHTOM_FEMALE", "HTOM_MALE", "HTOM_FEMALE")),
               names_to = c("combined.race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethniticy.indcator = substring(combined.race, 1, 1))%>%
  mutate(race = case_when(combined.race == "HAA" ~ "asian",
                          combined.race == "HBA" ~ "black",
                          combined.race == "HIA" ~ "american indian and alaska native",
                          combined.race == "HNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "HWA" ~ "white",
                          combined.race == "NHAA" ~ "asian",
                          combined.race == "NHBA" ~ "black",
                          combined.race == "NHIA" ~ "american indian and alaska native",
                          combined.race == "NHNA" ~ "native hawaiian and other pacific islander",
                          combined.race == "NHWA" ~ "white",
                          combined.race == "HTOM" ~ "two or more races",
                          combined.race == "NHTOM" ~ "two or more races"))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.10.19[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  mutate(age = as.character(census.age.mappings.10.19[AGEGRP]))%>%
  select(location, year, race, ethnicity, sex, age, count.by.sex)%>%
  mutate(outcome = "population")%>%
  mutate(value = count.by.sex)%>%
  select(-count.by.sex)

population.race.eth.sex.age= as.data.frame(population.race.eth.sex.age)

# # Put into CENSUS MANAGER --------------------------

 stratified.10.19.data = list(
   population.race,
   population.ethnicity,
   population.age.groups,
   population.sex,
   population.sex.age,
   population.race.age,
   population.ethnicity.age,
   population.race.eth,
   population.race.eth.sex,
   population.age.race.eth,
   population.race.eth.sex.age
)


# Remove Problem Locations ------------------------------------------------
#Note: Removing these in August 2024, they are counties from Alaska and Connecticut that existed after 2022
#Removing them because they create NA values for prior years which then prevent the calculation of the restratify.age.groups function

# problem.locations = c("02063", "02066", "02261", "09001", "09003", "09005", "09007", "09009", "09011", 
#                       "09013", "09015", "09110", "09120", "09130", "09140", "09150", "09160", "09170", 
#                       "09180", "09190")

problem.locations = c("02063", "02066", "02261")

stratified.10.19.data = lapply(stratified.10.19.data, function(x) filter(x, !location %in% problem.locations))

# Put ---------------------------------------------------------------------


for (data in stratified.10.19.data) {

  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values.to.distribute = list(race=c('two or more races')),
    url = 'www.census.gov',
    details = 'Census Reporting- Stratified 2010-2019 data is from the Vintage 2019')
}

