#This code is to add the newer (6-27-24) stratified population data by county from the census to the census manger
#This will replace the stratified census data and be used for adult.population in the surveillance manager

#For race I'm going to use: white alone, black or african american alone, 
#american indian or alaska native alone, asian alone, 
#native hawaiian or pacific islander alone

#For ethnicity: total NH male + NH female (NH_MALE, NH_FEMALE); 
#then Hispanic male + Hispanic female (H-MALE, H_FEMALE)

DATA.DIR.POP.20.23 = "Q:/data_raw/population/county_20.23"

pop.20.23 <- Sys.glob(paste0(DATA.DIR.POP.20.23, '/*.csv'))

#creating a list with sublists of filename, data#
pop.20.23.data <- lapply(pop.20.23, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

county_agegr_sex_race_eth.20.23 <- pop.20.23.data[[1]][[2]]

# Mappings ----------------------------------------------------------------

year.mappings.20.23 = c('1' = 'remove',
                  '2' = "2020",
                  '3'= "2021",
                  '4'= "2022",
                  '5'= "2023")
census.race.mappings.20.23 = c('WA' = 'white',
                           'BA' = 'black',
                           'IA'= 'american indian and alaska native',
                           'AA' = 'asian',
                           'NA' = 'native hawaiian and other pacific islander', 
                           'TOM' = 'two or more races')
census.eth.mappings.20.23 = c(  'H' = 'hispanic',
                           'NH' = 'not hispanic')
census.age.mappings.20.23 = c('1' = '0-4 years', 
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
census.age.mappings.two.20.23 = c('UNDER5' = '0-4 years', 
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

# population.total.20.23 <- county_agegr_sex_race_eth.20.23 %>%
#   select(STATE, COUNTY, YEAR, POPESTIMATE)%>%
#   filter(YEAR != "1")%>% #remove the april population estimate
#   rename(value = POPESTIMATE)%>%
#   mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
#   mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
#   mutate(location = paste(STATE, COUNTY, sep=""))%>%
#   mutate(outcome = "population")%>%
#   mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
#   select(outcome, year, location, value)
# 
# population.total.20.23= as.data.frame(population.total.20.23)

# SEX ---------------------------------------------------------------------

# population.sex.20.23 <- county_agegr_sex_race_eth.20.23 %>%
#   select(STATE, COUNTY, YEAR, POPEST_MALE, POPEST_FEM)%>%
#   filter(YEAR != "1")%>% #remove the april population estimate
#   mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
#   mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
#   mutate(location = paste(STATE, COUNTY, sep=""))%>%
#   pivot_longer(cols=c("POPEST_MALE", "POPEST_FEM"),
#                names_to = c("outcome", "sex"),
#                names_sep = "_",
#                values_to = "value")%>%
#   mutate(outcome = "population")%>%
#   mutate(sex = ifelse( sex == "MALE", 'male', 'female'))%>%
#   mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
#   select(year, location, outcome, sex, value)
# 
# population.sex.20.23= as.data.frame(population.sex.20.23)

# RACE --------------------------------------------------------------------

population.race.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE, TOM_MALE, TOM_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  filter(AGEGRP == "0")%>% #only selecting the total here
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings.20.23[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  select(location, year, race, sex, count.by.sex)%>%
  group_by(year, location, race)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.race.20.23= as.data.frame(population.race.20.23)


# ETHNICITY ---------------------------------------------------------------

population.ethnicity.20.23 <- county_agegr_sex_race_eth.20.23%>%
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
  mutate(ethnicity = as.character(census.eth.mappings.20.23[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  select(location, year, ethnicity, sex, count.by.sex)%>%
  group_by(year, location, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  select(-sex, -count.by.sex)%>%
  mutate(outcome = "population")

population.ethnicity.20.23= as.data.frame(population.ethnicity.20.23)

# AGE (groups) ---------------------------------------------------------------------

# poulation.age.groups.20.23 <- county_agegr_sex_race_eth.20.23 %>%
#   select(STATE, COUNTY, YEAR, UNDER5_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, AGE2529_TOT, AGE3034_TOT,
#          AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT, AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT,
#          AGE7579_TOT, AGE8084_TOT, AGE85PLUS_TOT)%>%
#   filter(YEAR != "1")%>% #remove the april population estimate
#   mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
#   mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
#   mutate(location = paste(STATE, COUNTY, sep=""))%>%
#   
#   pivot_longer(cols=c("UNDER5_TOT", "AGE59_TOT", "AGE1014_TOT", "AGE1519_TOT", "AGE2024_TOT", "AGE2529_TOT", "AGE3034_TOT",
#                       "AGE3539_TOT", "AGE4044_TOT", "AGE4549_TOT", "AGE5054_TOT", "AGE5559_TOT", "AGE6064_TOT", "AGE6569_TOT", "AGE7074_TOT",
#                       "AGE7579_TOT", "AGE8084_TOT", "AGE85PLUS_TOT"),
#                names_to = c("age", "outcome"),
#                names_sep = "_",
#                values_to = "value")%>%
#   mutate(outcome = "population")%>%
#   mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
#   mutate(age = as.character(census.age.mappings.two.20.23[age]))%>%
#   select(year, location, outcome, age, value)
# 
# poulation.age.groups.20.23= as.data.frame(poulation.age.groups.20.23)

# RACE + AGE --------------------------------------------------------------

population.race.age.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
         NA_MALE, NA_FEMALE, TOM_FEMALE, TOM_MALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(race = as.character(census.race.mappings.20.23[race]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings.20.23[AGEGRP]))%>%
  select(location, year, age, race, count.by.sex)%>%
  group_by(year, location, age, race)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, race, value)

population.race.age.20.23= as.data.frame(population.race.age.20.23)


# ETHNICITY + AGE ---------------------------------------------------------
population.ethnicity.age.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NH_MALE, NH_FEMALE, H_MALE, H_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
  mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
  mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
  mutate(location = paste(STATE, COUNTY, sep=""))%>%
  pivot_longer(cols=c(one_of("NH_MALE", "NH_FEMALE", "H_MALE", "H_FEMALE")), 
               names_to = c("ethnicity", "sex"),
               names_sep = "_",
               values_to = "count.by.sex")%>%
  mutate(ethnicity = as.character(census.eth.mappings.20.23[ethnicity]))%>%
  mutate(sex = tolower(sex))%>%
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  filter(AGEGRP != "0")%>%
  mutate(age = as.character(census.age.mappings.20.23[AGEGRP]))%>%
  select(location, year, age, ethnicity, count.by.sex)%>%
  group_by(year, location, age, ethnicity)%>%
  mutate(value = sum(count.by.sex))%>%
  mutate(outcome = 'population') %>%
  select(outcome, location, year, age, ethnicity, value)

population.ethnicity.age.20.23= as.data.frame(population.ethnicity.age.20.23)


# SEX + AGE ---------------------------------------------------------------

# population.sex.age.20.23 <- county_agegr_sex%>%
#   select(STATE, COUNTY, YEAR, UNDER5_MALE, AGE59_MALE, AGE1014_MALE, AGE1519_MALE, AGE2024_MALE, AGE2529_MALE, AGE3034_MALE,
#          AGE3539_MALE, AGE4044_MALE, AGE4549_MALE, AGE5054_MALE, AGE5559_MALE, AGE6064_MALE, AGE6569_MALE, AGE7074_MALE,
#          AGE7579_MALE, AGE8084_MALE, AGE85PLUS_MALE, UNDER5_FEM, AGE59_FEM, AGE1014_FEM, AGE1519_FEM, AGE2024_FEM, AGE2529_FEM, AGE3034_FEM,
#          AGE3539_FEM, AGE4044_FEM, AGE4549_FEM, AGE5054_FEM, AGE5559_FEM, AGE6064_FEM, AGE6569_FEM, AGE7074_FEM,
#          AGE7579_FEM, AGE8084_FEM, AGE85PLUS_FEM)%>%
#   filter(YEAR != "1")%>% #remove the april population estimate
#   mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
#   mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
#   mutate(location = paste(STATE, COUNTY, sep=""))%>%
#   pivot_longer(cols=c(one_of("UNDER5_MALE", "AGE59_MALE", "AGE1014_MALE", "AGE1519_MALE", "AGE2024_MALE", "AGE2529_MALE", "AGE3034_MALE",
#                              "AGE3539_MALE", "AGE4044_MALE", "AGE4549_MALE", "AGE5054_MALE", "AGE5559_MALE", "AGE6064_MALE", "AGE6569_MALE", "AGE7074_MALE",
#                              "AGE7579_MALE", "AGE8084_MALE", "AGE85PLUS_MALE", "UNDER5_FEM", "AGE59_FEM", "AGE1014_FEM", "AGE1519_FEM", "AGE2024_FEM", "AGE2529_FEM", "AGE3034_FEM",
#                              "AGE3539_FEM", "AGE4044_FEM", "AGE4549_FEM", "AGE5054_FEM", "AGE5559_FEM", "AGE6064_FEM", "AGE6569_FEM", "AGE7074_FEM",
#                              "AGE7579_FEM", "AGE8084_FEM", "AGE85PLUS_FEM")), 
#                names_to = c("age", "sex"),
#                names_sep = "_",
#                values_to = "value")%>%
#   mutate(sex = ifelse( sex == "MALE", 'male', 'female'))%>%
#   mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
#   mutate(age = as.character(census.age.mappings.two.20.23[age]))%>%
#   mutate(outcome = 'population') %>%
#   select(outcome, location, year, age, sex, value)
# 
# population.sex.age.20.23= as.data.frame(population.sex.age.20.23)


# Race+Ethnicity+Sex ----------------------------------------------------------
population.race.eth.sex.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE, 
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
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
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  select(location, year, race, ethnicity, sex, count.by.sex)%>%
  mutate(outcome = "population")%>%
  mutate(value = count.by.sex)%>%
  select(-count.by.sex)

population.race.eth.sex.20.23= as.data.frame(population.race.eth.sex.20.23)


# Age + Race + Ethnicity --------------------------------------------------
population.age.race.eth.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
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
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  mutate(age = as.character(census.age.mappings.20.23[AGEGRP]))%>%
  mutate(outcome = "population")%>%
  select(outcome, location, year, sex, race, ethnicity, age, count.by.sex)%>%
  group_by(year, location, race, ethnicity, age)%>%
  mutate(value = sum(count.by.sex))%>%
  select(outcome, year, location, race, ethnicity, age, value)

population.age.race.eth.20.23<- population.age.race.eth.20.23[!duplicated(population.age.race.eth.20.23), ]

population.age.race.eth.20.23= as.data.frame(population.age.race.eth.20.23)

# Race+Ethnicity ----------------------------------------------------------
population.race.eth.20.23 <- population.race.eth.sex.20.23%>%
  group_by(year, location, race, ethnicity)%>%
  mutate(value.new = sum(value))%>%
  select(-sex, -value)%>%
  rename(value = value.new)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))

population.race.eth.20.23= as.data.frame(population.race.eth.20.23)

population.race.eth.20.23<- population.race.eth.20.23[!duplicated(population.race.eth.20.23), ]


# Race + Ethnicity + Sex + Age --------------------------------------------

population.race.eth.sex.age.20.23 <- county_agegr_sex_race_eth.20.23%>%
  select(STATE, COUNTY, YEAR, AGEGRP, NHIA_FEMALE , NHIA_MALE ,HIA_MALE , HIA_FEMALE,
         HAA_MALE, HAA_FEMALE, NHAA_MALE, NHAA_FEMALE,
         HBA_MALE, HBA_FEMALE, NHBA_MALE, NHBA_FEMALE,
         HWA_MALE, HWA_FEMALE, NHWA_MALE, NHWA_FEMALE,
         HNA_MALE, HNA_FEMALE, NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE, HTOM_MALE, HTOM_FEMALE)%>%
  filter(YEAR != "1")%>% #remove the april population estimate
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
  mutate(year = as.character(year.mappings.20.23[YEAR]))%>%
  mutate(ethnicity = ifelse(ethniticy.indcator == "N", "not hispanic", 'hispanic'))%>%
  mutate(age = as.character(census.age.mappings.20.23[AGEGRP]))%>%
  select(location, year, race, ethnicity, sex, age, count.by.sex)%>%
  mutate(outcome = "population")%>%
  mutate(value = count.by.sex)%>%
  select(-count.by.sex)

population.race.eth.sex.age.20.23= as.data.frame(population.race.eth.sex.age.20.23)

# Put into CENSUS MANAGER --------------------------

stratified.20.23.data = list(
  #population.total.20.23,
  #poulation.sex.20.23,
  population.race.20.23,
  population.ethnicity.20.23,
  #poulation.age.groups.20.23,
  population.race.age.20.23,
  population.ethnicity.age.20.23,
  population.race.eth.20.23,
  population.race.eth.sex.20.23,
  population.age.race.eth.20.23,
  population.race.eth.sex.age.20.23
  #population.sex.age.20.23
  )

# Remove Problem Locations ------------------------------------------------
#Note: Removing these in August 2024, they are counties from Alaska and Connecticut that existed after 2022
#Removing them because they create NA values for prior years which then prevent the calculation of the restratify.age.groups function
problem.locations = c("02063", "02066", "02261", "09001", "09003", "09005", "09007", "09009", "09011", 
                      "09013", "09015", "09110", "09120", "09130", "09140", "09150", "09160", "09170", 
                      "09180", "09190")

stratified.20.23.data = lapply(stratified.20.23.data, function(x) filter(x, !location %in% problem.locations))

# Put ---------------------------------------------------------------------

for (data in stratified.20.23.data) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values.to.distribute = list(race=c('two or more races')),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


##############################################################################
#ADDING A SECTION FOR SINGLE YEAR AGES TO ASSESS HOW WE WANT TO DO THINGS
##############################################################################
DATA.DIR.POP.20.23.AGE = "Q:/data_raw/population/county_20.23/single_year_ages"

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

  data$year = as.character(year.mappings.20.23[data$YEAR])
  data$value = as.numeric(data$TOT_POP)
  data$outcome="population"
  data$age = paste(data$AGE, "years", sep=" ")
  data$age = ifelse(data$age == "0 years", '< 1 year', data$age)
  data$age = ifelse(data$age == "1 years", '1 year', data$age)
  data$age = ifelse(data$age == "85 years", '85+ years', data$age)
  
  data<-data %>% 
  filter(year != "remove")%>%
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
  
  data$year = as.character(year.mappings.20.23[data$YEAR])

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

# TOTAL (SINGLE YEAR) ---------------------------------------------------------------------
single.year.total = lapply(pop.20.23.single.year.age.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$STATE= str_pad(data$STATE, width=2, side="left", pad="0")
  data$COUNTY = str_pad(data$COUNTY, width=3, side="left", pad="0")
  data$location = paste(data$STATE, data$COUNTY, sep="")
  
  data$year = as.character(year.mappings.20.23[data$YEAR])
  data$value = as.numeric(data$TOT_POP)
  data$outcome="population"
  data$age = paste(data$AGE, "years", sep=" ")
  data$age = ifelse(data$age == "0 years", '< 1 year', data$age)
  data$age = ifelse(data$age == "1 years", '1 year', data$age)
  data$age = ifelse(data$age == "85 years", '85+ years', data$age)
  
  data<-data %>% 
    filter(year != "remove")%>%
    select(outcome, location, year, age, value)%>%
    group_by(year, location)%>%
    mutate(summed.value = sum(value))%>%
    select(-value, -age)%>%
    rename(value = summed.value)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data)
  
})

# PUT TOTAL (SINGLE YEAR) -------------------------------------------------

single.year.total.put  = lapply(single.year.total, `[[`, 2)
for (data in single.year.total.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

# SEX (SINGLE YEAR) ---------------------------------------------------------------------
single.year.sex = lapply(pop.20.23.single.year.age.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$STATE= str_pad(data$STATE, width=2, side="left", pad="0")
  data$COUNTY = str_pad(data$COUNTY, width=3, side="left", pad="0")
  data$location = paste(data$STATE, data$COUNTY, sep="")
  
  data$year = as.character(year.mappings.20.23[data$YEAR])
  
  data<-data %>% 
    filter(year != "remove")%>%
    select(location, year, AGE, TOT_MALE, TOT_FEMALE)%>%
    pivot_longer(cols=c("TOT_MALE", "TOT_FEMALE"), 
                 names_to = c("outcome", "sex"),
                 names_sep = "_",
                 values_to = "value")%>%
    group_by(year, location, sex)%>%
    mutate(summed.value = sum(value))%>%
    select(-AGE, -value)%>%
    rename(value = summed.value)
  
  data$outcome = 'population'
  data$sex = tolower(data$sex)
  
  data= as.data.frame(data)
  list(filename, data)
  
})

# PUT TOTAL (SINGLE YEAR) -------------------------------------------------

single.year.sex.put  = lapply(single.year.sex, `[[`, 2)
for (data in single.year.sex.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting- Stratified 2020-2023 data is from the Vintage 2023')
}
