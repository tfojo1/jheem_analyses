
################################################################################
##The immigration/emigration data has a 5-17 age bracket- need to split this out
##And then need to create adult.immigration/emigration outcome to just show 13+
################################################################################

#CREATE VECTOR OF WHAT YOU WANT AGES TO BE
desired.ages <- c("1-12 years", "13-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                  "65-69 years", "70-74 years", "75+ years")

#AGE RE-CALCULATION: EMIGRATION
age.em.df <- data.list.move.clean[grep("emigration_age", names(data.list.move.clean))] 
age.em.df <- as.data.frame(age.em.df$msa_emigration_age_11.15) 

age.em.df <- age.em.df%>%
  mutate(age = factor(age.em.df$age, levels =c("1-4 years", 
                                               "5-17 years", "18-19 years", "20-24 years", "25-29 years",
                                               "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                                               "50-54 years", "55-59 years", "60-64 years","65-69 years", 
                                               "70-74 years", "75+ years")))%>% 
  select(year, location, age, value)%>% 
  arrange(age,location, year)

agearray.dimnames.em <- list(year = unique(age.em.df$year), location = unique(age.em.df$location), age = unique(age.em.df$age))
agearray.emm<- array(data = age.em.df$value, 
                     dim= sapply(agearray.dimnames.em, length),
                     dimnames= agearray.dimnames.em)

#APPLY RESTRATIFY AGE FUNCTION: EMIGRATION
restratify.age.array.em <- jheem2::restratify.age.counts(agearray.emm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#AGE Re-CALCULATION: IMMIGRATION
age.df.imm <- data.list.move.clean[grep("immigration_age", names(data.list.move.clean))] 
age.df.imm <- as.data.frame(age.df.imm$msa_immigration_age_11.15) 
age.df.imm <- age.df.imm%>%
  mutate(age = factor(age.df.imm$age, levels =c("1-4 years", 
                                                "5-17 years", "18-19 years", "20-24 years", "25-29 years", 
                                                "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                                                "50-54 years", "55-59 years", "60-64 years","65-69 years", 
                                                "70-74 years", "75+ years")))%>% 
  select(year, location, age, value)%>% 
  arrange(age,location, year)

agearray.dimnames.imm <- list(year = unique(age.df.imm$year), location = unique(age.df.imm$location), age = unique(age.df.imm$age))
agearray.imm<- array(data = age.df.imm$value, 
                     dim= sapply(agearray.dimnames.imm, length),
                     dimnames= agearray.dimnames.imm)

#APPLY RESTRATIFY AGE FUNCTION: IMMIGRATION
restratify.age.array.imm <- jheem2::restratify.age.counts(agearray.imm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#PROPORTION ARRAY: EMIGRATION
total.array.em = apply(restratify.age.array.em, MARGIN = c("year", "location"), sum)
total.array.em #Denominator for proportion

adult.array.em = restratify.age.array.em[ , , 2:15]
adult.array.new.em = apply(adult.array.em, MARGIN = c("location"), sum) #Numerator for proportion- only people ages 13+

proportion.adult.array.em = (adult.array.new.em/total.array.em) #Proportion array
adult.prop.df.em = as.data.frame.table(proportion.adult.array.em)

#PROPORTION ARRAY: IMMIGRATION
total.array.imm = apply(restratify.age.array.imm, MARGIN = c("year", "location"), sum)
total.array.imm #Denominator for proportion

adult.array.imm = restratify.age.array.imm[ , , 2:15]
adult.array.new.imm = apply(adult.array.imm, MARGIN = c("location"), sum) #Numerator for proportion

proportion.adult.array.imm = (adult.array.new.imm/total.array.imm) #Proportion array
adult.prop.df.imm = as.data.frame.table(proportion.adult.array.imm)

#TOTAL: EMIGRATION
total.adults.em = as.data.frame.table(restratify.age.array.em)
total.adults.df.em <-total.adults.em %>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(age.group.sum = as.numeric(round(Freq)))%>%
  filter(age != "1-12 years")%>%
  select(-Freq, -age)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  group_by(location)%>%
  mutate(value = sum(age.group.sum))%>%
  select(-age.group.sum)

total.adults.df.em  = as.data.frame(total.adults.df.em)

#TOTAL: IMMIGRATION (+Proportion array)
total.adults.imm = as.data.frame.table(restratify.age.array.imm)
total.adults.df.imm <-total.adults.imm%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(age.group.sum = as.numeric(round(Freq)))%>%
  filter(age != "1-12 years")%>%
  select(-Freq, -age)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  group_by(location)%>%
  mutate(value = sum(age.group.sum))%>%
  select(-age.group.sum)

total.adults.df.imm  = as.data.frame(total.adults.df.imm)

#SEX: EMIGRATION
sex.df.em <- data.list.move.clean[grep("emigration_sex", names(data.list.move.clean))] 
sex.df.em <- as.data.frame(sex.df.em$msa_emigration_sex_11.15) 
sex.adult.prop.em = left_join(sex.df.em, adult.prop.df.em, by="location")
sex.adult.prop.em <- sex.adult.prop.em %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, sex, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)

#SEX: IMMIGRATION
sex.df.imm <- data.list.move.clean[grep("immigration_sex", names(data.list.move.clean))] 
sex.df.imm <- as.data.frame(sex.df.imm$msa_immigration_sex_11.15) 
sex.adult.prop.imm = left_join(sex.df.imm, adult.prop.df.imm, by="location")
sex.adult.prop.imm <- sex.adult.prop.imm %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, sex, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)

#RACE: EMIGRATION
# race.df.em <- data.list.move.clean[grep("emigration_race", names(data.list.move.clean))] 
# race.df.em <- as.data.frame(race.df.em$msa_emigration_race_11.15) 
# race.adult.prop.em = left_join(race.df.em, adult.prop.df.em, by="location")
# race.adult.prop.em <- race.adult.prop.em %>%
#   mutate(value_new = round(value * Freq))%>%
#   select(outcome, year.x, location, race, value_new)%>%
#   rename(value = value_new)%>%
#   mutate(year = as.character(year.x))%>%
#   mutate(outcome = 'adult.emigration')%>%
#   mutate(location = as.character(location))

#RACE: IMMIGRATION
# race.df.imm <- data.list.move.clean[grep("immigration_race", names(data.list.move.clean))] 
# race.df.imm <- as.data.frame(race.df.imm$msa_immigration_race_11.15) 
# race.adult.prop.imm = left_join(race.df.imm, adult.prop.df.imm, by="location")
# race.adult.prop.imm <- race.adult.prop.imm %>%
#   mutate(value_new = round(value * Freq))%>%
#   select(outcome, year.x, location, race, value_new)%>%
#   rename(value = value_new)%>%
#   mutate(year = as.character(year.x))%>%
#   mutate(outcome = 'adult.immigration')%>%
#   mutate(location = as.character(location))

#ETHNICITY: IMMIGRATION
eth.df.imm <- data.list.move.clean[grep("immigration_eth", names(data.list.move.clean))] 
eth.df.imm <- as.data.frame(eth.df.imm$msa_immigration_eth_11.15) 
eth.adult.prop.imm = left_join(eth.df.imm, adult.prop.df.imm, by="location")

eth.adult.prop.imm <- eth.adult.prop.imm %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select (-year.x, -year.y, -Freq, -X.msa_immigration_eth_11.15.)


#ETHNICITY: EMIGRATION
eth.df.em <- data.list.move.clean[grep("emigration_eth", names(data.list.move.clean))] 
eth.df.em <- as.data.frame(eth.df.em$msa_emigration_eth_11.15) 
eth.adult.prop.em = left_join(eth.df.em, adult.prop.df.em, by="location")

eth.adult.prop.em <- eth.adult.prop.em %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-year.x, -year.y, -Freq, -X.msa_emigration_eth_11.15.)


#AGE: IMMIGRATION
new.age.df.imm = as.data.frame.table(restratify.age.array.imm)

new.age.prop.1.imm <- new.age.df.imm  %>%
  filter(age != "1-12 years")%>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

#AGE: EMIGRATION
new.age.df.em = as.data.frame.table(restratify.age.array.em)

new.age.prop.1.em <- new.age.df.em %>%
  filter(age != "1-12 years")%>%
  mutate(outcome = "adult.emigration")%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

#OTHER RACE: EMIGRATION (Note this pulls dataset from immigration code)
other.emigration <- other_race[grep("emigration", names(other_race))] 
other.emigration <- as.data.frame(other.emigration$other.race.emigration)
other.emigration.adults = left_join(other.emigration, adult.prop.df.em, by = "location")

other.emigration.adults <- other.emigration.adults %>%
  mutate(outcome = "adult.emigration")%>%
  mutate(value = round(value*Freq))%>% #this is what i keep changing
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)

#OTHER RACE: IMMIGRATION (Note this pulls dataset from immigration code)
other.immigration <- other_race[grep("immigration", names(other_race))] 
other.immigration <- as.data.frame(other.immigration$other.race.immigration)
other.immigration.adults = left_join(other.immigration, adult.prop.df.imm, by = "location")

other.immigration.adults <- other.immigration.adults %>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(value*Freq))%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)

#RECALCULATED BLACK NH RACE: EMIGRATION (Note this pulls dataset from immigration code)
black.emigration <- black_nh_race[grep("emigration", names(black_nh_race))] 
black.emigration <- as.data.frame(black.emigration$black.nh.race.emigration)
black.emigration.adults = left_join(black.emigration, adult.prop.df.em, by = "location")

black.emigration.adults <- black.emigration.adults %>%
  mutate(outcome = "adult.emigration")%>%
  mutate(value = round(value*Freq))%>% #this is what i keep changing
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)

#RECALCULATED BLACK NH RACE: IMMIGRATION (Note this pulls dataset from immigration code)
black.immigration <- black_nh_race[grep("immigration", names(black_nh_race))] 
black.immigration <- as.data.frame(black.immigration$black.nh.race.immigration)
black.immigration.adults = left_join(black.immigration, adult.prop.df.imm, by = "location")

black.immigration.adults <- black.immigration.adults %>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(value*Freq))%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)



#CREATE LISTS FOR IMMIGRATION/EMIGRATION TO PUT INTO MANAGER
adult.imm.em.list = list(
  "df.total.imm" = total.adults.df.imm, 
  "df.age.imm" = new.age.prop.1.imm,
  "df.eth.imm" = eth.adult.prop.imm, 
  #"df.race.imm" = race.adult.prop.imm, 
  "df.sex.imm" = sex.adult.prop.imm,
  "df.other.race.imm"= other.immigration.adults,
  "df.black.imm" =black.immigration.adults,
  
  "df.total.em" = total.adults.df.em, 
  "df.age.em" = new.age.prop.1.em,
  "df.eth.em" = eth.adult.prop.em, 
  #"df.race.em" = race.adult.prop.em, 
  "df.sex.em" = sex.adult.prop.em ,
  "df.other.em"= other.emigration.adults,
  "df.black.em" =black.emigration.adults)


#PUT INTO DATA MANAGER
for (data in adult.imm.em.list) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}

###############################################################################
##Update March 11 2024: Using proportion of adults calculated above to apply to 
#new years of data that are not available with age strata
#you don't need to do this for 11-15 bc it's already done
#Uses these from above: adult.prop.df.em; adult.prop.df.imm
###############################################################################
proportion.df.immigration <- adult.prop.df.imm %>%
  select(location, Freq)%>%
  rename(adult.proportion = Freq)

proportion.df.emigration <- adult.prop.df.em %>%
  select(location, Freq)%>%
  rename(adult.proportion = Freq)

#immigration for 12.16, 13.17, 14.18, 15.19, 16.29
select.total.years <- data.list.move.clean[grep("_total", names(data.list.move.clean))]
select.years.of.interest <- select.total.years[grep("12.16|13.17|14.18|15.19|16.20", names(select.total.years))]

adult.movement.data.unstratified.years = lapply(select.years.of.interest, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  if(grepl("immigration", filename)) {
    data <- data%>%
      full_join(proportion.df.immigration, by = "location")%>%
      select(-outcome)%>%
      mutate(outcome = "adult.immigration")
  }
  
  if(grepl("emigration", filename)) {
    data <- data%>%
      full_join(  proportion.df.emigration, by = "location")%>%
      select(-outcome)%>%
      mutate(outcome = "adult.emigration")
  }
  
data <- data %>%
  mutate(new.value = value * adult.proportion)%>%
  rename(old.value = value)%>%
  rename(value = new.value) %>%
  filter(!is.na(adult.proportion))%>%
  filter(!is.na(old.value))
  

data= as.data.frame(data)
  
list(filename, data)

})

#PUT INTO DATA MANAGER

updated.adult.movement = lapply(adult.movement.data.unstratified.years, `[[`, 2)  

for (data in updated.adult.movement) {
  
  data.manager$put.long.form( 
    data = data,
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}
