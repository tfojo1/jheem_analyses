#SET UP FILES, assign file names
movement.file.names = file_path_sans_ext(basename(movement_files), compression = FALSE)
names(data.list.move.clean) = movement.file.names



################################################################################
##The immigration/emigration data has a 5-17 age bracket- need to split this out
##And then need to create adult.immigration/emigration outcome to just show 13+
################################################################################

#CREATE VECTOR OF WHAT YOU WANT AGES TO BE
desired.ages <- c("1-12 years", "13-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                  "65-69 years", "70-74 years", "75+ years")

#AGE RE-CALCULATION: EMIGRATION

agearray.emm = data.manager$data$emigration$estimate$census.population$census.immigration$year__location__age


#APPLY RESTRATIFY AGE FUNCTION: EMIGRATION
restratify.age.array.em <- jheem2::restratify.age.counts(agearray.emm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#AGE Re-CALCULATION: IMMIGRATION
agearray.imm = data.manager$data$immigration$estimate$census.population$census.immigration$year__location__age

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
total.adults.em <- data.list.move.clean[grep("emigration_total", names(data.list.move.clean))]
total.adults.em <- as.data.frame(total.adults.em$msa_emigration_total_11.15)
total.adults.df.em = left_join(total.adults.em, adult.prop.df.em, by="location")
total.adults.df.em <- total.adults.df.em %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)


#TOTAL: IMMIGRATION (+Proportion array)
total.adults.imm <- data.list.move.clean[grep("immigration_total", names(data.list.move.clean))]
total.adults.imm <- as.data.frame(total.adults.imm$msa_immigration_total_11.15)
total.adults.df.imm = left_join(total.adults.imm, adult.prop.df.imm, by="location")
total.adults.df.imm <- total.adults.df.imm %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)

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

#CREATE LISTS FOR IMMIGRATION/EMIGRATION TO PUT INTO MANAGER
adult.imm.em.list = list(
  "df.total.imm" = total.adults.df.imm, 
  "df.age.imm" = new.age.prop.1.imm,
  "df.sex.imm" = sex.adult.prop.imm,
  "df.total.em" = total.adults.df.em, 
  "df.age.em" = new.age.prop.1.em,
  "df.sex.em" = sex.adult.prop.em)


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


# Update for 1-21-25 ------------------------------------------------------
#Need to first calculate the adult population and then estimate the racial groups
#This is for adult.immigration and adult.emigration by race

#Apply the adult proportions to the original estimates of race/eth.  Then re-calc race/eth to get a fixed adult race/eth.

#ETHNICITY: IMMIGRATION ORIGINAL
eth.df.imm <- data.list.move.clean[grep("immigration_eth", names(data.list.move.clean))] 
eth.df.imm <- as.data.frame(eth.df.imm$msa_immigration_eth_11.15) 
eth.adult.prop.imm = left_join(eth.df.imm, adult.prop.df.imm, by="location")

eth.adult.prop.imm <- eth.adult.prop.imm %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select (-year.x, -year.y, -Freq, -X.msa_immigration_eth_11.15.)


#ETHNICITY: EMIGRATION ORIGINAL
eth.df.em <- data.list.move.clean[grep("emigration_eth", names(data.list.move.clean))] 
eth.df.em <- as.data.frame(eth.df.em$msa_emigration_eth_11.15) 
eth.adult.prop.em = left_join(eth.df.em, adult.prop.df.em, by="location")

eth.adult.prop.em <- eth.adult.prop.em %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-year.x, -year.y, -Freq, -X.msa_emigration_eth_11.15.)


#RACE: IMMIGRATION ORIGINAL
race.df.imm <- data.list.move.clean[grep("immigration_race", names(data.list.move.clean))] 
race.df.imm <- as.data.frame(race.df.imm$msa_immigration_race_11.15) 
race.adult.prop.imm = left_join(race.df.imm, adult.prop.df.imm, by="location")

race.adult.prop.imm <- race.adult.prop.imm %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select (-year.x, -year.y, -Freq, -X.msa_immigration_race_11.15.)


#RACE: EMIGRATION ORIGINAL
race.df.em <- data.list.move.clean[grep("emigration_race", names(data.list.move.clean))] 
race.df.em <- as.data.frame(race.df.em$msa_emigration_race_11.15) 
race.adult.prop.em = left_join(race.df.em, adult.prop.df.em, by="location")

race.adult.prop.em <- race.adult.prop.em %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-year.x, -year.y, -Freq, -X.msa_emigration_race_11.15.)

# -------------------------------------------------------------------------

#Now you have 4 data frames of the original race/eth categories by adult.pop
#Now you need to apply the re-calculation of race

prop.black.hisp <- 0.067
prop.hisp.black <-0.054


##IMMIGRATION##
immigration_total_adults <- as.data.frame(adult.imm.em.list$df.total.imm) #You need to put the adult immigration total
immigration_total_adults <- immigration_total_adults %>%
  rename(total = value)

imm_black_adults <- race.adult.prop.imm  #This is the dataframe you made
imm_black_adults <- imm_black_adults%>%
  rename(black = race)%>%
  rename(black.value = value)%>%
  select(-year, -outcome)

imm_hisp_adults <- eth.adult.prop.imm
imm_hisp_adults <- imm_hisp_adults %>%
  filter(race == 'hispanic or latino')%>%
  rename(hispanic = race)%>%
  rename(hispanic.value = value)%>%
  select(-year, -outcome)

imm_combo_1_adults <- merge(imm_hisp_adults, imm_black_adults, by="location")
imm_combo_adults <- merge(imm_combo_1_adults, immigration_total_adults, by="location")

imm_combo_adults <- imm_combo_adults %>%
  select(location, total, outcome, year, hispanic.value, black.value)%>% 
  mutate(black.nh = black.value-(sqrt(prop.black.hisp*prop.hisp.black*hispanic.value*black.value)))%>% 
  mutate(other.race = (total - (hispanic.value + black.nh)))

##EMIGRATION

emigration_total_adults <- as.data.frame(adult.imm.em.list$df.total.em)
emigration_total_adults <- emigration_total_adults %>%
  rename(total = value)

em_black_adults <- race.adult.prop.em 
em_black_adults <- em_black_adults%>%
  rename(black = race)%>%
  rename(black.value = value)

em_hisp_adults <- eth.adult.prop.em
em_hisp_adults <- em_hisp_adults %>%
  filter(race == 'hispanic or latino')%>%
  rename(hispanic = race)%>%
  rename(hispanic.value = value)

em_combo_1_adults <- merge(em_hisp_adults, em_black_adults, by="location")
em_combo_adults <- merge(em_combo_1_adults, emigration_total_adults, by="location")

em_combo_adults <- em_combo_adults %>%
  select(location, total, outcome, year, hispanic.value, black.value)%>% #add outcome and year back
  mutate(black.nh = black.value-(sqrt(prop.black.hisp*prop.hisp.black*hispanic.value*black.value)))%>% 
  mutate(other.race = total - (hispanic.value + black.nh))  

#Now you have recalculated adult race/eth groups.  Now select the proper groups to keep: black nh, hispanic, other

imm_other_adults <- imm_combo_adults %>%
  rename(value = other.race)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "other")%>%
  filter(value > 0)

em_other_adults <- em_combo_adults %>%
  rename(value = other.race)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "other")%>%
  filter(value > 0)

imm_blacknh_adults <- imm_combo_adults %>%
  rename(value = black.nh)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "black")%>%
  filter(value > 0)

em_blacknh_adults <- em_combo_adults %>%
  rename(value = black.nh)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "black")%>%
  filter(value > 0)

imm_hispanic_adults <- imm_combo_adults %>%
  rename(value = hispanic.value)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "hispanic or latino")%>%
  filter(value > 0)

em_hispanic_adults <- em_combo_adults %>%
  rename(value = hispanic.value)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "hispanic or latino")%>%
  filter(value > 0)

finished.imm.em.adult.race <- list(
  imm_other_adults,
  em_other_adults,
  imm_blacknh_adults,
  em_blacknh_adults,
  imm_hispanic_adults,
  em_hispanic_adults
)

#Put finsihed racial data into manager
for (data in finished.imm.em.adult.race) {
  
  data.manager$put.long.form( 
    data = data,
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}
