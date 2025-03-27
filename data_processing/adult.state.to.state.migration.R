#This code uses the restratify age counts function to create an estimate of adult.immigration for state to state migration

#Split up the 5-17 age bracket to show only 13+; this will allow us to have 'adult.immigration' as an outcome


# STARTING WITH IMMIGRATION ONLY ------------------------------------------

# Adult Calculations ------------------------------------------------------

#CREATE VECTOR OF WHAT YOU WANT AGES TO BE
desired.ages <- c("1-12 years", "13-17 years", "18-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75+ years")

#AGE RE-CALCULATION
agearray.imm = data.manager$data$immigration$estimate$census.population$census.immigration.state.to.state$year__location__age

#APPLY RESTRATIFY AGE FUNCTION
restratify.age.array <- jheem2::restratify.age.counts(agearray.imm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#PROPORTION ARRAY:
total.array = apply(restratify.age.array, MARGIN = c("year", "location"), sum)
total.array #Denominator for proportion

adult.array = restratify.age.array[ , , 2:9]
adult.array.new = apply(adult.array, MARGIN = c("year", "location"), sum) #Numerator for proportion- only people ages 13+

proportion.adult.array = (adult.array.new/total.array) #Proportion array
adult.prop.df = as.data.frame.table(proportion.adult.array)

# TOTAL LEVEL ADULT IMMIGRATION BY STATE ----------------------------------

total.state.to.state = lapply(state.imm.data.clean, `[[`, 2) #This is the total level state to state migration data
total.state.to.state<- bind_rows(total.state.to.state, .id = "column_label")

adult.immigration.by.total = left_join(total.state.to.state, adult.prop.df, by=c("location", "year")) #join with adult proportion df

adult.immigration.by.total <- adult.immigration.by.total %>%
  mutate(value_new = round(value * Freq))%>%
  select(year, location, value_new)%>%
  rename(value = value_new)%>%
  mutate(outcome = 'adult.immigration')

# ADULT IMMIGRATION BY STATE - SEX ---------------------------------------
sex.state.to.state = lapply(state.immigration.sex, `[[`, 2) #This is the sex level state to state migration data
sex.state.to.state <- bind_rows(sex.state.to.state, .id = "column_label") 

adult.immgration.by.sex = left_join(sex.state.to.state, adult.prop.df, by=c("location", "year"))

adult.immgration.by.sex <- adult.immgration.by.sex %>%
  mutate(value_new = round(value * Freq))%>%
  mutate(outcome = 'adult.immigration')%>%
 select(outcome, year, location, sex, value_new)%>%
  rename(value = value_new)

# ADULT IMMIGRATION BY STATE - AGE ----------------------------------------
adult.immigration.by.age = as.data.frame.table(restratify.age.array)

adult.immigration.by.age <- adult.immigration.by.age  %>%
  filter(age != "1-12 years")%>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

# ADULT IMMIGRATION BY STATE - RACE ----------------------------------------
#state.immigration.race
#need to determine how we are going to use these racial groups#


# Put adult.immigration (total, sex, age) ---------------------------------
adult.immigration.state.to.state = list(adult.immigration.by.age,
                                          adult.immgration.by.sex,
                                        adult.immigration.by.total)

for (data in adult.immigration.state.to.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.state.to.state.adults', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')
}
# -------------------------------------------------------------------------

#Because we cannot find stratified emigration data we are going to apply
#the age proportions from adult immigration to emigration

# -------------------------------------------------------------------------

# Total adult.emigration by state -----------------------------------------

total.state.to.state.emigration = lapply(state.emm.data.clean, `[[`, 2) #This is the total level state to state migration data
total.state.to.state.emigration<- bind_rows(total.state.to.state.emigration, .id = "column_label")

adult.emmigration.by.total = left_join(total.state.to.state.emigration, adult.prop.df, by=c("location", "year")) #join with adult proportion df

adult.emmigration.by.total <- adult.emmigration.by.total %>%
  mutate(value_new = round(value * Freq))%>%
  select(year, location, value_new)%>%
  rename(value = value_new)%>%
  mutate(outcome = 'adult.emigration')


# Put
  
  data.manager$put.long.form(
    data = adult.emmigration.by.total,
    ontology.name = 'census.immigration.state.to.state.adults', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')


