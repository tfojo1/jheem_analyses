
# TOTAL -------------------------------------------------------------------

DATA.DIR.NAT.IMM="../../data_raw/syphilis.manager/national.immigration"

national.immigration.files <- Sys.glob(paste0(DATA.DIR.NAT.IMM, '/*.xlsx'))

national.immigration.total <- lapply(national.immigration.files, function(x){
  list(filename=x, data=read_excel(x, sheet= 1, col_types = c("text", "text", "text", "numeric")))
       })

national.immigration.total.clean = lapply(national.immigration.total, function(file){
  
  data=file[[2]]
  filename = file[[1]]

  data = subset(data, !is.na(data$value))
  
  data = as.data.frame(data)
  list(filename, data) 
})

national.total.put = lapply(national.immigration.total.clean, `[[`, 2)  

for (data in national.total.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.national',
    source = 'census.population',  
    dimension.values = list(),
    url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
    details = 'Geographic Mobility by Selected Characteristics in the United States')
}


# SEX ---------------------------------------------------------------------
DATA.DIR.IMM.SEX="../../data_raw/syphilis.manager/national.immigration/sex"

nat.imm.sex <- Sys.glob(paste0(DATA.DIR.IMM.SEX, '/*.xlsx'))

national.immigration.sex <- lapply(nat.imm.sex, function(x){
  list(filename=x, data=read_excel(x, sheet = "Data", range = "A19:B20", col_names = FALSE))
})

national.immigration.sex.clean = lapply(national.immigration.sex, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    rename(sex = '...1')%>%
    rename(value = '...2')%>%
    mutate(location = "US")%>%
    mutate(outcome = "immigration")
  
  data$value = as.numeric(gsub(",", '', data$value))
  data$sex = tolower(data$sex)
  data$year = filename
  data$year = substring(data$year,58, 61)
  
  data = as.data.frame(data)
  list(filename, data) 
  
})

national.sex.put = lapply(national.immigration.sex.clean, `[[`, 2)  

for (data in national.sex.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.national',
    source = 'census.population',  
    dimension.values = list(),
    url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
    details = 'Geographic Mobility by Selected Characteristics in the United States')
}

# AGE ---------------------------------------------------------------------
# Commenting out bc we decided to restructure the age groups
# DATA.DIR.IMM.AGE="../../data_raw/syphilis.manager/national.immigration/age"
# 
# nat.imm.age <- Sys.glob(paste0(DATA.DIR.IMM.AGE, '/*.xlsx'))
# 
# national.immigration.age <- lapply(nat.imm.age, function(x){
#   list(filename=x, data=read_excel(x, sheet = "Data", range = "A84:B98", col_names = FALSE))
# })
# 
# national.immigration.age.clean = lapply(national.immigration.age, function(file){
#   
#   data=file[[2]]
#   filename = file[[1]]
#   
#   data <- data %>%
#     rename(age = '...1')%>%
#     rename(value = '...2')%>%
#     mutate(location = "US")%>%
#     mutate(outcome = "immigration")
#   
#   data$value = as.numeric(gsub(",", '', data$value))
#   
#   data$age = gsub(' to ', '-', data$age)
#   data$age = gsub('75 years and over', '75+ years', data$age)
#   data$age = gsub(' and ', '-', data$age)
#   
#   data$year = filename
#   data$year = substring(data$year,58, 61)
#   
#   data = as.data.frame(data)
#   list(filename, data) 
#   
# })
# 
# national.age.put = lapply(national.immigration.age.clean, `[[`, 2)  
# 
# for (data in national.age.put) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'census.immigration.national',
#     source = 'census.population',  
#     dimension.values = list(),
#     url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
#     details = 'Geographic Mobility by Selected Characteristics in the United States')
# }


# RACE --------------------------------------------------------------------
#National Level Immigration data from census has race separate from ethnicity
#And no data available for download with non hispanic
#So we are going to create 4 racial groups using a proportion of black/hispanic 
#from CDC wonder.  This is what we did for MSA level immigration as well.
#Race = white NH, hispanic, black NH, other
#Pulled from cdc wonder
#Proportion of Black who are Hispanic: 14,568,073 / 218,869,956 = 0.067
#Proportion of Hispanic who are Black:  14,568,073 / 269,697,533 = 0.054

# -------------------------------------------------------------------------


DATA.DIR.IMM.race="../../data_raw/syphilis.manager/national.immigration/race"

nat.imm.race <- Sys.glob(paste0(DATA.DIR.IMM.race, '/*.xlsx'))

national.immigration.race <- lapply(nat.imm.race, function(x){
  list(filename=x, data=read_excel(x, sheet = "Data", range = "B8", col_names = FALSE))
})

national.immigration.race.clean = lapply(national.immigration.race, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    rename(value = '...1')%>%
    mutate(location = "US")%>%
    mutate(outcome = "immigration")
  
  data$value = as.numeric(gsub(",", '', data$value))
  
  data$year = filename
  data$year = str_sub(data$year,-9, -6)

  if(grepl("black", filename)) {
    data$race = as.character("black")
  }
  
  if(grepl("white", filename)) {
    data$race = as.character("white non hispanic")
  }
  
  if(grepl("hispanic", filename)) {
    data$race = as.character("hispanic")
  }
  
  data = as.data.frame(data)
  list(data) 
  
})

# RACE RE-CALCULATION -----------------------------------------------------
prop.black.hisp <- 0.067
prop.hisp.black <-0.054

race.df <- data.frame(matrix(unlist(national.immigration.race.clean), nrow=length(national.immigration.race.clean), byrow=T))%>%
  rename(value = X1 )%>%       #Turn the list into a dataframe
  rename(location = X2)%>%
  rename(outcome= X3)%>%
  rename(year = X4)%>%
  rename(race =X5 )%>%
  mutate(value = as.numeric(value))

white.non.hispanic <- race.df%>%
  filter(race == "white non hispanic") #Put this separately

black <- race.df%>%
  filter(race == "black") 

hispanic <- race.df%>%
  filter(race == "hispanic")

total.race <- as.data.frame.table(national.immigration.total.clean[[1]][[2]])%>% #need total for calculation
  rename(year = Freq.year)%>%
  rename(total = Freq.value)%>%
  select(total, year)%>%
  group_by(year)

total.race<- total.race[!duplicated(total.race), ] #remove duplicates from total

black.hispanic.combined <- cbind(black, hispanic) #Put black, hispanic, and total values into one data frame
colnames(black.hispanic.combined) <- c('black.value', 'location', 'outcome', 'year', 'black', 'hispanic.value', 'drop.one', 'drop.two', 'drop.three', 'hispanic')
black.hispanic.combined <- merge(black.hispanic.combined, total.race, by = 'year')

black.hispanic.combined <- black.hispanic.combined%>% #Clean up dataframe
  select(-drop.one, -drop.two, -drop.three)

black.hispanic.combined <- black.hispanic.combined %>% #Perform race calculations
  mutate(black.nh = round(black.value-(sqrt(prop.black.hisp*prop.hisp.black*hispanic.value*black.value))))%>% 
  mutate(other.race = round(total - (hispanic.value + black.nh)))  #This total includes white, so you have 3 groups

#create dataframes to put
hispanic.put <- black.hispanic.combined%>%
  select(year, location, outcome, hispanic, hispanic.value)%>%
  rename(race = hispanic)%>%
  rename(value= hispanic.value)

#black
black.put <- black.hispanic.combined%>%
  select(year, location, outcome, black, black.nh)%>%
  rename(race = black)%>%
  rename(value= black.nh)

#other
other.put <- black.hispanic.combined%>%
  select(year, location, outcome, other.race)%>%
  mutate(race = "other")%>%
  rename(value= other.race)



# Put Race Data -----------------------------------------------------------

#black
data.manager$put.long.form(
  data = black.put,
  ontology.name = 'census.immigration.national',
  source = 'census.population',  
  dimension.values = list(),
  url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
  details = 'Geographic Mobility by Selected Characteristics in the United States')

#other
data.manager$put.long.form(
  data = other.put,
  ontology.name = 'census.immigration.national',
  source = 'census.population',  
  dimension.values = list(),
  url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
  details = 'Geographic Mobility by Selected Characteristics in the United States')

#hispanic
data.manager$put.long.form(
  data = hispanic.put,
  ontology.name = 'census.immigration.national',
  source = 'census.population',  
  dimension.values = list(),
  url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
  details = 'Geographic Mobility by Selected Characteristics in the United States')
