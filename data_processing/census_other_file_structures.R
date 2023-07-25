
#Separating these years because file structure varies prior to 2000
library(readr)
################################################################################
            ###Read 90-99 State and County Files###
################################################################################
DATA.DIR.CENSUS.90="../../data_raw/population/state_county_90.99"
DATA.DIR.CENSUS.80="../../data_raw/population/state_80.89"

ninties_files <- list.files(DATA.DIR.CENSUS.90, pattern = "txt", full.names = TRUE, recursive = TRUE)
eighties_files <- list.files(DATA.DIR.CENSUS.80, pattern = "txt", full.names = TRUE, recursive = TRUE)

#creating a list with sublists of filename, data#
data.list.state.county.90 <- lapply(ninties_files, function(x) {
    list(filename=x, data=read.table(x, quote="\"", comment.char="", colClasses=c(V2="character")))
  })

#This uses readr-1980 state
data.list.state.80 <- lapply(eighties_files, function(x) {
  list(filename=x, data=read_table(x, skip=26))
})

#1970 state-manual
state_70_79 <- read_table("~/JHEEM/data_raw/population/manual/state_70.79.txt", 
                          col_names = FALSE, skip = 14)


################################################################################
###Create Mappings for 90-99 Files###
################################################################################
year.mappings = c('90' = '1990',
                  '91' = '1991',
                  '92' = '1992',
                  '93' = '1993',
                  '94' = '1994',
                  '95' = '1995',
                  '96' = '1996',
                  '97' = '1997',
                  '98' = '1998',
                  '99' = '1999')

age.mappings.1 = c(	'0' = 'under 1 year',
                    '1' = '1-4 years',
                    '2' = '5-9 years',
                    '3' = '10-14 years',
                    '4' = '15-19 years',
                    '5' = '20-24 years',
                    '6' = '25-29 years',
                    '7' = '30-34 years',
                    '8' = '35-39 years',
                    '9' = '40-44 years',
                    '10' = '45-49 years',
                    '11' = '50-54 years',
                    '12' = '55-59 years',
                    '13' = '60-64 years',
                    '14' = '65-69 years',
                    '15' = '70-74 years',
                    '16' = '75-79 years',
                    '17' = '80-84 years',
                    '18' = '85 years and over')

race.sex.mappings = c('1' = 'White male',
                      '2' = 'White female',
                      '3' = 'Black male',
                      '4' = 'Black female',
                      '5' = 'American Indian or Alaska Native male',
                      '6' = 'American Indian or Alaska Native female',
                      '7' = 'Asian or Pacific Islander male',
                      '8' = 'Asian or Pacific Islander female')

ethnicity.mappings = c('1' = 'non-hispanic',
                       '2' = 'hispanic or latino')

################################################################################
                    ###COUNTY 1990-1999 POPULATION###
################################################################################

data.list.90.county = lapply(data.list.state.county.90 , function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = as.character(data$V1)    #you could take a lot of this out and just use it in the demos#
  data$location = data$V2
  data$age_group = as.character(data$V3)
  data$race_sex = data$V4
  data$ethnicity = data$V5
  data$population = as.numeric(data$V6)
  
  data$year = year.mappings[data$year]
  data$age_group = age.mappings.1[data$age_group]
  data$race_sex = race.sex.mappings[data$race_sex]
  data$ethnicity = ethnicity.mappings[data$ethnicity]
  
  data <- data %>%
    select(year, location, age_group, race_sex, ethnicity, population)
  
  data <- data %>%
    group_by(location) %>%
    summarise(sum_population = sum(population),
              .groups='drop')
  
  data$value= data$sum_population
  data$year = "1990"
  data$outcome = "population" 
  
  list(filename, data)  

})

################################################################################
                    ###STATE 1970-1979 POPULATION###
################################################################################
state_70_79_clean <- state_70_79 %>%
  rename(state_fips= X1) %>%
  rename(state= X2) %>%
  rename(age= X3) %>%
  rename(pop_1970= X4) %>%
  rename(pop_1971= X5) %>%
  rename(pop_1972= X6) %>%
  rename(pop_1973= X7) %>%
  rename(pop_1974= X8) %>%
  rename(pop_1975= X9) %>%
  rename(pop_1976= X10) %>%
  rename(pop_1977= X11) %>%
  rename(pop_1978= X12) %>%
  rename(pop_1979= X13) %>%  #I'm going to not use this 1980 estimate and rely on the more recent one#
  select(-c(X14)) %>%
 
  
  pivot_longer(cols=c("pop_1970", "pop_1971", "pop_1972", "pop_1973", "pop_1974", "pop_1975", "pop_1976", "pop_1977", "pop_1978", "pop_1979"),
               names_to = c("outcome", "year"),
               names_sep = "_",
               values_to = "value") %>%

  
  group_by(state, year) %>%
  summarise(sum_population = sum(value)) %>%
  
  rename(value = sum_population)%>%
  rename(location = state) %>%
  mutate(outcome = "population")

file= "State 1970-1979"
state_70_79_clean_list = list(file, state_70_79_clean)
################################################################################
      ###STATE 1980-1989 POPULATION###
################################################################################
data.list.80.state = lapply(data.list.state.80 , function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$fips = as.numeric(data$Code)
  data$state = data$Age
  data$age_group = data$Both
  data$both_sexes = data$sexes
  data$male = data$Male
  data$female = data$Female

  
  data<- data %>%
    select(state, both_sexes)%>%
    group_by(state) %>%
    summarise(sum_population = sum(both_sexes),
              .groups='drop')
  
  data$location= data$state
  data$outcome= "population"
  data$value = data$sum_population
  
  #make a year var based on file name bc there isnt a year var and each data set is a different year
  if(grepl("1980", filename)) {
    data$year="1980"  
  }
  if(grepl("1981", filename)) {
    data$year="1981"  
  }
  if(grepl("1982", filename)) {
    data$year="1982"  
  }
  if(grepl("1983", filename)) {
    data$year="1983"  
  }
  if(grepl("1984", filename)) {
    data$year="1984"  
  }
  if(grepl("1985", filename)) {
    data$year="1985"  
  }
  if(grepl("1986", filename)) {
    data$year="1986"  
  }
  if(grepl("1987", filename)) {
    data$year="1987"  
  }
  if(grepl("1988", filename)) {
    data$year="1988"  
  }
  if(grepl("1989", filename)) {
    data$year="1989"  
  }
  
  data= as.data.frame(data)
  
  list(filename, data)  
  
})
################################################################################
              ###STATE 1990-1999 POPULATION###
################################################################################

data.list.90.state = lapply(data.list.state.county.90 , function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = as.character(data$V1)    #you could take a lot of this out and just use it in the demos#
  data$fips = data$V2
  data$age_group = as.character(data$V3)
  data$race_sex = data$V4
  data$ethnicity = data$V5
  data$population = as.numeric(data$V6)
  
  data$year = year.mappings[data$year]
  data$age_group = age.mappings.1[data$age_group]
  data$race_sex = race.sex.mappings[data$race_sex]
  data$ethnicity = ethnicity.mappings[data$ethnicity]
  
  data$state_fips = substr(data$fips, 1, 2) #select states
  
   data <- data %>%
     select(year, state_fips, age_group, race_sex, ethnicity, population)
 
  data <- data %>%
    group_by(state_fips) %>%
    summarise(sum_population = sum(population),
              .groups='drop')
  
  
  data$value= data$sum_population
  data$year = "1990"
  data$outcome = "population"
  
  data= as.data.frame(data)
  
  #data$location = (locations::get.location.code(data$state_fips, "STATE")) #this is returning a list- can I map bc it's faster? 

  list(filename, data)  
  
})

################################################################################
                  ###PUT INTO CENSUS MANAGER###

#STATE POPULATION VALUES 1970-1979
state_70_pop = lapply(state_70_79_clean_list, `[[`, 2)  #I think this doesnt work bc it's not a list anymore#

for (data in state_70_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#STATE POPULATION VALUES 1990-1999
state_90_pop = lapply(data.list.90.state, `[[`, 2)

for (data in state_90_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#COUNTY POPULATION VALUES 1990-1999
county_90_pop = lapply(data.list.90.county, `[[`, 2)

for (data in county_90_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
