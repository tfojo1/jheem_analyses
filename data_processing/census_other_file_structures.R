
#Separating these years because file structure varies prior to 2000

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

#this uses readr bc these are a mess
data.list.state.80 <- lapply(eighties_files, function(x) {
  list(filename=x, data=read_table(x, skip=27))
})


state_1984 <- read_table("~/JHEEM/data_raw/population/state_80.89/state_1984.txt", 
                         skip = 27)

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
################################################################################

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
