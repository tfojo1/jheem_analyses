
#Separating these years because file structure varies prior to 2000
library(readr)
library(readxl)
################################################################################
            ###Read 90-99 County Files###
################################################################################
DATA.DIR.CENSUS.90="../../data_raw/population/county_90.99"

ninties_files <- list.files(DATA.DIR.CENSUS.90, pattern = "txt", full.names = TRUE, recursive = TRUE)

#90.99 County Data#
data.list.county.90 <- lapply(ninties_files, function(x) {
    list(filename=x, data=read.table(x, quote="\"", comment.char="", colClasses=c(V2="character")))
  })

#1980's County data
sheets <- excel_sheets("~/JHEEM/data_raw/population/county_70.89/county_80.89.xls")
county_80.89 <- lapply(sheets, function(x) read_excel("~/JHEEM/data_raw/population/county_70.89/county_80.89.xls", sheet = x, skip=5))

#1970's County Data
county_70.79 <- read.csv("~/JHEEM/data_raw/population/county_70.89/county_70.79.csv", header=FALSE, colClasses=c(V2="character"))

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

data.list.90.county = lapply(data.list.county.90 , function(file){
  
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
###COUNTY 1980-1989 POPULATION###
################################################################################

################################################################################
###COUNTY 1970-1979 POPULATION###
################################################################################

county_70.79_clean <- county_70.79 %>%
  rename(year = V1)%>%
  rename(fips = V2) %>%  
  rename(race_sex_code = V3) %>%
  rename ("0-4 year olds" = V4)%>%
  rename ("5-9 year olds" = V5) %>%
  rename ("10-14 year olds"= V6) %>%
  rename ("15-19 year olds" = V7) %>%
  rename ("20-24 year olds" = V8) %>%
  rename ("25-29 year olds" = V9) %>%
  rename ("30-34 year olds" = V10) %>%
  rename ("35-39 year olds" = V11) %>%
  rename ("40-44 year olds" = V12) %>%
  rename ("45-49 year olds" = V13) %>%
  rename ("50-54 year olds" = V14) %>%
  rename ("55-59 year olds" = V15) %>%
  rename ("60-64 year olds" = V16) %>%
  rename ("65-69 year olds" = V17) %>%
  rename ("70-74 year olds" = V18) %>%
  rename ("75-79 year olds" = V19) %>%
  rename ("80-84 year olds" = V20) %>%
  rename ("85 years old and older" = V21) 
  
  
################################################################################
                  ###PUT INTO CENSUS MANAGER###

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
