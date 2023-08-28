
#Separating these years because file structure varies prior to 2000
#This file includes population totals and demographics for 1970-1989
#But because the racial groups prior to 2000 are different I'm not sure
#how much of this we will use

#8/21: I'm adding back in the 1990s data so we can have population totals for
#1990-1999

#The demographics are commented out/not working bc of the racial ontology issue
#Decide if you want these in

################################################################################
                  ###Read 1970-1989 County Files###
################################################################################
#90.99 County Data#
DATA.DIR.CENSUS.90="../../data_raw/population/county_90.99"

ninties_files <- list.files(DATA.DIR.CENSUS.90, pattern = "txt", full.names = TRUE, recursive = TRUE)

data.list.county.90 <- lapply(ninties_files, function(x) {
  list(filename=x, data=read.table(x, quote="\"", comment.char="", colClasses=c(V2="character")))
})


#1980's County data
sheets <- excel_sheets("~/JHEEM/data_raw/population/county_70.89/county_80.89.xls")
county_80.89 <- lapply(sheets, function(x) {
  list(sheet=x, data=read_excel("~/JHEEM/data_raw/population/county_70.89/county_80.89.xls", sheet = x, skip=5))
})

#1970's County Data
county_70.79 <- read.csv("~/JHEEM/data_raw/population/county_70.89/county_70.79.csv", header=FALSE, colClasses=c(V2="character"))

################################################################################
                  ###Create Mappings###
################################################################################
race.sex.70s.mappings= c('1' = 'White male',
                         '2' = 'White female',
                         '3' = 'Black male',
                         '4' = 'Black female',
                         '5' = 'other race male',
                         '6' = 'other race female')

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

################################################################################
                  ###COUNTY 1990-1999 POPULATION###
################################################################################

data.list.county.90.clean = lapply(data.list.county.90 , function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#
  
  data$year = as.character(data$V1)    #you could take a lot of this out and just use it in the demos#
  data$location = data$V2
  data$age_group = as.character(data$V3)
  data$race_sex = data$V4
  data$ethnicity = data$V5
  data$population = as.numeric(data$V6)
  
  data$year = year.mappings[data$year]
  # data$race_sex = race.sex.90s.mappings[data$race_sex]
  # data$ethnicity = ethnicity.mappings[data$ethnicity]
  
  data <- data %>%
    select(year, location, age_group, ethnicity, population) %>%
    group_by(location) %>%   #don't need to group by year bc each df is a separate year#
    summarise(sum_population = sum(population),
              .groups='drop')
  
  data$value= data$sum_population
  data$outcome = "population"
  
  if(grepl("1990", filename)) {
    data$year = "1990"
  }
  if(grepl("1991", filename)) {
    data$year = "1991"
  }
  if(grepl("1992", filename)) {
    data$year = "1992"
  }
  if(grepl("1993", filename)) {
    data$year = "1993"
  }
  if(grepl("1994", filename)) {
    data$year = "1994"
  }
  if(grepl("1995", filename)) {
    data$year = "1995"
  }
  if(grepl("1996", filename)) {
    data$year = "1996"
  }
  if(grepl("1997", filename)) {
    data$year = "1997"
  }
  if(grepl("1998", filename)) {
    data$year = "1998"
  }
  if(grepl("1999", filename)) {
    data$year = "1999"
  }
  data = as.data.frame(data)
  list(filename, data)  
  
})

################################################################################
                  ###COUNTY 1980-1989 TOTAL POPULATION###
################################################################################
data.list.80.county.clean = lapply(county_80.89 , function(file){
  
  data=file[["data"]]
  sheet = file[["sheet"]]
  
  data$year = as.character(data$`Year of Estimate`)
  data$fips = data$`FIPS State and County Codes`
  
  data= subset(data, data$year != "NA") 
  
  data$row_sum = rowSums(data[,c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years",
                                 "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years",
                                 "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over")])
  
  data$total_population = data$row_sum
  data$location= data$fips
   
   data <- data %>%
     select(year, location, total_population)%>%
     group_by(year, location) %>%  
     summarise(value = sum(total_population),
               .groups='drop')
  
  data$outcome = "population"
  data= as.data.frame(data)
  
  list(sheet, data)  
  
})

################################################################################
                  ###COUNTY 1980-1989 DEMOGRAPHICS###
################################################################################

###this may be redundant###
data.list.80.county.demos = lapply(county_80.89 , function(file){
  
  data=file[["data"]]
  sheet = file[["sheet"]]
  
  data$year = as.character(data$`Year of Estimate`)
  data$fips = data$`FIPS State and County Codes`
  
  data= subset(data, data$year != "NA") 
  
  data$location= data$fips
  
  data$outcome = "population"
  data= as.data.frame(data)

  data <- data %>%
    select(-c(`Year of Estimate`, `FIPS State and County Codes`, fips)) %>%
    pivot_longer(cols=c(one_of("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years",
                               "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years",
                               "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over")),
                 names_to = "age",
                 values_to = "value") %>%
    mutate(sex= ifelse(grepl("female", `Race/Sex Indicator`), "female", "male")) %>%
    mutate(race= ifelse(grepl("White", `Race/Sex Indicator`), "White",
                        ifelse(grepl("Black", `Race/Sex Indicator`), "Black", "other race")))%>%
    select(-c(`Race/Sex Indicator`))
  
  data$age = ifelse(data$age == "Under 5 years", "0-4 years", data$age)
  
  data$age = (str_replace(data$age," to ", "-"))
  
  list(sheet, data)  
  
})

################################################################################
                ###COUNTY 1970-1979 POPULATION###
################################################################################
county_70.79_list  <- split(county_70.79, f = county_70.79$V1) #change from dataframe to list of dfs by year#

county_70.79_list_2  <- lapply(county_70.79_list , function(x) {
  list(filename="70s", data=x)
})

county_70.79_list_clean = lapply(county_70.79_list_2, function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#

  data$year = data$V1
  data$fips = data$V2 
  data$race_sex_code = data$V3
  data$"0-4 year olds" = data$V4
  data$"5-9 year olds" = data$V5
  data$"10-14 year olds"= data$V6
  data$"15-19 year olds" = data$V7
  data$"20-24 year olds" = data$V8
  data$"25-29 year olds" = data$V9
  data$"30-34 year olds" = data$V10
  data$"35-39 year olds" = data$V11
  data$"40-44 year olds" = data$V12
  data$"45-49 year olds" = data$V13
  data$"50-54 year olds" = data$V14
  data$"55-59 year olds" = data$V15
  data$"60-64 year olds" = data$V16
  data$"65-69 year olds" = data$V17
  data$"70-74 year olds" = data$V18
  data$"75-79 year olds" = data$V19
  data$"80-84 year olds" = data$V20
  data$"85 years old and older" = data$V21
  
  data$year = as.character(data$year)
  data$location= data$fips
  
  data$total_population = rowSums(data[,c("0-4 year olds", "5-9 year olds", "10-14 year olds", "15-19 year olds", "20-24 year olds", 
                                                             "25-29 year olds", "30-34 year olds", "35-39 year olds", "40-44 year olds", "45-49 year olds",
                                                             "50-54 year olds", "55-59 year olds", "60-64 year olds", "65-69 year olds", "70-74 year olds", 
                                                             "75-79 year olds", "80-84 year olds", "85 years old and older")])

data <- data %>%
  select(year, location, total_population) %>%
  group_by(year, location) %>%  
  summarise(value = sum(total_population),
            .groups='drop')

data$outcome = "population"
data= as.data.frame(data)
  
list(filename, data)  

})

################################################################################
                  ###COUNTY 1970-1979 DEMOGRAPHICS###
################################################################################
county_70.79_list_demos = lapply(county_70.79_list_2, function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#
  
  data$year = data$V1
  data$fips = data$V2 
  data$race_sex_code = data$V3
  data$"0-4 years" = data$V4
  data$"5-9 years" = data$V5
  data$"10-14 years"= data$V6
  data$"15-19 years" = data$V7
  data$"20-24 years" = data$V8
  data$"25-29 years" = data$V9
  data$"30-34 years" = data$V10
  data$"35-39 years" = data$V11
  data$"40-44 years" = data$V12
  data$"45-49 years" = data$V13
  data$"50-54 years" = data$V14
  data$"55-59 years" = data$V15
  data$"60-64 years" = data$V16
  data$"65-69 years" = data$V17
  data$"70-74 years" = data$V18
  data$"75-79 years" = data$V19
  data$"80-84 years" = data$V20
  data$"85 years and over" = data$V21
  
  data$year = as.character(data$year)
  data$location= data$fips
  data$race_sex = race.sex.70s.mappings[data$race_sex]
  
  data$outcome = "population"
  data= as.data.frame(data)
  
  data <- data %>%
    select(-c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, fips, race_sex_code)) %>%
    pivot_longer(cols=c(one_of("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", 
                               "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years",
                               "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", 
                               "75-79 years", "80-84 years", "85 years and over")),
                 names_to = "age",
                 values_to = "value") %>%
    mutate(sex= ifelse(grepl("female", race_sex), "female", "male")) %>%
    mutate(race= ifelse(grepl("White", race_sex), "White",
                        ifelse(grepl("Black", race_sex), "Black", "other race")))%>%
    select(-c(race_sex))
  
  list(filename, data)  
  
})

################################################################################
                  ###PUT INTO CENSUS MANAGER###
################################################################################
#COUNTY TOTAL POPULATION VALUES 1990-1999
county_90_pop = lapply(data.list.county.90.clean, `[[`, 2)

for (data in county_90_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#COUNTY TOTAL POPULATIONS VALUES 1980-1989 
county_80_pop = lapply(data.list.80.county.clean, `[[`, 2)

for (data in county_80_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#COUNTY TOTAL POPULATIONS VALUES 1970-1979 
county_70_pop = lapply(county_70.79_list_clean , `[[`, 2)

for (data in county_70_pop) {  
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

# #COUNTY DEMOGRAPHICS  1980-1989 
# county_80_demo = lapply(data.list.80.county.demos, `[[`, 2)
# 
# for (data in county_80_demo) {
#   
#   census.manager$put.long.form(
#     data = data,
#     ontology.name = 'census',
#     source = 'census',
#     dimension.values = list(),
#     url = 'www.census.gov',
#     details = 'Census Reporting')
# }
# 
# #COUNTY DEMOGRAPHICS  1970-1979
# county_70_demo = lapply(county_70.79_list_demos, `[[`, 2)
# 
# for (data in county_70_demo) {
#   
#   census.manager$put.long.form(
#     data = data,
#     ontology.name = 'census',
#     source = 'census',
#     dimension.values = list(),
#     url = 'www.census.gov',
#     details = 'Census Reporting')
# }
