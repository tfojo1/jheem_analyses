library(readxl)
################################################################################
##read in immigration/emigration##
################################################################################

DATA.DIR.MOVEMENT="../../data_raw/movement"

movement_files <- Sys.glob(paste0(DATA.DIR.MOVEMENT, '/*.xlsx'))

data.list.move <- lapply(movement_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
age.mappings.immigration = c('01' = '1-4 years',
                             '02' = '5-17 years',
                             '03' = '18-19 years',
                             '04' = '20-24 years',
                             '05' = '25-29 years',
                             '06' = '30-34 years',
                             '07' = '35-39 years',
                             '08' = '40-44 years',
                             '09' = '45-49 years',
                             '10' = '50-54 years',
                             '11' = '55-59 years',
                             '12' = '60-64 years',
                             '13' = '65-69 years',
                             '14' = '70-74 years',
                             '15' = '75+ years')

race.mappings.immigration = c('02' = 'Black')

ethnicity.mappings.immigration = c( '01' = 'White, Non-Hispanic',
                                    '03' = 'Hispanic or Latino')

################################################################################
#Clean
#BY SEX
#outcome=immigration
#outcome=emigration
################################################################################

data.list.move.clean = lapply(data.list.move, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2011-2015"
  
##TOTAL##
    if(grepl("immigration_total", filename)) {
   data$location = data$`Current Residence Metro Code1`
   data$location = paste("C", data$location, sep=".")
   data$outcome = "immigration"
   
     data <-data %>%
     mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
     select(outcome, year, location, value)
    
    data<- data[!duplicated(data), ]

    }

  if(grepl("emigration_total", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
      data <-data %>%
        mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` ))%>%
        select(outcome, year, location, value)
      
      data<- data[!duplicated(data), ]
  }

  
###BY SEX##
  if(grepl("immigration_sex", filename)) {
   data$location = data$`Current Residence Metro Code1`
   data$location = paste("C", data$location, sep=".")
   data$outcome = "immigration"
   
   data$sex = if_else(data$`Sex Code2` == "01", "male", "female")
   
     data <-data %>%
     mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
     select(outcome, year, location, sex, value)
    
    data<- data[!duplicated(data), ]
  }
  
  if(grepl("emigration_sex", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
    data$sex = if_else(data$`Sex Code2` == "01", "male", "female")
    
      data <-data %>%
        mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` ))%>%
        select(outcome, year, location, sex, value)
      
      data<- data[!duplicated(data), ]
  }
  
  
  ##BY AGE##
  if(grepl("immigration_age", filename)) {
    data$location = data$`Current Residence Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "immigration"
    
    data$`Movers from Different Metropolitan Statistical Area3 Estimate` = if_else (is.na(data$`Movers from Different Metropolitan Statistical Area3 Estimate`), 0, data$`Movers from Different Metropolitan Statistical Area3 Estimate`)
    data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate` = if_else (is.na(data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`), 0, data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`)
    data$`Movers from Abroad4 Estimate` = if_else (is.na(data$`Movers from Abroad4 Estimate`), 0, data$`Movers from Abroad4 Estimate`)
    
    data$age = age.mappings.immigration[data$`Age Group Code`]
    
    data <-data %>%
      mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
      select(outcome, year, location, age, value)
    
    data<- data[!duplicated(data), ]
  }
  
  if(grepl("emigration_age", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
    data$`Movers to Different Metropolitan Statistical Area3 Estimate` = if_else (is.na(data$`Movers to Different Metropolitan Statistical Area3 Estimate`), 0, data$`Movers to Different Metropolitan Statistical Area3 Estimate`)
    data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate` = if_else (is.na(data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate`), 0, data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate`)
    
    data$age = age.mappings.immigration[data$`Age Group Code`]
    
    data <-data %>%
      mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` ))%>%
      select(outcome, year, location, age, value)
    
    data<- data[!duplicated(data), ]
  }
  
  ##Race File, race=black##
  if(grepl("immigration_race", filename)) {
    data$location = data$`Current Residence Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "immigration"
    
    data$`Movers from Different Metropolitan Statistical Area3 Estimate` = if_else (is.na(data$`Movers from Different Metropolitan Statistical Area3 Estimate`), 0, data$`Movers from Different Metropolitan Statistical Area3 Estimate`)
    data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate` = if_else (is.na(data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`), 0, data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`)
    data$`Movers from Abroad4 Estimate` = if_else (is.na(data$`Movers from Abroad4 Estimate`), 0, data$`Movers from Abroad4 Estimate`)
    
    data=subset(data, data$`Race Code`=="02")
    data$race = race.mappings.immigration[data$`Race Code`]
    
    data <-data %>%
      mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
      select(outcome, year, location, race, value)
    
    data<- data[!duplicated(data), ]
  }
  
  if(grepl("emigration_race", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
    data$`Movers to Different Metropolitan Statistical Area3 Estimate` = if_else (is.na(data$`Movers to Different Metropolitan Statistical Area3 Estimate`), 0, data$`Movers to Different Metropolitan Statistical Area3 Estimate`)
    data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate` = if_else (is.na(data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate`), 0, data$`Movers to Elsewhere in the U.S. or Puerto Rico Estimate`)
    
    data=subset(data, data$`Race Code`=="02")
    data$race = race.mappings.immigration[data$`Race Code`]
    
    data <-data %>%
      mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` ))%>%
      select(outcome, year, location, race, value)
    
    data<- data[!duplicated(data), ]
  }
  ##Ethnicity File; eth=white; hispanic##

  if(grepl("immigration_eth", filename)) {
    data$location = data$`Current Residence Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "immigration"
    
    data$`Movers from Different Metropolitan Statistical Area3 Estimate` = if_else (is.na(data$`Movers from Different Metropolitan Statistical Area3 Estimate`), 0, data$`Movers from Different Metropolitan Statistical Area3 Estimate`)
    data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate` = if_else (is.na(data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`), 0, data$`Movers from Elsewhere in the U.S. or Puerto Rico Estimate`)
    data$`Movers from Abroad4 Estimate` = if_else (is.na(data$`Movers from Abroad4 Estimate`), 0, data$`Movers from Abroad4 Estimate`)
    
    data=subset(data, data$`Hispanic Origin Code`!="02")
    data$race = ethnicity.mappings.immigration[data$`Hispanic Origin Code`]
    
    data <-data %>%
      mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
      select(outcome, year, location, race, value)
    
    data<- data[!duplicated(data), ]
  }
  
  if(grepl("emigration_eth", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
    data=subset(data, data$`Hispanic Origin Code`!="02")
    data$race = ethnicity.mappings.immigration[data$`Hispanic Origin Code`]
    
    data <-data %>%
      mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` ))%>%
      select(outcome, year, location, race, value)
    
    data<- data[!duplicated(data), ]
  }
  

  #remove invalid locations?
  data$location_test = locations::get.location.code(data$location, "CBSA")
  data = subset(data, data$location_test != "FALSE")
  data = subset(data, !is.na(data$location_test))
  
  data= as.data.frame(data)
  
  list(filename, data)
  
})

################################################################################
#put
################################################################################

movement_data = lapply(data.list.move.clean, `[[`, 2)  

for (data in movement_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration',
    source = 'census',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}
