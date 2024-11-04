#library(readxl)
################################################################################
##read in immigration/emigration##
################################################################################

DATA.DIR.MOVEMENT="../../data_raw/movement"

movement_files <- Sys.glob(paste0(DATA.DIR.MOVEMENT, '/*.xlsx'))

data.list.move <- lapply(movement_files, function(x){
  skip=1
  list(filename=file_path_sans_ext(basename(x), compression=FALSE), data=read_excel(x, sheet= 1, skip=skip))
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

race.mappings.immigration = c('02' = 'black')

ethnicity.mappings.immigration = c( '01' = 'white, non-hispanic',
                                    '03' = 'hispanic or latino')

################################################################################
#Clean
#BY SEX
#outcome=immigration
#outcome=emigration
################################################################################

data.list.move.clean = lapply(data.list.move, function(file){
  
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("11.15", filename)) {
    data$year = "2011-2015"
  }
  if(grepl("12.16", filename)) {
    data$year = "2012-2016"
  }
  if(grepl("13.17", filename)) {
    data$year = "2013-2017"
  }
  if(grepl("14.18", filename)) {
    data$year = "2014-2018"
  }
  if(grepl("15.19", filename)) {
    data$year = "2015-2019"
  }
  if(grepl("16.20", filename)) {
    data$year = "2016-2020"
  }
  
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
  
  #Removing any negative values- this was relevant for PR msas only#
  data = subset(data, data$value >0)
  
  data= as.data.frame(data)
  
  data <- data %>%
    select(-location_test)
  
  list(filename, data)
  
})

###############################################################################
#Calculate the 'other' race category
#Pulling the individual variables from the other dfs to make one df
#Note that 2011-2015 is the only stratified immigration data so this only applies to datasets from that year range
###############################################################################
#Pulled from cdc wonder
#Proportion of Black who are Hispanic: 14,568,073 / 218,869,956 = 0.067
#Proportion of Hispanic who are Black:  14,568,073 / 269,697,533 = 0.054
###############################################################################

prop.black.hisp <- 0.067
prop.hisp.black <-0.054

#SET UP FILES, assign file names
movement.file.names = file_path_sans_ext(basename(movement_files), compression = FALSE)
names(data.list.move.clean) = movement.file.names

##IMMIGRATION##
immigration_total <- data.list.move.clean[grep("immigration_total", names(data.list.move.clean))] 
immigration_total <- as.data.frame(immigration_total$msa_immigration_total_11.15)

immigration_total <- immigration_total %>%
  rename(total = value)

imm_black <- data.list.move.clean[grep("immigration_race", names(data.list.move.clean))] 
imm_black <- as.data.frame(imm_black$msa_immigration_race_11.15) 


imm_black <- imm_black%>%
  rename(black = race)%>%
  rename(black.value = value)%>%
  select(-year, -outcome)

imm_hisp <- data.list.move.clean[grep("immigration_eth", names(data.list.move.clean))] 
imm_hisp <- as.data.frame(imm_hisp$msa_immigration_eth_11.15) 

imm_hisp <- imm_hisp %>%
  filter(race == 'hispanic or latino')%>%
  rename(hispanic = race)%>%
  rename(hispanic.value = value)%>%
  select(-year, -outcome)

imm_combo_1 <- merge(imm_hisp, imm_black, by="location")
imm_combo <- merge(imm_combo_1, immigration_total, by="location")

imm_combo <- imm_combo %>%
  select(location, total, outcome, year, hispanic.value, black.value)%>% 
  mutate(black.nh = black.value-(sqrt(prop.black.hisp*prop.hisp.black*hispanic.value*black.value)))%>% 
  mutate(other.race = (total - (hispanic.value + black.nh)))

##EMIGRATION

emigration_total <- data.list.move.clean[grep("emigration_total", names(data.list.move.clean))] 
emigration_total <- as.data.frame(emigration_total$msa_emigration_total_11.15)

emigration_total <- emigration_total %>%
  rename(total = value)

em_black <- data.list.move.clean[grep("emigration_race", names(data.list.move.clean))] 
em_black <- as.data.frame(em_black$msa_emigration_race_11.15) 

em_black <- em_black%>%
  rename(black = race)%>%
  rename(black.value = value)

em_hisp <- data.list.move.clean[grep("emigration_eth", names(data.list.move.clean))] 
em_hisp <- as.data.frame(em_hisp$msa_emigration_eth_11.15) 

em_hisp <- em_hisp %>%
  filter(race == 'hispanic or latino')%>%
  rename(hispanic = race)%>%
  rename(hispanic.value = value)

em_combo <- merge(em_hisp, em_black, by="location")
em_combo <- merge(em_combo, emigration_total, by="location")

em_combo <- em_combo %>%
  select(location, total, outcome, year, hispanic.value, black.value)%>% #add outcome and year back
  mutate(black.nh = black.value-(sqrt(prop.black.hisp*prop.hisp.black*hispanic.value*black.value)))%>% 
  mutate(other.race = total - (hispanic.value + black.nh))    #I didn't put white here but I did above

######
#Reformat for put

imm_combo <- imm_combo %>%
  rename(value = other.race)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "other")%>%
  filter(value > 0)

em_combo <- em_combo %>%
  rename(value = other.race)%>%
  select(location, outcome, year, value)%>%
  mutate(race = "other")%>%
  filter(value > 0)

other_race <- list(imm_combo, em_combo)

#Assign file names for other race to be used in immigration_age_calc code
other.race.file.names = c("other.race.immigration", "other.race.emigration")
names(other_race) = other.race.file.names


# 10-15-24: Updating Put to Distribute Other Race -------------------------

movement_data = lapply(data.list.move.clean, `[[`, 2) 

movement_data[[length(movement_data)+1]]<-imm_combo

movement_data[[length(movement_data)+1]]<-em_combo

for (data in movement_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}


