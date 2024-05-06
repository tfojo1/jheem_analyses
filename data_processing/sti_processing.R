# library(jheem2)
# library(tidyverse)

################################################################################
                ###Read in STI Data from Atlas Plus###
################################################################################ 

DATA.DIR.STI="../../data_raw/sti"

sti_files <- Sys.glob(paste0(DATA.DIR.STI, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.sti <- lapply(sti_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

################################################################################
              ###Read in early syphilis data###
################################################################################ 

DATA.DIR.EARLY="../../data_raw/sti/early_syphilis"

early_files <- Sys.glob(paste0(DATA.DIR.EARLY, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.early <- lapply(early_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

################################################################################
                    ###Clean STI Data from Atlas Plus###
                       ###Syphilis and Gonorrhea###
                ##Updating to add early and congenital syphilis##
################################################################################

#MAPPINGS#
outcome.mappings.sti = c('Primary and Secondary Syphilis'='ps.syphilis',
                     'Early Non-Primary, Non-Secondary Syphilis' = 'early.syphilis',
                     'Congenital Syphilis' = 'congenital.syphilis',
                     'Gonorrhea' = 'gonorrhea')

age.mappings.sti = c('0-14' = '13-14 years', #decided to make this change on 5-6-24 to align with ontology (assume no on under 13 has STI)
                 '15-19' = '15-19 years',
                 '20-24' = '20-24 years',
                 '25-29' = '25-29 years',
                 '30-34' = '30-34 years',
                 '35-39' = '35-39 years',
                 '40-44' = '40-44 years',
                 '45-54' = '45-54 years',
                 '55-64' = '55-64 years',
                 '65+' = '65+ years',
                 'Unknown' = 'unknown',
                 '15-24'= '15-24 years',
                 '25-34'= '25-34 years',
                 '35-44'= '35-44 years')

####
data.list.sti.clean = lapply(data.list.sti, function(file){

  data=file[["data"]]
  filename = file[["filename"]]

  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))

  data$outcome = outcome.mappings.sti [data$Indicator]

  data <- data %>%
  mutate(value= ifelse(grepl("Data not available", Cases), NA,
                      ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)

  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }

  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("age", filename)) {
    data$age = age.mappings.sti[data$Age.Group]
    data = subset(data, data$age != "unknown")
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }

  data= as.data.frame(data)

  list(filename, data)
})

#Assign names to data list
sti.file.names = file_path_sans_ext(basename(sti_files), compression = FALSE)
names(data.list.sti.clean) = sti.file.names

################################################################################
               ###Clean Early Syphilis Data###
################################################################################

data.list.early.syphilis = lapply(data.list.early, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = outcome.mappings.sti [data$Indicator]
  
  data <- data %>%
    mutate(value= ifelse(grepl("Data not available", Cases), NA,
                         ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("age", filename)) {
    data$age = age.mappings.sti[data$Age.Group]
    data = subset(data, data$age != "unknown")
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
})

################################################################################
                      ###Put Data into Data Manager###
################################################################################

#Create 2 put statements one for the majority of the STI data and then one separate put
#for the early.syphilis data bc the ontology is different bc of the slight change in age groups

sti_data = lapply(data.list.sti.clean, `[[`, 2)

 for (data in sti_data) {

   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc.sti',
     source = 'cdc.sti',
     dimension.values = list(),
     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
     details = 'CDC Reporting')
 }

early_syphilis_data = lapply(data.list.early.syphilis, `[[`, 2)

for (data in early_syphilis_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.syphilis',
    source = 'cdc.sti',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}

################################################################################
##Calculating year over year ratio for STI data (by county only)
################################################################################
# 
# list.for.ratio.calculation <- data.list.sti.clean[grep("county", names(data.list.sti.clean))] 
# 
# county.ratio.total <- list.for.ratio.calculation[grep("total", names(list.for.ratio.calculation))]
# county.ratio.total <- rbind(county.ratio.total [[1]][[2]], county.ratio.total [[2]][[2]]) 
# 
# county.ratio.sex <- list.for.ratio.calculation[grep("sex", names(list.for.ratio.calculation))]
# county.ratio.sex <- rbind(county.ratio.sex [[1]][[2]], county.ratio.sex [[2]][[2]]) 
# 
# county.ratio.race <- list.for.ratio.calculation[grep("race", names(list.for.ratio.calculation))]
# county.ratio.race <- rbind(county.ratio.race [[1]][[2]], county.ratio.race [[2]][[2]]) 
# 
# county.ratio.age <- list.for.ratio.calculation[grep("age", names(list.for.ratio.calculation))]
# county.ratio.age <- rbind(county.ratio.age [[1]][[2]], county.ratio.age [[2]][[2]]) 
# 
# county.ratio.total  <- county.ratio.total %>%
#   mutate(year = as.numeric(year))%>%
#   group_by(location,outcome)%>%
#   arrange(year, .by_group = T)%>%
#   mutate(year.over.year=value/lag(value,1)) %>%
#   filter(!is.na(year.over.year))%>% #Remove rows without a year over year value (so 2018 bc it's the first year)
#   select(outcome, location, year, year.over.year)%>%
#   rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
# 
# county.ratio.sex  <- county.ratio.sex %>%
#   mutate(year = as.numeric(year))%>%
#   filter(!is.na(value))%>% #beacuse this is so stratified there are certain rows where value = NA bc the 'data is not available'
#   group_by(location,outcome, sex)%>%
#   arrange(year, .by_group = T)%>%
#   mutate(year.over.year=value/lag(value,1)) %>%
#   filter(!is.na(year.over.year))%>% #Remove rows without a year over year value (so 2018 bc it's the first year)
#   select(outcome, location, year, sex, year.over.year)%>%
#   rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
# 
# county.ratio.race  <- county.ratio.race %>%
#   mutate(year = as.numeric(year))%>%
#   filter(!is.na(value))%>% 
#   group_by(location,outcome, race)%>%
#   arrange(year, .by_group = T)%>%
#   mutate(year.over.year=value/lag(value,1)) %>%
#   filter(!is.na(year.over.year))%>% 
#   select(outcome, location, year, race, year.over.year)%>%
#   rename(value = year.over.year) 
# 
# county.ratio.age  <- county.ratio.age %>%
#   mutate(year = as.numeric(year))%>%
#   filter(!is.na(value))%>% 
#   group_by(location,outcome, age)%>%
#   arrange(year, .by_group = T)%>%
#   mutate(year.over.year=value/lag(value,1)) %>%
#   filter(!is.na(year.over.year))%>% 
#   select(outcome, location, year, age, year.over.year)%>%
#   rename(value = year.over.year) 
# 
# ##Put into data manager##
# sti.county.ratios = list(
#   "df.total" = county.ratio.total, 
#   "df.age" = county.ratio.sex,
#   "df.race" = county.ratio.race, 
#   "df.age" = county.ratio.age)
# 
# 
# #PUT INTO DATA MANAGER
# for (data in sti.county.ratios) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'cdc.sti',
#     source = 'cdc.sti',
#     dimension.values = list(),
#     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
#     details = 'CDC Reporting')
# }

