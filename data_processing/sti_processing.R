library(jheem2)
library(tidyverse)

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
                    ###Clean STI Data from Atlas Plus###
                       ###Syphilis and Gonorrhea###
################################################################################

data.list.sti.clean = lapply(data.list.sti, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$outcome = tolower(data$Indicator)
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = ifelse(data$outcome == "primary and secondary syphilis", "ps.syphilis", data$outcome)
  
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
  
  ###
  
  if(grepl("total", filename)) {
    data <- data %>%
      select(year, location, outcome, value)
  }
  if(grepl("sex", filename)) {
    data$sex = tolower(data$Sex)
    data <- data %>%
      select(year, location, outcome, sex, value)
  }
  if(grepl("race", filename)) {
    data$race = data$Race.Ethnicity
    data <- data %>%
      select(year, location, outcome, race, value)
  }
  if(grepl("age", filename)) {
    str1= "years"
    data$age = ifelse(data$Age.Group == "Unknown", "Unknown", paste(data$Age.Group, str1, sep= " "))
    data <- data %>%
      select(year, location, outcome, age, value)
  }

  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                      ###Put Data into Data Manager###
################################################################################

sti_data = lapply(data.list.sti.clean, `[[`, 2)

 for (data in sti_data) {

   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc sti',
     source = 'cdc',
     dimension.values = list(),
     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
     details = 'CDC Reporting')
 }
