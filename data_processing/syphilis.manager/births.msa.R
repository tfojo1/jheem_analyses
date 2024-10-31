#Data for Syphilis Manager

#Request is for births by MSA.  By age and race of mother.  Methods mirror fertility rate (so age 15-44)
#I'm going to put county level birth data and then aggregate to MSA

DATA.DIR.BIRTHS="../../data_raw/syphilis.manager/births"

births.files <- list.files(DATA.DIR.BIRTHS, pattern = ".txt", full.names = "TRUE")

raw.birth.data <- lapply(births.files, function(x) {
  list(filename=x, data=read.delim2(x))
})


# Clean -------------------------------------------------------------------

clean.births.data = lapply(raw.birth.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'births'
  data$year = as.character(data$Year)
  data$age = data$'Age.of.Mother.9'
  data$ethnicity = data$'Mother.s.Hispanic.Origin'
  
  data$Births = ifelse(data$Births == "Not Available", NA, data$Births)
  data$value = as.numeric(data$Births)
  
  data$location= str_pad(data$`County.Code`, width=5, side="left", pad="0")  
  
  #Demographic conditionals#
  if(grepl("bridged.race", filename)) {
    data$race = data$'Mother.s.Bridged.Race'
  }
  
  if(grepl("single.race", filename)) {
    data$race = data$'Mother.s.Single.Race'
    
    #Single race data separates Asian from Native Hawaiian or other Pacific Islander (but for bridged they are combined, so I am combining them here)
    data$race.new = ifelse(data$race == "Asian", "Asian or Pacific Islander", data$race)
    data$race.new = ifelse(data$race.new == "Native Hawaiian or Other Pacific Islander", "Asian or Pacific Islander", data$race.new)
    
    data<-data %>%
      select(-race)%>%
      rename(race = race.new)
  }
  
  data <- data %>%
    select(outcome, year, location, age, race, ethnicity, Births, value)
  
  
  if(grepl("single.race", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, age, ethnicity, race)%>%
      mutate(combined.asian.births = sum(Births))%>%
      select(-value)%>%
      rename(value = combined.asian.births)%>%
      ungroup()
  }
  
  data = as.data.frame(data)
  
  
  data <- data %>%
    mutate(location.check = locations::is.location.valid(location))%>% #Removing counties coded as 'unidentified'
    filter(location.check == T)%>%
    mutate(race = tolower(race))%>%
    mutate(ethnicity = tolower(ethnicity))
  
  list(filename, data) 
  
})

# Put? --------------------------------------------------------------------
clean.births.put = lapply(clean.births.data, `[[`, 2)

for (data in clean.births.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values.to.distribute = list(ethnicity=c('unknown or not stated'), race = c('more than one race')), 
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}

# In data processing main, this outcome will be aggregated from county to MSA.
