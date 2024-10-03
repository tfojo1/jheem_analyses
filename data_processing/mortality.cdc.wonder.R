
# Mortality Rate data from CDC Wonder -------------------------------------

DATA.DIR.MORTALITY="../../data_raw/syphilis.manager/mortality.rates"

mortality.files <- list.files(DATA.DIR.MORTALITY, pattern = ".txt", full.names = "TRUE")

mortality.rates.raw <- lapply(mortality.files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})

# Clean -------------------------------------------------------------------

##NOTE NEED TO GROUP TOGETHER ASIAN AND PACIFIC ISLANDER FOR 2018-2022 DATA
#REMOVE AGE IS NOT STATEd


mortality.rates.clean = lapply(mortality.rates.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'mortality.rate'
  data$year = as.character(data$Year)
  data$sex = tolower(data$Gender)
  data$age = data$'Ten.Year.Age.Groups'
  data$ethnicity = data$'Hispanic.Origin'
  data$location = "US"
  data$value = "Crude.Rate"
  
  
  data$value = ifelse(data$value == "Unreliable", NA, data$value) #Remove suppressed or missing mortality rates
  data$value = ifelse(data$value == "Not Applicable", NA, data$value)
  data$value = as.numeric(data$value)

  
  #Reconile Bridged vs Single Race
  if(grepl("99.17", filename)) { #1999-2017 is bridged race
    data$race = data$Race
  }
  
  if(grepl("18.22", filename)) { #2018-2022 is single race
    data$race = data$'Single.Race.6'
    
    #Single race data separates Asian from Native Hawaiian or other Pacific Islander (but for bridged they are combined, so I am combining them here)
    data$race.new = ifelse(data$race == "Asian", "Asian or Pacific Islander", data$race)
    data$race.new = ifelse(data$race.new == "Native Hawaiian or Other Pacific Islander", "Asian or Pacific Islander", data$race.new)
    
    data<-data %>%
      select(-race)%>%
      rename(race = race.new)
  }
  
  data <- data %>%
    select(outcome, year, location, sex, age, race, ethnicity, value)
  
  
  if(grepl("18.22", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    #I think you may have to re-calc this
    data <- data %>%
      group_by(year, location, sex, age, ethnicity, race)%>%
      mutate(combined.asian.births = sum(Births))%>%
      mutate(combined.asian.female.pop = sum(Female.Population))%>%
      mutate(combined.asian.rate = round(((combined.asian.births/combined.asian.female.pop)*1000), digits = 2))%>%
      select(-value)%>%
      rename(value = combined.asian.rate)%>%
      ungroup()
  }
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})



# Put into Syphilis Manager -----------------------------------------------

#Combine
fertility.data.list = list(
  fertility.rate.data,
  female.population.data)

#for (data in fertility.data.list) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.deaths',
    source = 'cdc.wonder.mortality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html',
    details = 'CDC Wonder Underlying Cause of Death')
}
