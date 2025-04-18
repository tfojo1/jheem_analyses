################################################################################
#1980-2001 AIDS diagnoses data by vital status
################################################################################
DATA.DIR.AIDS="Q:/data_raw/aids_diagnoses"

aids.files <- list.files(DATA.DIR.AIDS, pattern = ".txt", full.names = "TRUE")

data.list.aids <- lapply(aids.files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})
################################################################################
#Set up mappings
################################################################################
aids.sex.mappings = c('Female (any)' = 'female',
                      'Male (bisexual)' = 'male',
                      'Male (heterosexual or pediatric)' = 'male',
                      'Male (homosexual)  or Unknown Classification' = 'male')

aids.risk.mappings = c('Heterosexual contact with HIV' = 'heterosexual',
                       'IV drug use (female and hetero male)' = 'idu',
                       'Male homo/bisexual and IV drug use' = 'msm_idu',
                       'Male homosexual/bisexual contact' = 'msm',
                       'Mother with HIV, or HIV risk' = "other",
                       'Pediatric hemophilia' = "other",
                       'Pediatric receipt of blood' = "other",
                       'Pediatric risk not reported or identified' = "other",
                       'Receipt of blood, blood components or tissue' = "other",
                       'Risk not reported or identified' = "other",
                       'Hemophilia/coagulation disorder'= "other")

aids.race.mappings = c('American Indian /Alaskan Native' = 'american indian/alaska native',
                      'Asian / Pacific Islander' = 'asian',
                      'Black (and also not Hispanic)' = 'black',
                      'Hispanic' = 'hispanic',
                      'White (and also not Hispanic)' = 'white',
                      'Unknown'= 'unknown')

aids.age.mappings = c('Less than 1 Year' = '< 1 year',
                      '1 - 12 Years' = '1-12 years',
                      '13 - 19 Years' = '13-19 years',
                      '20 - 24 Years' = '20-24 years',
                      '25 - 29 Years' = '25-29 years',
                      '30 - 34 Years' = '30-34 years',
                      '35 - 39 Years or age is missing' = '35-39 years', #decided to group missing with 35-39
                      '40 - 44 Years' = '40-44 years',
                      '45 - 49 Years' = '45-49 years',
                      '50 - 54 Years' = '50-54 years',
                      '55 - 59 Years' = '55-59 years',
                      '60 - 64 Years' = '60-64 years',
                      '65+ Years' = '65+ years')

################################################################################
#PROCESSING
################################################################################
#Use alive, deceased, all

aids.data.clean = lapply(data.list.aids, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = data$Year.Diagnosed
  data$year = ifelse(data$year == "Before 1982", "1981", data$year)
  
  data$Cases[data$Cases %in% c("Data suppressed")] = NA    
  data$Cases[data$Cases %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Cases)) 
  
  if(grepl("alive", filename)) {
    data$location = "US"
    data$outcome= "aids.diagnoses.alive.by.2001"
  }
  
  if(grepl("deceased", filename)) {
    data= subset(data, data$Location != "San Juan, PR")
    
    #Decided 2/2/24: There are old locations that do not map to a current MSA.  Because this is older data I"m going to manually add those in so we have more AIDS data.
    data$Location <- gsub("Bergen-Passaic, NJ", "New York, NY",
                             gsub("Greenvile, SC", "Greenville, SC",
                              gsub("Middlesex, NJ", "New York, NY",
                              gsub("Orange County, CA", "Los Angeles, CA",
                              gsub("West Palm Beach, FL", "Miami, FL",
                              gsub("Middlesex, NJ", "New York, NY",
                              gsub("Nassau-Suffolk, NY", "New York, NY",
                              gsub("Monmouth-Ocean City, NJ", "New York, NY",
                              gsub("Gary, IN", "Chicago, IL", data$Location)))))))))

    data$location = locations::get.cbsa.for.msa.name(data$Location)
    data$outcome= "aids.diagnoses.deceased.by.2001"
  }
  
  if(grepl("all", filename)) {
    data= subset(data, data$Location != "San Juan, PR")
    
    #Decided 2/2/24: There are old locations that do not map to a current MSA.  Because this is older data I"m going to manually add those in so we have more AIDS data.
    data$Location <- gsub("Bergen-Passaic, NJ", "New York, NY",
                          gsub("Greenvile, SC", "Greenville, SC",
                          gsub("Middlesex, NJ", "New York, NY",
                          gsub("Orange County, CA", "Los Angeles, CA",
                          gsub("West Palm Beach, FL", "Miami, FL",
                          gsub("Middlesex, NJ", "New York, NY",
                          gsub("Nassau-Suffolk, NY", "New York, NY",
                          gsub("Monmouth-Ocean City, NJ", "New York, NY",
                         gsub("Gary, IN", "Chicago, IL", data$Location)))))))))
    
    data$location = locations::get.cbsa.for.msa.name(data$Location)
    data$outcome= "aids.diagnoses"
    
  }

  ##Demographic conditionals##

  if(grepl("sex", filename)) {
    data$sex = aids.sex.mappings[data$Sex.and.Sexual.Orientation]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, sex)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, sex)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("race", filename)) {
    data$race = aids.race.mappings[data$Race.or.Ethnicity]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, race)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
       select(year, outcome, location, value.adjusted, race)%>%
       rename(value = value.adjusted)
    
    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("age", filename)) {
    data$age = aids.age.mappings[data$Age.at.Diagnosis]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, age)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, age)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("risk", filename)) {
    data$risk = aids.risk.mappings[data$HIV.Exposure.Category]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, risk)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, risk)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("total", filename)) {
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
       select(year, outcome, location, value.adjusted)%>%
       rename(value = value.adjusted)
    
    data<- data[!duplicated(data), ] #This will take out the dup values
  }

  ##
  if(grepl("national", filename)) {
    data$sex = aids.sex.mappings[data$Sex.and.Sexual.Orientation]
    data$race = aids.race.mappings[data$Race.or.Ethnicity]
    data$age = aids.age.mappings[data$Age.at.Diagnosis]
    data$risk = aids.risk.mappings[data$HIV.Exposure.Category]
  }

  data$location = as.character(data$location)
  
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##Put into surveillance manager
################################################################################

 aids.data.clean.put = lapply(aids.data.clean, `[[`, 2)  

 for (data in aids.data.clean.put) {

   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc.aids', 
     source = 'cdc.aids',
     dimension.values.to.distribute = list(race=c('unknown'), risk ='other'),
     url = 'https://wonder.cdc.gov/AIDSPublic.html',
     details = 'CDC Wonder AIDS Public Information Data')  
 }

##############################################################################
#Adding in aids.deaths (this will require a second ontology)
##############################################################################
DATA.DIR.AIDS.DEATHS="Q:/data_raw/aids_diagnoses/aids.deaths"

aids.deaths.files <- list.files(DATA.DIR.AIDS.DEATHS, pattern = ".txt", full.names = "TRUE")

data.list.aids.deaths <- lapply(aids.deaths.files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})
##############################################################################
#Aids.deaths processing (this will require a second ontology)
##############################################################################

aids.deaths.clean = lapply(data.list.aids.deaths, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "1981-2001"
  data$outcome = "aids.deaths"
  
  data$Cases[data$Cases %in% c("Data suppressed")] = NA    
  data$Cases[data$Cases %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Cases)) 
  
  data= subset(data, data$Location != "San Juan, PR")
    
    #Decided 2/2/24: There are old locations that do not map to a current MSA.  Because this is older data I"m going to manually add those in so we have more AIDS data.
    data$Location <- gsub("Bergen-Passaic, NJ", "New York, NY",
                      gsub("Greenvile, SC", "Greenville, SC",
                      gsub("Middlesex, NJ", "New York, NY",
                      gsub("Orange County, CA", "Los Angeles, CA",
                      gsub("West Palm Beach, FL", "Miami, FL", 
                      gsub("Middlesex, NJ", "New York, NY",
                      gsub("Nassau-Suffolk, NY", "New York, NY",
                      gsub("Monmouth-Ocean City, NJ", "New York, NY",
                      gsub("Gary, IN", "Chicago, IL", data$Location)))))))))
    
    data$location = locations::get.cbsa.for.msa.name(data$Location)
  
  ##Demographic conditionals##
  
  if(grepl("sex", filename)) {
    data$sex = aids.sex.mappings[data$Sex.and.Sexual.Orientation]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, sex)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, sex)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("race", filename)) {
    data$race = aids.race.mappings[data$Race.or.Ethnicity]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, race)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, race)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("age", filename)) {
    data$age = aids.age.mappings[data$Age.at.Diagnosis]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, age)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, age)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
  if(grepl("risk", filename)) {
    data$risk = aids.risk.mappings[data$HIV.Exposure.Category]
    #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
    data <- data %>%
      group_by(location, year, risk)%>%
      mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
      select(year, outcome, location, value.adjusted, risk)%>%
      rename(value = value.adjusted)

    data<- data[!duplicated(data), ] #This will take out the dup values
  }
    if(grepl("total", filename)) {
      #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
      data <- data %>%
        group_by(location, year)%>%
        mutate(value.adjusted = sum(value)) %>% #Check that this is right and then rename
        select(year, outcome, location, value.adjusted)%>%
        rename(value = value.adjusted)

      data<- data[!duplicated(data), ] #This will take out the dup values
    }

  data$location = as.character(data$location)
  
  data= as.data.frame(data)
  list(filename, data) 
  
})

##############################################################################
#Put in aids.deaths (this will require a second ontology)
##############################################################################

aids.deaths.put = lapply(aids.deaths.clean, `[[`, 2)

for (data in aids.deaths.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.aids.deaths',
    source = 'cdc.aids',
    dimension.values.to.distribute = list(race=c('unknown'), risk ='other'),
    url = 'https://wonder.cdc.gov/AIDSPublic.html',
    details = 'CDC Wonder AIDS Public Information Data')
}