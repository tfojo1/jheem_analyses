################################################################################
#1980-2001 AIDS diagnoses data by vital status
################################################################################
DATA.DIR.AIDS="../../data_raw/aids_diagnoses"

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

aids.race.mappings = c('American Indian /Alaskan Native' = 'American Indian/Alaska Native',
                      'Asian / Pacific Islander' = 'Asian',
                      'Black (and also not Hispanic)' = 'Black',
                      'Hispanic' = 'Hispanic',
                      'White (and also not Hispanic)' = 'White',
                      'Unknown'= 'Unknown')

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
    data$outcome= "aids.diagnoses.deceased.by.2001"
    
    #Decided 2/2/24: There are old locations that do not map to a current MSA.  Because this is older data I"m going to manually add those in so we have more AIDS data.
    data$location.fixed = ifelse(data$Location == "Greenvile, SC", "Greenville, SC", data$Location) #fixing typo for Greenville, SC
    data$location.fixed = ifelse(data$Location == "Bergen-Passaic, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Middlesex, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Monmouth-Ocean City, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Nassau-Suffolk, NY", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Gary, IN", "Chicago, IL", data$Location)
    data$location.fixed = ifelse(data$Location == "Orange County, CA", "Los Angeles, CA", data$Location)
    data$location.fixed = ifelse(data$Location == "West Palm Beach, FL", "Miami, FL", data$Location)
    
    data$location = locations::get.cbsa.for.msa.name(data$location.fixed)
  }
  if(grepl("all", filename)) {
    data$outcome= "aids.diagnoses"
    
    #Decided 2/2/24: There are old locations that do not map to a current MSA.  Because this is older data I"m going to manually add those in so we have more AIDS data.
    data$location.fixed = ifelse(data$Location == "Greenvile, SC", "Greenville, SC", data$Location) #fixing typo for Greenville, SC
    data$location.fixed = ifelse(data$Location == "Bergen-Passaic, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Middlesex, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Monmouth-Ocean City, NJ", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Nassau-Suffolk, NY", "New York, NY", data$Location)
    data$location.fixed = ifelse(data$Location == "Gary, IN", "Chicago, IL", data$Location)
    data$location.fixed = ifelse(data$Location == "Orange County, CA", "Los Angeles, CA", data$Location)
    data$location.fixed = ifelse(data$Location == "West Palm Beach, FL", "Miami, FL", data$Location)
    
    data$location = locations::get.cbsa.for.msa.name(data$location.fixed)
  }

  ##Demographic conditionals##

  if(grepl("sex", filename)) {
    data$sex = aids.sex.mappings[data$Sex.and.Sexual.Orientation]
  }
  if(grepl("race", filename)) {
    data$race = aids.race.mappings[data$Race.or.Ethnicity]
  }
  if(grepl("age", filename)) {
    data$age = aids.age.mappings[data$Age.at.Diagnosis]
  }
  if(grepl("risk", filename)) {
    data$risk = aids.risk.mappings[data$HIV.Exposure.Category]
  }

  ##
  if(grepl("national", filename)) {
    data$sex = aids.sex.mappings[data$Sex.and.Sexual.Orientation]
    data$race = aids.race.mappings[data$Race.or.Ethnicity]
    data$age = aids.age.mappings[data$Age.at.Diagnosis]
    data$risk = aids.risk.mappings[data$HIV.Exposure.Category]
  }

  data <- data %>%
    select(-Notes, - Cases, - Year.Diagnosed, -Year.Diagnosed.Code)

  data$location = as.character(data$location)

  # data <- data %>%
  #   mutate(location_check = locations::is.location.valid(location))%>%
  #   filter(location_check == "TRUE")%>%
  #   filter(!is.na(location))
  
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
     dimension.values = list(),
     url = 'https://wonder.cdc.gov/AIDSPublic.html',
     details = 'CDC Wonder AIDS Public Information Data')  
 }