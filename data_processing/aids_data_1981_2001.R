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
                      'Male (homosexual) or Unknown Classification' = 'male')


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

aids.age.mappings = c('Less than 1 year old' = '< 1 year',
                      '1 to 12 years old' = '1-12 years',
                      '13 to 19 years old' = '13-19 years',
                      '20 to 24 years old' = '20-24 years',
                      '25 to 29 years old' = '25-29 years',
                      '30 to 34 years old' = '30-34 years',
                      '35 to 39 years old or age is missing' = '35-39 years',
                      '40 to 44 years old' = '40-44 years',
                      '45 to 49 years old' = '45-49 years',
                      '50 to 54 years old' = '50-54 years',
                      '55 to 59 years old' = '55-59 years',
                      '60 to 64 years old' = '60-64 years',
                      '65 years old or older' = '65+ years')

################################################################################
#PROCESSING
################################################################################
#Use alive, deceased, all

aids.data.clean = lapply(data.list.aids, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = data$Year.Diagnosed
  
  data$Cases[data$Cases %in% c("Data suppressed")] = NA    
  data$Cases[data$Cases %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Cases))   

  
  if(grepl("alive", filename)) {
    data$location = "US"
  }
  if(grepl("deceased", filename)) {
    data$location = locations::get.cbsa.for.msa.name(data$Location)
  }
  if(grepl("all", filename)) {
    data$location = locations::get.cbsa.for.msa.name(data$Location)
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

  
  list(filename, data) 
})

################################################################################
##Put into surveillance manager
################################################################################


 aids.data.clean.put = lapply(aids.data.clean, `[[`, 2)  
 
 for (data in aids.data.clean.put) {
   
   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc.national', #UPDATE
     source = 'cdc.hiv', #UPDATE
     dimension.values = list(),
     url = 'https://wonder.cdc.gov/AIDSPublic.html',
     details = 'CDC Atlas Plus data')  #UPDATE
 }