##Read in National Atlas Plus Data##
DATA.DIR.NATIONAL="../../data_raw/national"
national_files <- Sys.glob(paste0(DATA.DIR.NATIONAL, '/*.csv'))
data.list.national <- lapply(national_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

##Create mappings
outcome.mappings.national = c('HIV diagnoses'='diagnoses',
                     'HIV prevalence' = 'diagnosed.prevalence')

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

age.mappings.national = c('13–19' = '13-19 years',
                 '20–24' = '20-24 years',
                 '25-34' = '25-34 years',
                 '35-44' = '35-44 years',
                 '45-54' = '45-54 years',
                 '55–59' = '55-59 years',
                 '60–64' = '60-64 years',
                 '65–69' = '65-69 years',
                 '70–74' = '70-74 years',
                 '75–79' = '75-79 years',
                 '80–84' = '80-84 years',
                 '85+' = '85+ years')


##Clean Diagnoses and Prevalence National Level Data##

national.clean = lapply(data.list.national, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = outcome.mappings.national[data$Indicator]
  data = data[!is.na(data$outcome),]
  
  names(data)[names(data)=='Year'] = 'year'   
  data$year = substring(data$year,1, 4)                                          
  data$year = as.character(data$year)
  
  data$Cases[data$Cases %in% c("Data suppressed")] = NA    
  data$Cases[data$Cases %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Cases))   
  
  data$location = "US"
  data$age = age.mappings.national[data$Age.Group]
  
  ##Demographic conditionals##

  if(grepl("race", filename)) {
    names(data)[names(data)=='Race.Ethnicity'] = 'race'
    data = subset(data, data$race != 'Multiracial') #Removing multiracial 10-14-24
  }
  if(grepl("male", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  if(grepl("female", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  if(grepl("risk", filename)) {
    data$risk = risk.mappings[data$Transmission.Category]
  }
  
   data <- data %>%
     select(-Indicator, -Geography, -FIPS, -Population, -Rate.per.100000, -Transmission.Category, -Cases)
  
  list(filename, data) 
} )


##Put into Data Manager##

national.clean.put = lapply(national.clean, `[[`, 2)  

for (data in national.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.national',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}