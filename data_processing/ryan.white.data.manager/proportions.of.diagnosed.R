#This code processing data for the following outcomes: diagnosed.prevalence; adap.proportion.of.diagnosed; adap.suppressed.proportion.of.diagnosed

# DIAGNOSED.PREVALENCE ------------------------------------------------------
DATA.DIR.PREVALENCE="../../data_raw/prevalence"
prevalence_files <- Sys.glob(paste0(DATA.DIR.PREVALENCE, '/*.csv'))
data.list.prevalence <- lapply(prevalence_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

outcome.mappings = c('HIV diagnoses'='diagnoses',
                     'HIV prevalence' = 'diagnosed.prevalence',
                     'Linkage to HIV care' = 'linkage_1mo',
                     'Receipt of HIV medical care' = 'engagement',
                     'HIV viral suppression' = 'suppression',
                     'Knowledge of Status' = 'awareness')

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

age.mappings = c('13-24' = '13-24 years',
                 '25-34' = '25-34 years',
                 '35-44' = '35-44 years',
                 '45-54' = '45-54 years',
                 '55+' = '55+ years')

national.age.mappings = c('13-24' = '13-24 years',
                          '25-34' = '25-34 years',
                          '35-44' = '35-44 years',
                          '45-54' = '45-54 years',
                          '55-64' = '55-64 years',
                          '65+' = '65+ years')

data.list.clean.prevalence = lapply(data.list.prevalence, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = outcome.mappings[data$Indicator]
  data = data[!is.na(data$outcome),]
  
  names(data)[names(data)=='Year'] = 'year'   
  data$year = substring(data$year,1, 4)                                          
  data$year = as.character(data$year)
  
  data$Cases[data$Cases %in% c("Data suppressed")] = NA    
  data$Cases[data$Cases %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Cases))   
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name 
    data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
    names(data)[names(data)=='Geography'] = 'state'
    data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state])
    
    data$state.data = T
  }
  if(grepl("ehe", filename)) {
    data$location = as.character(data$FIPS)
    
    data$state.data = F
  }
  if(grepl("msa", filename)) {
    data$msa_indicator= substring(data$FIPS, 6, 10)
    
    data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
    data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
    
    data$cbsa = substring(data$FIPS, 1, 5)
    data$location = paste("C", data$cbsa, sep=".")
    
    data$state.data = F
  }
  if(grepl("allcounty", filename)) {
    data$location = as.character(data$FIPS)
    
    data$state.data = F
  } 
  
  if(grepl("age", filename)) {
    data$age = age.mappings[data$Age.Group]
  }
  if(grepl("race", filename)) {
    names(data)[names(data)=='Race.Ethnicity'] = 'race'
    data$race= tolower(data$race)
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
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
  
  data = subset(data, data$state.data == T) #only pull state data bc that is what we have for adap
  
  list(filename, data) 
})

prevalence_all = lapply(data.list.clean.prevalence, `[[`, 2) 

for (data in prevalence_all) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values.to.distribute = list(race=c('multiracial')),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}


# ADAP.PROPORTION.OF.DIAGNOSED --------------------------------------------
#Formula is (adap.clients/diagnosed.prevalence)
diagnosed.prevalence = as.data.frame.table(data.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location)
adap.clients = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location) 

#Can only pull overlapping years
overlapping.years <- c("2016", "2017", "2018", "2019", "2020", "2021")

diagnosed.prevalence = filter(diagnosed.prevalence, year %in% overlapping.years)%>% rename(diagnosed.prevalence = Freq)
adap.clients = filter(adap.clients, year %in% overlapping.years)%>%rename(adap.clients = Freq)

combined = merge(diagnosed.prevalence, adap.clients, by=c("year", "location"))

adap.proportion.dx.df <- combined %>%
  mutate(value = adap.clients/diagnosed.prevalence)%>%
  mutate(outcome = 'adap.proportion.of.diagnosed')%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))

  data.manager$put.long.form(
    data = adap.proportion.dx.df,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')


# ADAP.SUPPRESSED.PROPORTION.OF.DIAGNOSED ---------------------------------
#Formula is (adap.clients * adap.suppression) / diagnosed.prevalence

adap.suppression = as.data.frame.table(data.manager$data$adap.suppression$estimate$nastad.adap$ryan.white.pdfs$year__location) 
  
adap.suppression = filter(adap.suppression, year %in% overlapping.years)%>%rename(adap.suppression = Freq)
  
combined.two = merge(combined, adap.suppression, by=c("year", "location"))

adap.suppression.df <- combined.two %>%
  mutate(numerator = (adap.clients * adap.suppression))%>%
  mutate(value = numerator/diagnosed.prevalence)%>%
  mutate(outcome = 'adap.suppressed.proportion.of.diagnosed')%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))

data.manager$put.long.form(
  data = adap.suppression.df,
  ontology.name = 'ryan.white.pdfs',
  source = 'ryan.white.program',
  url = 'https://ryanwhite.hrsa.gov/data/reports',
  details = 'Ryan White Downloaded PDF Reports')

