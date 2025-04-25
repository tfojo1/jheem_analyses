#This code processing data for the following outcomes: diagnosed.prevalence; adap.proportion.of.diagnosed; adap.suppressed.proportion.of.diagnosed

# DIAGNOSED.PREVALENCE ------------------------------------------------------
DATA.DIR.PREVALENCE="Q:/data_raw/prevalence"
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
  }
  if(grepl("allcounty", filename)) {
    data$location = as.character(data$FIPS)
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

#total level
diagnosed.prevalence = as.data.frame.table(data.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location)
adap.clients = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location) 

#Can only pull overlapping years
overlapping.years <- c("2016", "2017", "2018", "2019", "2020", "2021")
states.only <- locations::get.all.for.type('state')

diagnosed.prevalence = filter(diagnosed.prevalence, year %in% overlapping.years)%>% rename(diagnosed.prevalence = Freq)
diagnosed.prevalence= filter(diagnosed.prevalence,location %in% states.only) 

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
  
  #Stratified by sex
  diagnosed.prevalence.sex = as.data.frame.table(data.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__sex)
  adap.clients.sex = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex) 
  
  diagnosed.prevalence.sex = filter(diagnosed.prevalence.sex, year %in% overlapping.years)%>% rename(diagnosed.prevalence = Freq)
  diagnosed.prevalence= filter(diagnosed.prevalence.sex,location %in% states.only) 
  
  adap.clients.sex = filter(adap.clients.sex, year %in% overlapping.years)%>%rename(adap.clients = Freq)
  
  combined.sex = merge(diagnosed.prevalence.sex, adap.clients.sex, by=c("year", "location", "sex"))
  
  adap.proportion.dx.df.sex <- combined.sex %>%
    mutate(value = adap.clients/diagnosed.prevalence)%>%
    mutate(outcome = 'adap.proportion.of.diagnosed')%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(sex = as.character(sex))
  
  data.manager$put.long.form(
    data = adap.proportion.dx.df.sex,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
  
#Stratified by Age
  #Group older ages into 55+ (aligns with cdc ontology)
  diagnosed.prevalence.age = as.data.frame.table(data.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age)
  adap.clients.age = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age) 
  
  diagnosed.prevalence.age = filter(diagnosed.prevalence.age, year %in% overlapping.years)%>% rename(diagnosed.prevalence = Freq)
  diagnosed.prevalence= filter(diagnosed.prevalence.age,location %in% states.only) 
  adap.clients.age = filter(adap.clients.age, year %in% overlapping.years)%>%rename(adap.clients = Freq)
  
  #align age groups
  adap.clients.age <- adap.clients.age%>%
    mutate(adjusted.age = case_when(age == "13-24 years" ~ "13-24 years",
                                    age == "25-34 years" ~ "25-34 years",
                                    age == "35-44 years" ~ "35-44 years",
                                    age == "45-54 years" ~ "45-54 years",
                                    age == "55-64 years" ~ "55+ years",
                                    age == "65+ years" ~ "55+ years"))%>%
    group_by(year, location, adjusted.age)%>%
    mutate(summed.adap.clients = sum(adap.clients))%>%
    select(-age, -adap.clients)%>%
    rename(age = adjusted.age)%>%
    rename(adap.clients = summed.adap.clients)
  
  combined.age = merge(diagnosed.prevalence.age, adap.clients.age, by=c("year", "location", "age"))
  
  adap.proportion.dx.df.age <- combined.age %>%
    mutate(value = adap.clients/diagnosed.prevalence)%>%
    mutate(outcome = 'adap.proportion.of.diagnosed')%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(age = as.character(age))
  
  data.manager$put.long.form(
    data = adap.proportion.dx.df.age,
    ontology.name = 'cdc',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
  
  #Stratified by Race
  #Group races together
  diagnosed.prevalence.race = as.data.frame.table(data.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__race)
  adap.clients.race = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race) 
  
  diagnosed.prevalence.race = filter(diagnosed.prevalence.race, year %in% overlapping.years)%>% rename(diagnosed.prevalence = Freq)
  diagnosed.prevalence= filter(diagnosed.prevalence.race,location %in% states.only) 
  adap.clients.race = filter(adap.clients.race, year %in% overlapping.years)%>%rename(adap.clients = Freq)
  
  #align race groups:
  diagnosed.prevalence.race <- diagnosed.prevalence.race%>%
    mutate(adjusted.race = case_when(race == "american indian/alaska native" ~ "american indian alaska native",
                                     race == "asian" ~ "asian",
                                     race == "black/african american" ~ "black",
                                     race == "hispanic/latino" ~ "hispanic",
                                     race == "native hawaiian/other pacific islander" ~ "native hawaiian pacific islander",
                                     race == "white" ~ "white"))%>%
    select(-race)%>%
    rename(race = adjusted.race)
  
  combined.race = merge(diagnosed.prevalence.race, adap.clients.race, by=c("year", "location", "race"))
  
  adap.proportion.dx.df.race <- combined.race %>%
    mutate(value = adap.clients/diagnosed.prevalence)%>%
    mutate(outcome = 'adap.proportion.of.diagnosed')%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(race = as.character(race))%>%
    filter(value != "Inf")%>%  #removing proportion values greater than one until you hear from Todd
    filter(value <= 1)
  
  data.manager$put.long.form(
    data = adap.proportion.dx.df.race,
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

