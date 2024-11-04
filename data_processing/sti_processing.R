
#  Syphilis (2000-2022) -----------------------------------
DATA.DIR.SYPHILIS="../../data_raw/syphilis.manager/syphilis"
syphilis_files <- Sys.glob(paste0(DATA.DIR.SYPHILIS, '/*.csv'))
syphilis.data <- lapply(syphilis_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

# Mappings ----------------------------------------------------------------

outcome.mappings.syphilis = c('Primary and Secondary Syphilis'='ps.syphilis',
                              'Early Non-Primary, Non-Secondary Syphilis' = 'early.syphilis',
                              'Congenital Syphilis' = 'congenital.syphilis',
                              'Unknown Duration or Late Syphilis' = 'unknown.duration.or.late.syphilis')

syphilis.mappings.age = c('0-14' = '13-14 years',  #decided to make this change on 5-6-24 to align with ontology (assume no on under 13 has STI)
                          '15-19' = '15-19 years',
                          '20-24' = '20-24 years',
                          '25-29' = '25-29 years',
                          '30-34' = '30-34 years',
                          '35-39' = '35-39 years',
                          '40-44' = '40-44 years',
                          '45-54' = '45-54 years',
                          '55-64' = '55-64 years',
                          '65+' = '65+ years',
                          'Unknown' = 'Unknown')

# Cleaning ----------------------------------------------------------------

syphilis.clean = lapply(syphilis.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = outcome.mappings.syphilis[data$Indicator]
  
  data <- data %>%
    mutate(value= ifelse(Cases == "Data not available" | Cases == "Data suppressed", NA, Cases))
  
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("agegrp", filename)) {
    data$age = syphilis.mappings.age[data$Age.Group]
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
    data$race = tolower(data$race)
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
})

# Put Syphilis---------------------------------------------------------------------

syphilis.clean.put = lapply(syphilis.clean, `[[`, 2)

for (data in syphilis.clean.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values.to.distribute = list(race=c('Multiracial', 'Unknown'), age = 'Unknown'), 
    url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
    details = 'CDC Atlas Plus')
}



# Clean + Put Early Syphilis because age categories are different ---------

DATA.DIR.SYPHILIS.EARLY="../../data_raw/syphilis.manager/syphilis/early.syphilis"
syphilis_files_early <- Sys.glob(paste0(DATA.DIR.SYPHILIS.EARLY, '/*.csv'))
syphilis.data.early <- lapply(syphilis_files_early, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

early.syphilis.age = c('0-14' = '13-14 years',  #decided to make this change on 5-6-24 to align with ontology (assume no on under 13 has STI)
                       '15-24' = '15-24 years',
                       '25-34' = '25-34 years',
                       '35-44' = '35-44 years',
                       '45-54' = '45-54 years',
                       '55-64' = '55-64 years',
                       '65+' = '65+ years',
                       'Unknown' = 'Unknown')



# Early Spyhilis Cleaning ----------------------------------------------------------------

syphilis.clean.early = lapply(syphilis.data.early, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = outcome.mappings.syphilis[data$Indicator]
  
  data <- data %>%
    mutate(value= ifelse(Cases == "Data not available" | Cases == "Data suppressed", NA, Cases))
  
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("agegrp", filename)) {
    data$age = early.syphilis.age[data$Age.Group]
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
    data$race = tolower(data$race)
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
})


# Early Syphilis Put ------------------------------------------------------

early.syphilis.put = lapply(syphilis.clean.early, `[[`, 2)

for (data in early.syphilis.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.syphilis',
    source = 'cdc.sti',
    dimension.values.to.distribute = list(race=c('Multiracial', 'Unknown'), age = 'Unknown'), 
    url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
    details = 'CDC Atlas Plus')
}


# Gonorrhea Data ----------------------------------------------------------

DATA.DIR.GC="../../data_raw/gonorrhea"
gonorrhea_files <- Sys.glob(paste0(DATA.DIR.GC, '/*.csv'))
gonorrhea.data <- lapply(gonorrhea_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

gonorrhea.clean = lapply(gonorrhea.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = "gonorrhea"
  
  data <- data %>%
    mutate(value= ifelse(Cases == "Data not available" | Cases == "Data suppressed", NA, Cases))
  
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
    data$age = syphilis.mappings.age[data$Age.Group]
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
    data$race = tolower(data$race)
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
})

# Put Gonorrhea -----------------------------------------------------------
gonorrhea.clean.put = lapply(gonorrhea.clean, `[[`, 2)

for (data in gonorrhea.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values.to.distribute = list(race=c('Multiracial', 'Unknown'), age = 'Unknown'),  
    url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
    details = 'CDC Atlas Plus')
}
