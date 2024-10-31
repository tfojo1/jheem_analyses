

# PrEP Data from Atlas Plus -----------------------------------------------

#Atlas Plus Prep data
DATA.DIR.ATLAS.PREP="../../data_raw/prep/atlas_plus"

#---PrEP Data from Atlas Plus---#
atlas_prep_files <- Sys.glob(paste0(DATA.DIR.ATLAS.PREP, '/*.csv'))
#creating a list with sublists of filename, data#
data.list.atlas.prep <- lapply(atlas_prep_files, function(x){
  skip=8
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})


#---Clean prep---#

data.list.clean.atlas.prep = lapply(data.list.atlas.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = "prep"
  
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
    data$race = tolower(data$race)
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
  
} )

#---Clean prep INIDCATIONS---#

data.list.clean.indications = lapply(data.list.atlas.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = "prep.indications"
  
  names(data)[names(data)=='Year'] = 'year'   
  data$year = substring(data$year,1, 4)                                          
  data$year = as.character(data$year)
  
  data = subset(data, data$year=="2018" | data$year == "2017") #Decided 4-18-24 to remove any years after 2018 bc data is all the same
  
  data$Population[data$Population %in% c("Data suppressed")] = NA    
  data$Population[data$Population %in% c("Data not available")] = NA  
  data$value = as.numeric(gsub(",", '', data$Population))   
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name 
    data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
    names(data)[names(data)=='Geography'] = 'state'
    data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
  }
  if(grepl("ehe", filename)) {
    data$location = as.character(data$FIPS)
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
    data$race = tolower(data$race)
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

# Put into Syphilis Manager -----------------------------------------------

##Outcome=prep 
prep_atlas_all = lapply(data.list.clean.atlas.prep, `[[`, 2)  

for (data in prep_atlas_all) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.prep',
    dimension.values.to.distribute = list(race=c('multiracial')),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}

##Outcome=prep.INDICATIONS
indications_all = lapply(data.list.clean.indications, `[[`, 2)  

for (data in indications_all) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.prep.indications',
    dimension.values.to.distribute = list(race=c('multiracial')),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}


# AIDS Vu PrEP Data -------------------------------------------------------


#library(readxl)

################################################################################
###Read in Aids Vu PrEP Excel Datasets###
################################################################################

DATA.DIR.PREP="../../data_raw/prep/aidsvu"

prep_files <- Sys.glob(paste0(DATA.DIR.PREP, '/*.xlsx'))
data.list.prep <- lapply(prep_files, function(x){
  skip=3
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
###Create list- Total PrEP Use###
################################################################################
data.list.prep.total = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Formatting all files will need#
  data$year = data$Year 
  data$outcome = "prep"
  #data= subset(data, data$`State Abbreviation` != "PR") #leaving Puerto Rico in this data#
  
  #Location conditional formatting
  if(grepl("state", filename)) {
    data$location = data$`State Abbreviation`
    
    #Create State Total
    data$count = as.numeric(data$`State PrEP Users`)
    data$count[data$count %in% c("-1")] = NA  #data suppressed#
    data$count[data$count %in% c("-2")] = NA  #data suppressed#
    data$count[data$count %in% c("-4")] = NA  #data not available at county level#
    data$count[data$count %in% c("-8")] = NA  #data undefined#
    data$count[data$count %in% c("-9")] = NA  #data unavailable#
  }
  
  if(grepl("county", filename)) {
    data$FIPS = as.numeric(data$`GEO ID`)
    data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
    
    #Create County total
    data$count = as.numeric(data$`County PrEP Users`)
    data$count[data$count %in% c("-1")] = NA  #data suppressed#
    data$count[data$count %in% c("-2")] = NA  #data suppressed#
    data$count[data$count %in% c("-4")] = NA  #data not available at county level#
    data$count[data$count %in% c("-8")] = NA  #data undefined#
    data$count[data$count %in% c("-9")] = NA  #data unavailable#
  }
  
  data$value = as.numeric(data$count)
  data$year = as.character(data$year)
  
  data= as.data.frame(data)
  
  list(filename, data)
  
})

################################################################################
###Create list- PrEP Use by Sex###
################################################################################
data.list.prep.sex = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Formatting all files will need#
  data$year = data$Year 
  data$outcome = "prep"
  
  #Location conditional formatting
  if(grepl("state", filename)) {
    data$location = data$`State Abbreviation`
  }
  if(grepl("county", filename)) {
    data$FIPS = as.numeric(data$`GEO ID`)
    data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
  }
  data$male=data$`Male PrEP Users`
  data$female =data$`Female PrEP Users` 
  
  data <- data %>%
    pivot_longer(cols=c("male", "female"),
                 names_to = "sex",
                 values_to = "value")
  
  data$value = as.numeric(data$value)
  data$year = as.character(data$year)
  
  data$value[data$value %in% c("-1")] = NA  #data suppressed#
  data$value[data$value %in% c("-2")] = NA  #data suppressed#
  data$value[data$value %in% c("-4")] = NA  #data not available at county level#
  data$value[data$value %in% c("-8")] = NA  #data undefined#
  data$value[data$value %in% c("-9")] = NA  #data unavailable#
  
  data= as.data.frame(data)
  
  list(filename, data)
  
})

################################################################################
###Create list-PrEP Use by Age###
################################################################################

data.list.prep.age = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Formatting all files will need#
  data$year = data$Year 
  data$outcome = "prep"
  
  #Location conditional formatting
  if(grepl("state", filename)) {
    data$location = data$`State Abbreviation`
  }
  
  if(grepl("county", filename)) {
    data$FIPS = as.numeric(data$`GEO ID`)
    data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
  }
  
  data$`13-24 years`= data$`Age LE 24 PrEP Users` #Updating for 2/5/24: decided to change this to 13-24 to align with Atlas bc there are few under 13 using prep
  data$`25-34 years`= data$`Age 25-34 PrEP Users`
  data$`35-44 years` = data$`Age 35-44 PrEP Users`
  data$`45-54 years`= data$`Age 45-54 PrEP Users`
  data$`55+ years` = data$`Age 55+ PrEP Users`
  
  data <- data %>%
    pivot_longer(cols=c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
                 names_to = "age",
                 values_to = "value")
  
  data$value = as.numeric(data$value)
  data$year = as.character(data$year)
  
  data$value[data$value %in% c("-1")] = NA  #data suppressed#
  data$value[data$value %in% c("-2")] = NA  #data suppressed#
  data$value[data$value %in% c("-4")] = NA  #data not available at county level#
  data$value[data$value %in% c("-8")] = NA  #data undefined#
  data$value[data$value %in% c("-9")] = NA  #data unavailable#
  
  data= as.data.frame(data)
  
  list(filename, data)
  
})
################################################################################
###Create list- PrEP Use by Race###
###Note race is not available at the county level###
################################################################################

data.list.prep.state = data.list.prep[11:20] #Subset to just have state level data#

data.list.prep.race = lapply(data.list.prep.state, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Formatting all files will need#
  data$year = data$Year 
  data$outcome = "prep"
  
  #Location conditional formatting
  if(grepl("state", filename)) {
    data$location = data$`State Abbreviation`
    
    data$black=data$`Black PrEP Users`
    data$hispanic =data$`Hispanic PrEP Users`
    data$white =data$`White PrEP Users` 
    
    data <- data %>%
      pivot_longer(cols=c("black", "hispanic", "white"),
                   names_to = "race",
                   values_to = "count")
    
    data$count[data$count %in% c("-1")] = NA  #data suppressed#
    data$count[data$count %in% c("-2")] = NA  #data suppressed#
    data$count[data$count %in% c("-4")] = NA  #data not available at county level#
    data$count[data$count %in% c("-8")] = NA  #data undefined#
    data$count[data$count %in% c("-9")] = NA  #data unavailable#
    
    data$value = as.numeric(data$count)
    data$year = as.character(data$year)
    
    data= as.data.frame(data)
  }
  
  list(filename, data)
  
})

################################################################################
###Put AIDS Vu Files into Data.Manager###
################################################################################

##Total PrEP Use State + County##

prep_total = lapply(data.list.prep.total, `[[`, 2) 

for (data in prep_total) {
  
  data.manager$put.long.form(
    data = data,
    outcome= "prep", 
    ontology.name = 'aidsvu', 
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'AIDS Vu Reporting')
}

##Total PrEP Use Sex##

prep_sex = lapply(data.list.prep.sex, `[[`, 2)  

for (data in prep_sex) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'aidsvu',
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'AIDS Vu Reporting')
}

##Total PrEP Use Age##

prep_age = lapply(data.list.prep.age, `[[`, 2)  

for (data in prep_age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'aidsvu',
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'AIDS Vu Reporting')
}

##Total PrEP Use Race##

prep_race = lapply(data.list.prep.race, `[[`, 2)  

for (data in prep_race) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'aidsvu',
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'AIDS Vu Reporting')
}

