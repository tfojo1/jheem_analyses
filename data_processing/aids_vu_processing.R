
library(readxl)

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
  
  data$`under 25 years`= data$`Age LE 24 PrEP Users`
  data$`25-34 years`= data$`Age 25-34 PrEP Users`
  data$`35-44 years` = data$`Age 35-44 PrEP Users`
  data$`45-54 years`= data$`Age 45-54 PrEP Users`
  data$`55+ years` = data$`Age 55+ PrEP Users`

  data <- data %>%
   pivot_longer(cols=c("under 25 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
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
