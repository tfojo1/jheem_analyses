# library(jheem2)
# library(tidyverse)
# library(stringr)
# library(haven)

################################################################################
                  ###Read in NSDUH Data###
################################################################################ 
DATA.DIR.NSDUH="../../data_raw/nsduh"

nsduh_files <- Sys.glob(paste0(DATA.DIR.NSDUH, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.nsduh <- lapply(nsduh_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
################################################################################
      ###Read in Mapping File for NSDUH "Regions"###
      ###Commenting out bc Jeff is working on this piece in the locations package###
################################################################################
# 
# mapping_file <- read_sas("~/JHEEM/data_raw/nsduh/substate_county161718.sas7bdat", 
#                                   NULL)
# substate_regions <- mapping_file$SBST18N
# 
# mapping_file$state_code= str_pad(mapping_file$state, width=2, side="left", pad="0")
# mapping_file$county_code= str_pad(mapping_file$county, width=3, side="left", pad="0")
# mapping_file$FIPS = paste(mapping_file$state_code, mapping_file$county_code, sep="")
# 
# mapping_file$state_abbrev = locations::get.location.code(mapping_file$state_code, "STATE")
# mapping_file$region_name = mapping_file$SBST18N
# mapping_file$matching_var = paste(mapping_file$region_name, mapping_file$state_abbrev, sep=" ")
# 
# mapping_file <- mapping_file %>%
#   select(matching_var, FIPS)

################################################################################
                       ###Clean NSDUH Data###
                          ###REGION###
################################################################################
data.list.nsduh.region.clean = lapply(data.list.nsduh, function(file){

  data=file[["data"]]
  filename = file[["filename"]]

  data= subset(data, data$geography != "South")
  data= subset(data, data$geography != "West")
  data= subset(data, data$geography != "Midwest")
  data= subset(data, data$geography != "Northeast")
  data= subset(data, data$geography != "United States")

  data$estimate = if_else(data$estimate == "suppressed", NA, data$estimate)

  if(grepl("heroin", filename)) {
    data$outcome = "heroin"
  }

  if(grepl("cocaine", filename)) {
    data$outcome = "cocaine"
  }
  
  #Create state abbreviations to get regions by filtering by those w/o a state abbrev
  data <- data%>%
    mutate(state_abbrev = state.abb[match(geography, state.name)])%>%
    mutate(state_abbrev = if_else(geography == "District of Columbia", "DC", state_abbrev ))%>%
    filter(is.na(state_abbrev))

   if(grepl("02.04", filename)) {
    data$year = "2002-2004"
  }
  if(grepl("04.06", filename)) {
    data$year = "2004-2006"
  }
  if(grepl("06.08", filename)) {
    data$year = "2006-2008"
  }
  if(grepl("08.10", filename)) {
    data$year = "2008-2010"
  }
  if(grepl("10.12", filename)) {
    data$year = "2010-2012"
  }
  if(grepl("12.14", filename)) {
    data$year = "2014-2016"
  }
   if(grepl("14.16", filename)) {
      data$year = "2014-2016"
    }
  if(grepl("16.18", filename)) {
    data$year = "2016-2018"
    }

  data$location = locations::get.location.code(data$geography, "NSDUH")
  data$location = as.character(data$location)
  data$value = as.numeric(data$estimate)
  data$age = data$age_group
  
  #I think you need to take out NA values from values col in order to put into manager#
  data = subset(data, !is.na(data$value))
  #I'm going to remove locations that are NA
  data = subset(data, !is.na(data$location))

      data <- data %>%
        select(year, outcome, value, location, age)

  data= as.data.frame(data)

  list(filename, data)
})
################################################################################
                      ###Clean NSDUH Data###
                            ###STATE###
################################################################################

data.list.nsduh.state.clean = lapply(data.list.nsduh, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data= subset(data, data$geography != "South")
  data= subset(data, data$geography != "West")
  data= subset(data, data$geography != "Midwest")
  data= subset(data, data$geography != "Northeast")
  data= subset(data, data$geography != "United States")
  
  data$estimate = if_else(data$estimate == "suppressed", NA, data$estimate)
  
    if(grepl("heroin", filename)) {
      data$outcome = "heroin"
    }
    if(grepl("cocaine", filename)) {
      data$outcome = "cocaine"
    }
  
  data <- data%>%
    mutate(state = state.abb[match(geography, state.name)])%>%
    mutate(location = ifelse(geography == "District of Columbia", "DC", state))

    if(grepl("02.04", filename)) {
      data$year = "2002-2004"
    }
    if(grepl("04.06", filename)) {
      data$year = "2004-2006"
    }
    if(grepl("06.08", filename)) {
      data$year = "2006-2008"
    }
    if(grepl("08.10", filename)) {
      data$year = "2008-2010"
    }
    if(grepl("10.12", filename)) {
      data$year = "2010-2012"
    }
    if(grepl("12.14", filename)) {
      data$year = "2014-2016"
    }
    if(grepl("14.16", filename)) {
        data$year = "2014-2016"
      }
    if(grepl("16.18", filename)) {
      data$year = "2016-2018"
      }

  data$value = as.numeric(data$estimate)
  data$age = data$age_group
   
  data= subset(data, !is.na(data$location)) #remove any location that isn't a state
  
   data <- data %>%
     select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                      ###Clean NSDUH Data###
                          ###NATIONAL###
################################################################################

data.list.nsduh.national.clean = lapply(data.list.nsduh, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data= subset(data, data$geography == "United States")

  data$estimate = if_else(data$estimate == "suppressed", NA, data$estimate)
  
  if(grepl("heroin", filename)) {
    data$outcome = "heroin"
  }
  if(grepl("cocaine", filename)) {
    data$outcome = "cocaine"
  }
  if(grepl("02.04", filename)) {
    data$year = "2002-2004"
  }
  if(grepl("04.06", filename)) {
    data$year = "2004-2006"
  }
  if(grepl("06.08", filename)) {
    data$year = "2006-2008"
  }
  if(grepl("08.10", filename)) {
    data$year = "2008-2010"
  }
  if(grepl("10.12", filename)) {
    data$year = "2010-2012"
  }
  if(grepl("12.14", filename)) {
    data$year = "2014-2016"
  }
  if(grepl("14.16", filename)) {
    data$year = "2014-2016"
  }
  if(grepl("16.18", filename)) {
    data$year = "2016-2018"
  }
  
  data$value = as.numeric(data$estimate)
  data$location = "US"
  data$age = data$age_group
  
   data <- data %>%
     select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                    ###Put Data into Data Manager###
################################################################################
nsduh_region = lapply(data.list.nsduh.region.clean, `[[`, 2)

for (data in nsduh_region) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}

nsduh_state = lapply(data.list.nsduh.state.clean, `[[`, 2)

for (data in nsduh_state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}

nsduh_national = lapply(data.list.nsduh.national.clean, `[[`, 2)

for (data in nsduh_national) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
