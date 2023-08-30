library(jheem2)
library(tidyverse)
library(stringr)
library(haven)

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
################################################################################

mapping_file <- read_sas("~/JHEEM/data_raw/nsduh/substate_county161718.sas7bdat", 
                                  NULL)
substate_regions <- mapping_file$SBST18N

mapping_file$state_code= str_pad(mapping_file$state, width=2, side="left", pad="0")
mapping_file$county_code= str_pad(mapping_file$county, width=3, side="left", pad="0")
mapping_file$FIPS = paste(mapping_file$state_code, mapping_file$county_code, sep="")

mapping_file$state_abbrev = locations::get.location.code(mapping_file$state_code, "STATE")
mapping_file$region_name = mapping_file$SBST18N
mapping_file$matching_var = paste(mapping_file$region_name, mapping_file$state_abbrev, sep=" ")

mapping_file <- mapping_file %>%
  select(matching_var, FIPS)

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
  
  data$estimate = if_else(data$estimate == "suppressed", NA, data$estimate)
  
  data$outcome = tolower(data$outcome)
  
  data <- data%>%
  mutate(state_name = str_extract(geography, paste(state.name, collapse = "|")))%>%
    mutate(state_name= ifelse(grepl("District of Columbia", geography), "District of Columbia", state_name))%>%
    mutate(state_abbrev = state.abb[match(state_name, state.name)]) %>%
    mutate(region_name = str_extract(geography, paste(substate_regions, collapse = "|")))
  
  data$state_name = if_else(data$geography == "United States", "United States", data$state_name)
   
    if(grepl("14.16", filename)) {
      data$year = "2014-2016"
    }
  
  if(grepl("16.18", filename)) {
    data$year = "2016-2018"
    }
  
  data= subset(data, data$region_name != "NA")
  
  data$matching_var = paste(data$region_name, data$state_abbrev, sep=" ")
  
  data = merge(data, mapping_file, by = "matching_var")
  
  data$value = as.numeric(data$estimate)
  data$location = data$FIPS
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                      ###Clean NSDUH Data###
                            ###STATE###
################################################################################



################################################################################
                      ###Clean NSDUH Data###
                          ###NATIONAL###
################################################################################




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

nsduh_state = lapply(data.list.nsduh.region.clean, `[[`, 2)

for (data in nsduh_state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}

nsduh_national = lapply(data.list.nsduh.region.clean, `[[`, 2)

for (data in nsduh_national) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
