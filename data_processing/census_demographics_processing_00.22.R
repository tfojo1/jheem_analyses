################################################################################
###DEMOGRAPHIC MAPPINGS###
################################################################################
year.mappings.00.10 = c('2' = '2000',
                        '3' = '2001',
                        '4' = '2002',
                        '5' = '2003',
                        '6' = '2004',
                        '7' = '2005',
                        '8' = '2006',
                        '9' = '2007',
                        '10' = '2008',
                        '11' = '2009',
                        '13' = '2010')

year.mappings.10.19 = c('3' = '2010',
                        '4' = '2011',
                        '5' = '2012',
                        '6' = '2013',
                        '7' = '2014',
                        '8' = '2015',
                        '9' = '2016',
                        '10' = '2017',
                        '11' = '2018',
                        '12' = '2019')

year.mapping.20.22 = c('2' = '2020',
                       '3' = '2021',
                       '4' = '2022')

age.mappings.10.22 = c('0' = 'Total',
                       '1' = 'Age 0 to 4 years',
                       '2' = 'Age 5 to 9 years',
                       '3' = 'Age 10 to 14 years',
                       '4' = 'Age 15 to 19 years',
                       '5' = 'Age 20 to 24 years',
                       '6' = 'Age 25 to 29 years',
                       '7' = 'Age 30 to 34 years',
                       '8' = 'Age 35 to 39 years',
                       '9' = 'Age 40 to 44 years',
                       '10' = 'Age 45 to 49 years',
                       '11' = 'Age 50 to 54 years',
                       '12' = 'Age 55 to 59 years',
                       '13' = 'Age 60 to 64 years',
                       '14' = 'Age 65 to 69 years',
                       '15' = 'Age 70 to 74 years',
                       '16' = 'Age 75 to 79 years',
                       '17' = 'Age 80 to 84 years',
                       '18' = 'Age 85 years or older')

race.mapping.10.22 = c('WA' = 'White alone',
                       'BA' = 'Black alone',
                       'IA'= 'American Indian and Alaska Native alone',
                       'AA' = 'Asian alone',
                       'NA' = 'Native Hawaiian and Other Pacific Islander alone',
                       
                       'TOM' = 'Two or More Races',
                       
                       'WAC' = 'White alone or in combination',
                       'BAC' = 'Black or African American alone or in combination',
                       'IAC' = 'American Indian and Alaska Native alone or in combination',
                       'AAC' = 'Asian alone or in combination',
                       'NAC' = 'Native Hawaiian and Other Pacific Islander alone or in combination',
                       
                       'NH' = 'Not Hispanic',
                       
                       'NHWA' = 'Not Hispanic, White alone',
                       'NHBA' = 'Not Hispanic, Black or African American alone',
                       'NHIA' = 'Not Hispanic, American Indian and Alaska Native alone',
                       'NHAA' = 'Not Hispanic, Asian alone',
                       'NHNA' = 'Not Hispanic, Native Hawaiian and Other Pacific Islander alone',
                       
                       'NHTOM' = 'Not Hispanic, Two or More Races',
                       
                       'NHWAC' = 'Not Hispanic, White alone or in combination',
                       'NHBAC' = 'Not Hispanic, Black or African American alone or in combination',
                       'NHIAC' = 'Not Hispanic, American Indian and Alaska Native alone or in combination',
                       'NHAAC' = 'Not Hispanic, Asian alone or in combination',
                       'NHNAC' = 'Not Hispanic, Native Hawaiian and Other Pacific Islander alone or in combination',
                       
                       'H' = 'Hispanic',
                       
                       'HWA' = 'Hispanic, White alone',
                       'HBA' = 'Hispanic, Black or African American alone',
                       'HIA' = 'Hispanic, American Indian and Alaska Native alone',
                       'HAA' = 'Hispanic, Asian alone',
                       'HNA' = 'Hispanic, Native Hawaiian and Other Pacific Islander alone',
                       
                       'HTOM' = 'Hispanic, Two or More Races',
                       
                       'HWAC' = 'Hispanic, White alone or in combination',
                       'HBAC' = 'Hispanic, Black or African American alone or in combination',
                       'HIAC' = 'Hispanic, American Indian and Alaska Native alone or in combination',
                       'HAAC' = 'Hispanic, Asian alone or in combination',
                       'HNAC' = 'Hispanic, Native Hawaiian and Other Pacific Islander alone or in combination')

################################################################################
###READ IN County Demographics 2010-2022##
################################################################################
DATA.DIR.CENSUS.DEMOS="../../data_raw/population/demographics_10.22"
DATA.DIR.CENSUS.DEMOS.STATE="../../data_raw/population/county_demographics_bystate_00.10"

census_demo_files_10.22 <- Sys.glob(paste0(DATA.DIR.CENSUS.DEMOS, '/*.csv'))
census_demo_files_00.10 <- Sys.glob(paste0(DATA.DIR.CENSUS.DEMOS.STATE, '/*.csv'))


data.list.demos.10.22 <- lapply(census_demo_files_10.22, function(x){
  list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(STATE="character", COUNTY="character", AGEGRP= "character")))
})

data.list.demos.00.10 <- lapply(census_demo_files_00.10, function(x){
  list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(STATE="character", COUNTY="character", AGEGRP= "character")))
})
################################################################################
###County Demographics 2010-2022##
###FIGURE OUT HOW THIS WILL BE PUT INTO THE MANAGER GIVEN THE POTENTIAL OVERLAP 
##BETWEEN CATEGORIES- DO YOU NEED TO PARSE IT OUT MORE
################################################################################

data.list.demos.10.22.clean = lapply(data.list.demos.10.22, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = paste(data$STATE, data$COUNTY, sep="")
  
  data$age_group = age.mappings.10.22[data$AGEGRP]
  
  if(grepl("10.19", filename)) {
    data= subset(data, data$YEAR != "1")  #REMOVE CENSUS AND USE POP ESTIMATE#
    data= subset(data, data$YEAR != "2")
    data$year = year.mappings.10.19[data$YEAR]
  }
  if(grepl("20.22", filename)) {
    data= subset(data, data$YEAR != "1")  #REMOVE CENSUS AND USE POP ESTIMATE#
    data$year = year.mapping.20.22[data$YEAR]
  }
  
  data <- data %>%
    pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE",
                               "AA_FEMALE", "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE", "WAC_MALE", "WAC_FEMALE", 
                               "BAC_MALE", "BAC_FEMALE", "IAC_MALE", "IAC_FEMALE", "AAC_MALE", "AAC_FEMALE", "NAC_MALE", 
                               "NAC_FEMALE", "NH_MALE", "NH_FEMALE", "NHWA_MALE", "NHWA_FEMALE", "NHBA_MALE", "NHBA_FEMALE", "NHIA_MALE", "NHIA_FEMALE", 
                               "NHAA_MALE", "NHAA_FEMALE", "NHNA_MALE", "NHNA_FEMALE", "NHTOM_MALE", "NHTOM_FEMALE", "NHWAC_MALE", "NHWAC_FEMALE", "NHBAC_MALE", 
                               "NHBAC_FEMALE", "NHIAC_MALE", "NHIAC_FEMALE", "NHAAC_MALE", "NHAAC_FEMALE", "NHNAC_MALE", "NHNAC_FEMALE", "H_MALE", "H_FEMALE",
                               "HWA_MALE", "HWA_FEMALE", "HBA_MALE", "HBA_FEMALE", "HIA_MALE", "HIA_FEMALE", "HAA_MALE", "HAA_FEMALE", "HNA_MALE", 
                               "HNA_FEMALE","HTOM_MALE", "HTOM_FEMALE", "HWAC_MALE", "HWAC_FEMALE", "HBAC_MALE", "HBAC_FEMALE", "HIAC_MALE", "HIAC_FEMALE",
                               "HAAC_MALE", "HAAC_FEMALE", "HNAC_MALE", "HNAC_FEMALE")), 
                 names_to = c("race", "sex"),
                 names_sep = "_",
                 values_to = "value") %>%
    select(-c(SUMLEV, STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, TOT_POP, TOT_MALE, TOT_FEMALE))
  
  data$sex= tolower(data$sex)
  
  data$year = as.character(data$year)
  data$race = race.mapping.10.22[data$race]
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

################################################################################
###County Demographics 2000-2010##
###FIGURE OUT HOW THIS WILL BE PUT INTO THE MANAGER GIVEN THE POTENTIAL OVERLAP 
##BETWEEN CATEGORIES- DO YOU NEED TO PARSE IT OUT MORE
################################################################################

data.list.demos.00.10.clean = lapply(data.list.demos.00.10, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = paste(data$STATE, data$COUNTY, sep="")
  
  data$age_group = year.mappings.00.10[data$AGEGRP]
  
  data= subset(data, data$YEAR != "1")  #REMOVE CENSUS AND USE POP ESTIMATE#
  data= subset(data, data$YEAR != "12")  
  
  data$year = as.character(data$YEAR)
  
  data <- data %>%
    pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE",
                               "AA_FEMALE", "NA_MALE", "NA_FEMALE", "TOM_MALE", "TOM_FEMALE", "NH_MALE", "NH_FEMALE", 
                               "NHWA_MALE", "NHWA_FEMALE", "NHBA_MALE", "NHBA_FEMALE", "NHIA_MALE", "NHIA_FEMALE", 
                               "NHAA_MALE", "NHAA_FEMALE", "NHNA_MALE", "NHNA_FEMALE", "H_MALE", "H_FEMALE",
                               "HWA_MALE", "HWA_FEMALE", "HBA_MALE", "HBA_FEMALE", "HIA_MALE", "HIA_FEMALE", "HAA_MALE", "HAA_FEMALE", "HNA_MALE", 
                               "HNA_FEMALE","HTOM_MALE", "HTOM_FEMALE", "NHTOM_MALE", "NHTOM_FEMALE")), 
                 names_to = c("race", "sex"),
                 names_sep = "_",
                 values_to = "value") %>%
    select(-c(SUMLEV, STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, TOT_POP, TOT_MALE, TOT_FEMALE))
  
  data$sex= tolower(data$sex)
  
  data$race = race.mapping.10.22[data$race]
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

################################################################################
###Put data into Census Manager###
################################################################################

#County DEMOGRAPHICS 2010-2022
demos_10.22 = lapply(data.list.demos.10.22.clean, `[[`, 2)

for (data in county_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#County DEMOGRAPHICS 2000-2010
demos_10.22 = lapply(data.list.demos.00.10.clean, `[[`, 2)

for (data in demos_00.10) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
