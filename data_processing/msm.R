
################################################################################
                      ##Create Proportion of MSM##
              ##Note this code pulls from the BRFSS dataset created in the BRFSS weighted state code
                      ##This is a weighted measure
                ##MSM Estimation using BRFSS Data by State
################################################################################
#######
##Total
#######
#Pulling from the brffs.state.sex data list so that denominator for msm proportion is males only
data.list.brfss.state.msm = lapply(data.list.brfss.state.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location, msm) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>%  
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##Prior to calculating MSM proportion for race and age, need to create
##Denominator values that are total males of each race and total males of each
#age group so that MSM proportion can have male only denominator and be stratified by race/age appropriately
################################################################################
data.list.race.male.denom = lapply(data.list.brfss.state.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$sex =="male")
  
  data<- data %>%
    group_by(location, race) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data= as.data.frame(data)
  list(filename, data) 
})
data.list.age.male.denom = lapply(data.list.brfss.state.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$sex =="male")
  
  data<- data %>%
    group_by(location, age) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
################################################################################

#######
##Race
#######

data.list.brfss.state.msm.race = lapply(data.list.race.male.denom, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm, race) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()

  data <- data %>%
    select(outcome, year, location, race, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
   
  data$value = round(data$value, digits=2)
  
   data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})

#######
##Age
#######

##this needs to be just males of the age group##

data.list.brfss.state.msm.age = lapply(data.list.age.male.denom, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm,age) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()

  data <- data %>%
    select(outcome, year, location, age, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})
###############################################################################
                ##Put MSM Proportion into data manager- BRFSS##
###############################################################################
##BRFSS MSM State-Total
msm.state.total = lapply(data.list.brfss.state.msm, `[[`, 2)  

for (data in msm.state.total) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##BRFSS MSM State- RACE
msm.state.race = lapply(data.list.brfss.state.msm.race, `[[`, 2)  

for (data in msm.state.race) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##BRFSS MSM State-AGE
msm.state.age = lapply(data.list.brfss.state.msm.age, `[[`, 2)  

for (data in msm.state.age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

###############################################################################
            ##Emory MSM Data by County for 2013#
###############################################################################
DATA.DIR.MSM="../../data_raw/emory"

emory_files <- Sys.glob(paste0(DATA.DIR.MSM, '/*.csv'))

data.list.emory.msm <- lapply(emory_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(COUNTYFP="character", STATEFP= "character")))
})

###############################################################################
                ##Clean Emory data##
###############################################################################
##########
##County
#########
data.list.emory.msm.county = lapply(data.list.emory.msm, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2013"
  data$outcome= "proportion.msm"

  data$value = as.numeric(data$MSM12MTH/data$ADULTMEN)
  data$value = round(data$value, digits=2)
  
  #To create location- combine state and county FIPS
  data$state_code= str_pad(data$STATEFP, width=2, side="left", pad="0")
  data$county_code= str_pad(data$COUNTYFP, width=3, side="left", pad="0")
  data$location = paste(data$state_code, data$county_code, sep="")

  data= as.data.frame(data)
  
  list(filename, data)
})
##########
##MSA
#########
data.list.emory.msm.msa = lapply(data.list.emory.msm, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2013"
  data$outcome= "proportion.msm"
  
  data$value = as.numeric(data$MSM12MTH/data$ADULTMEN)
  data$value = round(data$value, digits=2)
  
  #To create location- for MSA use 'CBSACODE' and also 'METMICSA' ==1 for metropolitian statistical area
    data = subset(data, data$METMICSA == "1") #Select msas only
    data$location = sub("^", "C.", data$CBSACODE)
    data$location_check = locations::is.location.valid(data$location)
  #Removing invalid MSA locations#
    data=subset(data, data$location_check == 'TRUE')
  
  data= as.data.frame(data)
  
  list(filename, data)
})

###############################################################################
##Put MSM Proportion into data manager##
###############################################################################
###Emory MSM by County
msm.emory.county = lapply(data.list.emory.msm.county, `[[`, 2)

for (data in msm.emory.county) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'emory',
    source = 'emory',
    dimension.values = list(),
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
}

###Emory MSM by MSA
msm.emory.msa = lapply(data.list.emory.msm.msa, `[[`, 2)

for (data in msm.emory.msa ) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'emory',
    source = 'emory',
    dimension.values = list(),
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
}
