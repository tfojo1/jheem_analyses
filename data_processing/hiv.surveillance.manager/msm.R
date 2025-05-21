
################################################################################
                      ##Create Proportion of MSM##
              ##Note this code pulls from the BRFSS dataset created in the BRFSS weighted state code
                      ##This is a weighted measure
                ##MSM Estimation using BRFSS Data by State
################################################################################
##Create a version of the brfss.state.sex data list to use below##
data.list.brfss.state.sex.for.msm = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, !is.na(data$sex)) #Need to remove sex is NA
  
  data<- data %>%
    group_by(location, sex) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location, sex) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by sex#
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`, race, risk, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

#######
##Total
#######
#Pulling from the brffs.state.sex data list so that denominator for msm proportion is males only
data.list.brfss.state.msm = lapply(data.list.brfss.state.sex.for.msm, function(file){
  
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
    select(outcome, year, location, msm, msm_total, n_weighted, `_LLCPWT`)%>% #n_weighted here is the sum of the weights by sex#
    filter(!is.na(msm_total))%>%
     mutate(value = (msm_total/n_weighted))

  data$value = round(data$value, digits=2)

  data<- data[!duplicated(data), ]

  #Need to add sex column in for put statment dimensions
  data$sex = "male"

  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##Prior to calculating MSM proportion for race and age, need to create
##Denominator values that are total males of each race and total males of each
#age group so that MSM proportion can have male only denominator and be stratified by race/age appropriately
################################################################################
data.list.race.male.denom = lapply(data.list.brfss.state.sex.for.msm, function(file){
  
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
data.list.age.male.denom = lapply(data.list.brfss.state.sex.for.msm, function(file){
  
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
  
  data= subset(data, data$race != "unknown")
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm, race) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()

  data <- data %>%
    select(outcome, year, location, race, msm, msm_total, n_weighted, `_LLCPWT`)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
   
  data$value = round(data$value, digits=2)
  
   data<- data[!duplicated(data), ]
   
   #Need to add sex column in for put statment dimensions
   data$sex = "male"
  
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
  
  data= subset(data, data$age != "Unknown")
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm,age) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()

  data <- data %>%
    select(outcome, year, location, age, msm, msm_total, n_weighted, `_LLCPWT`)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  #Need to add sex column in for put statment dimensions
  data$sex = "male"
  
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
    dimension.values = list(sex = "male"),
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
    dimension.values = list(sex = "male"),
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
    dimension.values = list(sex = "male"),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}


# Adding proportion.msm.n -------------------------------------------------

proportion.msm.n.total = lapply(data.list.brfss.state.msm, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(year, location, n_weighted, sex)%>%
        rename(value = n_weighted)%>%
        mutate(outcome = "proportion.msm.n")
    
    data= as.data.frame(data)
    list(filename, data) 
})

proportion.msm.n.race = lapply(data.list.brfss.state.msm.race, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(year, location, n_weighted, sex, race)%>%
        rename(value = n_weighted)%>%
        mutate(outcome = "proportion.msm.n")
    
    data= as.data.frame(data)
    list(filename, data) 
})

proportion.msm.n.age = lapply(data.list.brfss.state.msm.age, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(year, location, n_weighted, sex, age)%>%
        rename(value = n_weighted)%>%
        mutate(outcome = "proportion.msm.n")
    
    data= as.data.frame(data)
    list(filename, data) 
})

#Put proportion.msm.n - TOTAL
msm.n.total = lapply(proportion.msm.n.total, `[[`, 2)
for (data in msm.n.total) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#Put proportion.msm.n - RACE
msm.n.race = lapply(proportion.msm.n.race, `[[`, 2)  

for (data in msm.n.race) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#Put proportion.msm.n - AGE
msm.n.age = lapply(proportion.msm.n.age, `[[`, 2)  

for (data in msm.n.age) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


# Update for May 2025: Add in Variance value for proportion.msm da --------
# Create Variance for proportion.msm data from BRFSS (note, this is only by state level)
#stratifications: sex; sex+race; sex+age

#Variance- Total (sex) -----------------------------------------------------
variance.total.state = lapply(data.list.brfss.state.msm, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance)%>%
        rename(value = variance)%>% #rename the old value to now be variance.  This now represents the variance metric for the proportion tested outcome
        mutate(sex = "male")
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})


#Variance- Sex+Race -----------------------------------------------------
variance.sex.state = lapply(data.list.brfss.state.msm.race, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, sex, race)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, race, sex)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})
#Variance- Sex+Age -----------------------------------------------------
variance.age.state = lapply(data.list.brfss.state.msm.age, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, sex, age)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, age, sex)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})


# Put the variance data for proportion.msm ---------------------------------------------------
prop.msm.variance.state= lapply(variance.total.state, `[[`, 2)

for (data in prop.msm.variance.state) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

prop.msm.variance.sex.state= lapply(variance.sex.state, `[[`, 2)

for (data in prop.msm.variance.sex.state) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

prop.msm.variance.age.state= lapply(variance.age.state, `[[`, 2)

for (data in prop.msm.variance.age.state) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


###############################################################################
            ##Emory MSM Data by County for 2013#
###############################################################################
DATA.DIR.MSM="Q:/data_raw/emory"

emory_files <- Sys.glob(paste0(DATA.DIR.MSM, '/*.csv'))

data.list.emory.msm <- lapply(emory_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(COUNTYFP="character", STATEFP= "character")))
})

state.to.fips.mappings.emory = c('01' = 'AL',
                           '02'='AK',
                           '04'='AZ',
                           '05'='AR',
                           '06'='CA',
                           '08'='CO',
                           '09'='CT',
                           '10'='DE',
                           '11'='DC',
                           '12'='FL',
                           '13'='GA',
                           '15'='HI',
                           '16'='ID',
                           '17'='IL',
                           '18'='IN',
                           '19'='IA',
                           '20'='KS',
                           '21'='KY',
                           '22'='LA',
                           '23'='ME',
                           '24'='MD',
                           '25'='MA',
                           '26'='MI',
                           '27'='MN',
                           '28'='MS',
                           '29'='MO',
                           '30'='MT',
                           '31'='NE',
                           '32'='NV',
                           '33'='NH',
                           '34'='NJ',
                           '35'='NM',
                           '36'='NY',
                           '37'='NC',
                           '38'='ND',
                           '39'='OH',
                           '40'='OK',
                           '41'='OR',
                           '42'='PA',
                           '44'='RI',
                           '45'='SC',
                           '46'='SD',
                           '47'='TN',
                           '48'='TX',
                           '49'='UT',
                           '50'='VT',
                           '51'='VA',
                           '53'='WA',
                           '54'='WV',
                           '55'='WI',
                           '56'='WY',
                           '72'= 'PR')
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

  data$value = as.numeric(data$MSM5YEAR/data$ADULTMEN)
  data$value = round(data$value, digits=2)
  
  #To create location- combine state and county FIPS
  data$state_code= str_pad(data$STATEFP, width=2, side="left", pad="0")
  data$county_code= str_pad(data$COUNTYFP, width=3, side="left", pad="0")
  data$location = paste(data$state_code, data$county_code, sep="")
  
  data = subset(data, data$location != "51515") #Removed this from the locations package March 2025

  #Need to add sex column in for put statment dimensions
  data$sex = "male"
  
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
  
  #To create location- for MSA use 'CBSACODE' and also 'METMICSA' ==1 for metropolitian statistical area
    data = subset(data, data$METMICSA == "1") #Select msas only
    data$location = sub("^", "C.", data$CBSACODE)
    data$location_check = locations::is.location.valid(data$location)
    
   #Removing invalid MSA locations#
     data=subset(data, data$location_check == 'TRUE')
    
    #Group by location to sum across MSAs
     data <- data %>%
       group_by(location)%>%
       mutate(sum_msm = sum(MSM5YEAR))%>%
       mutate(sum_adult_men = sum(ADULTMEN))
    
    data$value = as.numeric(data$sum_msm/data$sum_adult_men)
    data$value = round(data$value, digits=2)
    
    #Need to add sex column in for put statment dimensions
    data$sex = "male"
    
    data <- data %>%
      select(year, location, outcome, sex, value)
  
    data<- data[!duplicated(data), ]
    
  data= as.data.frame(data)
  
  list(filename, data)
})

##########
##State
#########
data.list.emory.msm.state = lapply(data.list.emory.msm, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2013"
  data$outcome= "proportion.msm"
  
  #To create location- combine state and county FIPS
  data$state_code= str_pad(data$STATEFP, width=2, side="left", pad="0")
  data$location = state.to.fips.mappings.emory[data$state_code]
  
  #Group by location to sum across states
  data <- data %>%
    group_by(location)%>%
    mutate(sum_msm = sum(MSM5YEAR))%>%
    mutate(sum_adult_men = sum(ADULTMEN))
  
  data$value = as.numeric(data$sum_msm/data$sum_adult_men)
  data$value = round(data$value, digits=2)
  
  #Need to add sex column in for put statement dimensions
  data$sex = "male"
  
    data <- data %>%
      select(year, location, outcome, sex, value)
  
  data<- data[!duplicated(data), ]
  
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
    dimension.values = list(sex = "male"),
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
    dimension.values = list(sex = "male"),
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
}

##Emory MSM by States

msm.emory.state = lapply(data.list.emory.msm.state, `[[`, 2)

for (data in msm.emory.state ) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'emory',
    source = 'emory',
    dimension.values = list(sex = "male"),
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
}