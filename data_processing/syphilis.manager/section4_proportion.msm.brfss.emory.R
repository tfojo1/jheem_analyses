#BRFSS Data and Emory Data: Proportion.MSM

#===============================================================================
# Source One = BRFSS 

#Note this pulls from a list of dataframes created in 'proportion.tested.for.
#hiv_state_weighted.R' code.  

#This is a weighted estimate by state.
#===============================================================================

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


# Total: ------------------------------------------------------------------
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

# Race: -------------------------------------------------------------------

data.list.brfss.state.msm.race = lapply(data.list.race.male.denom, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, !is.na(data$race))
    
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


# Age ---------------------------------------------------------------------


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

# Put BRFSS Data: ---------------------------------------------------------

msm.state.total = lapply(data.list.brfss.state.msm, `[[`, 2)  

for (data in msm.state.total) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.msm',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


msm.state.race = lapply(data.list.brfss.state.msm.race, `[[`, 2)  

for (data in msm.state.race) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.msm',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


msm.state.age = lapply(data.list.brfss.state.msm.age, `[[`, 2)  

for (data in msm.state.age) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.msm',
        source = 'brfss',
        dimension.values = list(sex = "male"),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


#===============================================================================
# Source Two = Emory 
#===============================================================================
surveillance.manager = load.data.manager(name="surveillance.manager", file="Q:/data_managers/surveillance.manager.rdata")


emory_sex = as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$emory$emory$year__location__sex)%>%
rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.msm")%>%
  filter(location != "51515")%>% #Removed this from the locations package March 2025%>%
select(-sex)%>%
    filter(!is.na(value))%>%
    mutate(outcome = ifelse(year == "2020", "proportion.msm.last.year", outcome),#Make separate outcomes for each year of emory estimates
           year = ifelse(year == "2013", "2011", year)) #Parastu wanted this year change to represent the midpoint of this estimate range
    



#Emory Put
  data.manager$put.long.form(
    data = emory_sex,
    ontology.name = 'emory',
    source = 'emory',
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
  

# Adding Proportion MSM at National Level (For Emory and BRFSS) -----------


# Emory National ----------------------------------------------------------
  DATA.DIR.MSM=file.path(Q_ROOT, "data_raw/emory")
  emory_files <- Sys.glob(paste0(DATA.DIR.MSM, '/*.csv'))
  data.list.emory.msm <- lapply(emory_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(COUNTYFP="character", STATEFP= "character")))
  })
  
  emory.msm.national = lapply(data.list.emory.msm, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$year = "2011" #changes from 2013 to 2011 Oct 2026 to represent mid year of estimate range
    data$outcome= "proportion.msm"
    data$location = "US"
    
    #Group by location to sum across states
    data <- data %>%
      group_by(location)%>%
      mutate(sum_msm = sum(MSM5YEAR))%>%
      mutate(sum_adult_men = sum(ADULTMEN))
    
    data$value = as.numeric(data$sum_msm/data$sum_adult_men)
    data$value = round(data$value, digits=2)

    
    data <- data %>%
      select(year, location, outcome, value)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    
    list(filename, data)
  })  
  
  national.emory.msm.put = lapply(emory.msm.national, `[[`, 2)
  
  for (data in national.emory.msm.put ) {
    
    data.manager$put.long.form(
      data = data,
      ontology.name = 'emory',
      source = 'emory',
      url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
      details = 'Emory University MSM Research from American Community Survey')
  }
  

# BRFSS National ----------------------------------------------------------

  #I'm going to write a separate code for this.

  

# -------------------------------------------------------------------------
#Adding aggregated proportion.msm by MSA for 2013 only.  This is because
  #2020 data already has data by MSA.
  
  agg.msa.2013 <- as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$emory.aggregated$emory$year__location__sex)%>%
      select(-sex)%>%
      mutate(year = as.character(year),
             value = as.numeric(Freq),
             location = as.character(location),
             outcome = "proportion.msm")
  
      
      data.manager$put.long.form(
          data = agg.msa.2013,
          ontology.name = 'emory',
          source = 'emory',
          url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
          details = 'Aggregated from County Data')

  
