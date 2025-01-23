#Put the proportion.msm into the syphilis manager
#I'm going to pull this from the surveillance manager because it actually pulls from
#BRFSS so to run it independently would be a lot of processing time

#At first this code is just pulling for county data but at the bottom I add national

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")


# Source One = BRFSS -----------------------------------------------------
brfss_sex = as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$brfss$brfss$year__location__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.msm")

brfss_race_sex = as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$brfss$brfss$year__location__race__sex)%>%
rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(race = as.character(race))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.msm")%>%
  mutate(race = tolower(race))

brfss_age_sex = as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$brfss$brfss$year__location__age__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(age = as.character(age))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.msm")

#Combine BRFSS
prop.msm.brfss = list(
  brfss_sex,
  brfss_race_sex,
  brfss_age_sex
)

# Source Two = Emory ------------------------------------------------------

emory_sex = as.data.frame.table(surveillance.manager$data$proportion.msm$estimate$emory$emory$year__location__sex)%>%
rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.msm")


# Need Two Put Statements because Sources are different -------------------
 
#BRFSS Put
for (data in prop.msm.brfss) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.msm',
    source = 'brfss',
    dimension.values = list(sex = "male"),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

#Emory Put
  data.manager$put.long.form(
    data = emory_sex,
    ontology.name = 'emory',
    source = 'emory',
    dimension.values = list(sex = "male"),
    url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
    details = 'Emory University MSM Research from American Community Survey')
  

# Adding Proportion MSM at National Level (For Emory and BRFSS) -----------


# Emory National ----------------------------------------------------------
  DATA.DIR.MSM="../../data_raw/emory"
  emory_files <- Sys.glob(paste0(DATA.DIR.MSM, '/*.csv'))
  data.list.emory.msm <- lapply(emory_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(COUNTYFP="character", STATEFP= "character")))
  })
  
  emory.msm.national = lapply(data.list.emory.msm, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$year = "2013"
    data$outcome= "proportion.msm"
    data$location = "US"
    
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
  
  national.emory.msm.put = lapply(emory.msm.national, `[[`, 2)
  
  for (data in national.emory.msm.put ) {
    
    data.manager$put.long.form(
      data = data,
      ontology.name = 'emory',
      source = 'emory',
      dimension.values = list(sex = "male"),
      url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
      details = 'Emory University MSM Research from American Community Survey')
  }
  

# BRFSS National ----------------------------------------------------------

  #I'm going to write a separate code for this.

