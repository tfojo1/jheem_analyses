#Put the proportion.msm into the syphilis manager
#I'm going to pull this from the surveillance manager because it actually pulls from
#BRFSS so to run it independently would be a lot of processing time

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
  mutate(outcome = "proportion.msm")

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
    ontology.name = 'brfss',
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

