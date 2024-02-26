library(haven)
library(dplyr)
################################################################################
##Read in NHANES Data
DATA.DIR.NHANES="C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/NHANES"

nhanes.files <- list.files(DATA.DIR.NHANES, pattern = ".XPT", full.names = "TRUE")

nhanes.files.data.list <- lapply(nhanes.files, function(x) {
  list(filename=x, data=read_xpt(x))
})
################################################################################
age.mappings = c('9' = '2015-2016',
                 '8' = '2013-2014',
                 '7' = '2011-2012',
                 '6' = '2009-2010',
                 '5' = '2007-2008')
################################################################################
nhanes.clean = lapply(nhanes.files.data.list, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("demographics", filename)) {
    data <- data %>%
      select(SEQN, WTINT2YR, WTMEC2YR, SDDSRVYR, RIDAGEYR, RIDRETH1, RIAGENDR)%>%
      rename(survey.year = SDDSRVYR)%>%
      rename(ID = SEQN)%>%
      rename(age = RIDAGEYR)%>%
      rename(gender = RIAGENDR)
  }
  
  if(grepl("sexual_behavior_ok", filename)) {
    data <- data %>%
      select(SEQN, SXD021, SXQ809, SXD031, SXQ251, SXQ270, SXQ292, SXQ294)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(hx.gonorrhea = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)

  }
  
  if(grepl("sexual_behavior_fix1", filename)) { #NEED TO ADD IN HERE SOMETHNG FOR CALCULATE SEX WITH MEN SXQ101
    data <- data %>%
      select(SEQN, SXQ021,  SXQ101, SXD031, SXQ251, SXQ270, SXQ292, SXQ294)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXQ021)%>%
      rename(men.ever.sex.with.men =SXQ101)%>%    #You will need to use gender from demos to re-calculate this bc it's different from other vars in other years but I'm relabeling for the purpose of merge
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(hx.gonorrhea = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)
  }
  
  if(grepl("sexual_behavior_fix2", filename)) {
    data <- data %>%
      select(SEQN, SXD021,  SXQ809, SXD031, SXQ251, SXQ270, SXQ295, SXQ296)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(hx.gonorrhea = SXQ270)%>%
      rename(sexual.orientation.male = SXQ296)%>%
      rename(sexual.orientation.female = SXQ295)
  }
  
  data$survey.year = outcome.mappings[data$Isurvey.year]
  
  list(filename, data) 
  
})

################################################################################
#Merge all the years for demo and sexual behavior
################################################################################

demo = nhanes.clean[[1]]
demo = demo [[2]]
sex = nhanes.clean[[2]]
sex = sex [[2]]
total.07.08= merge(demo, sex, by="ID")

demo = nhanes.clean[[3]]
demo = demo [[2]]
sex = nhanes.clean[[4]]
sex = sex [[2]]
total.09.10= merge(demo, sex, by="ID")

demo = nhanes.clean[[5]]
demo = demo [[2]]
sex = nhanes.clean[[6]]
sex = sex [[2]]
total.11.12= merge(demo, sex, by="ID")

demo = nhanes.clean[[7]]
demo = demo [[2]]
sex = nhanes.clean[[8]]
sex = sex [[2]]
total.13.14= merge(demo, sex, by="ID")

demo = nhanes.clean[[9]]
demo = demo [[2]]
sex = nhanes.clean[[10]]
sex = sex [[2]]
total.15.16= merge(demo, sex, by="ID")

nhanes.total = rbind(total.07.08, total.09.10, total.11.12, total.13.14, total.15.16)

################################################################################
#Now wrangle data for subset
################################################################################

nhanes.total.clean <- nhanes.total%>%
  mutate()



 