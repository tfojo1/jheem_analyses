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
survey.year.mappings = c('9' = '2015-2016',
                 '8' = '2013-2014',
                 '7' = '2011-2012',
                 '6' = '2009-2010',
                 '5' = '2007-2008')

race.mappings = c('1' = 'hispanic', #Grouping Mexican American and Other Hispanic into 'Hispanic'
                  '2' = 'hispanic',
                  '3' = 'white, non hispanic',
                  '4' = 'black, non hispanic',
                  '5'= 'other',
                  '.' = "missing")

gender.mappings = c('1'= 'male',
                    '2'= 'female',
                    '.' = 'missing')

msm.mappings = c('1' = 'yes',
                '2' = 'no',
                '7' = 'refused',
                '9' = 'dont know',
                '.' = 'missing')

sexual.orientation.mappings = c( '1'= 'heterosexual',
                                 '2' = 'homosexual',
                                 '3' = 'bisexual',
                                 '4' = 'something else',
                                 '5' = 'not sure',
                                 '7' = 'refused',
                                 '9' = 'dont know',
                                 '.' = 'missing')
  
gonorrhea.mappings = c('1' = 'yes',
                       '2' = 'no',
                       '7' = 'refused',
                       '9' = 'dont know',
                       '.' = 'missing')
  
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
      rename(race = RIDRETH1)%>%
      rename(gender = RIAGENDR)%>%
      mutate(survey.year = as.character(survey.year))
    
    data$survey.year = survey.year.mappings[data$survey.year]
    data$race = race.mappings[data$race]
    data$gender = gender.mappings[data$gender]
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
      select(SEQN, SXQ021,  SXQ410, SXD031, SXQ251, SXQ270, SXQ292, SXQ294)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXQ021)%>%
      rename(men.ever.sex.with.men =SXQ410)%>%    #You will need to use gender from demos to re-calculate this bc it's different from other vars in other years but I'm relabeling for the purpose of merge
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
total.07.08 <- total.07.08 %>%
  mutate(men.ever.sex.with.men = ifelse(men.ever.sex.with.men == 99999, NA, men.ever.sex.with.men))%>%
  mutate(men.ever.sex.with.men = ifelse(men.ever.sex.with.men == 77777, NA, men.ever.sex.with.men))%>%
  mutate(fixed.msm = ifelse(gender == "1" & men.ever.sex.with.men > 0, 1, 2))%>% #1= msm; 2=not msm
  select(-men.ever.sex.with.men)%>%
  rename(men.ever.sex.with.men = fixed.msm)

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

nhanes.total$men.ever.sex.with.men = msm.mappings[nhanes.total$men.ever.sex.with.men]

nhanes.total$hx.gonorrhea = gonorrhea.mappings[nhanes.total$hx.gonorrhea]
nhanes.total$sexual.orientation.male = sexual.orientation.mappings[nhanes.total$sexual.orientation.male]
nhanes.total$sexual.orientation.female = sexual.orientation.mappings[nhanes.total$sexual.orientation.female]

nhanes.subset = nhanes.total
################################################################################
#MSM Tables
#Should these be by year?
################################################################################

total.msm.table <- nhanes.total%>%
# mutate(msm = case_when(men.ever.sex.with.men == "1" ~ "yes",
#                        men.ever.sex.with.men == "2" ~ "no",
#                        men.ever.sex.with.men == "7" ~ "refused",
#                        men.ever.sex.with.men == "9" ~ "don't know",
#                        is.na(men.ever.sex.with.men) ~ "missing"))%>%
  count(men.ever.sex.with.men)%>%
  rename(men.ever.sex.with.men.count  = n)%>%
  mutate(proportion.msm.unweighted = round(men.ever.sex.with.men.count/sum(men.ever.sex.with.men.count), digits=2))

race.msm.table <- nhanes.total%>%
  filter(men.ever.sex.with.men == "yes")%>%
  count(race)%>%
  rename(men.ever.sex.with.men.count = n)%>%
  mutate(proportion.msm.unweighted = round(men.ever.sex.with.men.count/sum(men.ever.sex.with.men.count), digits=2))

age.msm.table <- nhanes.total%>%
  mutate(age.group = case_when(age <= 24 ~ "18-24 years",
                               age >= 25 & age <=34 ~ "25-34 years",
                               age >= 35 & age <= 44 ~ "35-44 years",
                               age >= 45 & age <= 54 ~ "45-54 years",
                               age >= 55~ "55+ years"))%>%
  filter(men.ever.sex.with.men == "yes")%>%
  count(age.group)%>%
  rename(men.ever.sex.with.men.count = n)%>%
  mutate(proportion.msm.unweighted = round(men.ever.sex.with.men.count/sum(men.ever.sex.with.men.count), digits=2))


################################################################################
#Age at first time of sex Tables
################################################################################
single.age.at.sex.table <- nhanes.total%>%
  count(age.at.first.sex)%>%
  rename(age.at.first.sex.count=n)%>%
  mutate(proportion.age.at.first.sex.unweighted = round(age.at.first.sex.count/sum(age.at.first.sex.count), digits=2))

age.grouping.at.sex.table <- nhanes.total%>%
  mutate(age.at.first.sex = ifelse(age.at.first.sex == 99, NA, age.at.first.sex))%>%
  mutate(age.at.first.sex = ifelse(age.at.first.sex == 77, NA, age.at.first.sex))%>%
  mutate(age.group = case_when(age.at.first.sex <= 24 ~ "18-24 years",
                               age.at.first.sex >= 25 & age.at.first.sex <=34 ~ "25-34 years",
                               age.at.first.sex >= 35 & age.at.first.sex <= 44 ~ "35-44 years",
                               age.at.first.sex >= 45 & age.at.first.sex <= 54 ~ "45-54 years",
                               age.at.first.sex >= 55~ "55+ years",
                               TRUE ~ 'missing'))%>%
  count(age.group)%>%
  rename(age.group.count=n)%>%
  mutate(proportion.age.at.first.sex.unweighted = round(age.group.count/sum(age.group.count), digits=2))

new.age.grouping.at.sex.table <- nhanes.total%>%
  mutate(age.at.first.sex = ifelse(age.at.first.sex == 99, NA, age.at.first.sex))%>%
  mutate(age.at.first.sex = ifelse(age.at.first.sex == 77, NA, age.at.first.sex))%>%
  mutate(age.group = case_when(
                               age.at.first.sex >= 10 & age.at.first.sex <= 12 ~ "10-12 years",
                               age.at.first.sex >= 13 & age.at.first.sex <= 17 ~ "13-17 years",
                               age.at.first.sex >= 18 & age.at.first.sex <= 24 ~ "18-24 years",
                               age.at.first.sex >= 25 & age.at.first.sex <=34 ~ "25-34 years",
                               age.at.first.sex >= 35 & age.at.first.sex <= 44 ~ "35-44 years",
                               age.at.first.sex >= 45 & age.at.first.sex <= 54 ~ "45-54 years",
                               age.at.first.sex >= 55~ "55+ years",
                               TRUE ~ 'missing'))%>%
  count(age.group)%>%
  rename(age.group.count=n)%>%
  mutate(proportion.age.at.first.sex.unweighted = round(age.group.count/sum(age.group.count), digits=2))


 