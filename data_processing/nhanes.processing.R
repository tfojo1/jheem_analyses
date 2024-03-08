library(haven)
library(dplyr)
################################################################################
##Read in NHANES Data

#running on local:
DATA.DIR.NHANES="C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/First Week/NHANES"

#If running on remote:
#DATA.DIR.NHANES="Q:/data_clean/nhanes/raw_data"

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

nhanes.mappings = c('1' = 'yes',
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
    
    #Creating an age grouping- this is flexible 
    data <- data %>%
      mutate(age.group= case_when(age < 18 ~ "Under 18",
                                   age >= 18 & age <= 24 ~ "18-24 years",
                                   age >= 25 & age <= 29 ~ "25-29 years",
                                   age >= 30 & age <= 34 ~ "30-34 years",
                                   age >= 35 & age <= 39 ~ "35-39 years",
                                   age >= 40 & age <= 44 ~ "40-44 years",
                                   age >= 45 & age <= 49 ~ "45-49 years",
                                   age >= 50 & age <= 54 ~ "50-54 years",
                                   age >= 55 & age <= 59 ~ "55-59 years",
                                   age >= 60 ~ "60+ years"))
                                   
  }
  

  if(grepl("07.08_sexual_behavior", filename)) { 
    data <- data %>%
      select(SEQN, SXQ021,  SXQ410, SXD031, SXQ251, SXQ270, SXQ292, SXQ294, SXQ510, SXQ550, SXQ450, SXQ490)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXQ021)%>%
      rename(men.ever.sex.with.men =SXQ410)%>%    #You will need to use gender from demos to re-calculate this bc it's different from other vars in other years but I'm relabeling for the purpose of merge
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(gonorrhea.past.year = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)%>%
      rename(male.sex.with.female.past.year= SXQ510)%>%
      rename(male.sex.with.male.past.year = SXQ550)%>%
      rename(female.sex.with.male.past.year=SXQ450)%>%
      rename(female.sex.with.female.past.year=SXQ490)
    
  }
  if(grepl("09.10_sexual_behavior", filename)) {
    data <- data %>%
      select(SEQN, SXD021, SXQ809, SXD031, SXQ251, SXQ270, SXQ292, SXQ294, SXD510, SXQ550, SXD450, SXQ490)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(gonorrhea.past.year = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)%>%
      rename(male.sex.with.female.past.year= SXD510)%>%
      rename(male.sex.with.male.past.year = SXQ550)%>%
      rename(female.sex.with.male.past.year=SXD450)%>%
      rename(female.sex.with.female.past.year=SXQ490)

    
  }
  if(grepl("11.12_sexual_behavior", filename)) {
    data <- data %>%
      select(SEQN, SXD021, SXQ809, SXD031, SXQ251, SXQ270, SXQ292, SXQ294, SXD510, SXQ550, SXD450, SXQ490)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(gonorrhea.past.year = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)%>%
      rename(male.sex.with.female.past.year= SXD510)%>%
      rename(male.sex.with.male.past.year = SXQ550)%>%
      rename(female.sex.with.male.past.year=SXD450)%>%
      rename(female.sex.with.female.past.year=SXQ490)

    
  }
  if(grepl("13.14_sexual_behavior", filename)) {
    data <- data %>%
      select(SEQN, SXD021, SXQ809, SXD031, SXQ251, SXQ270, SXQ292, SXQ294, SXD510, SXQ550, SXD450, SXQ490)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(gonorrhea.past.year = SXQ270)%>%
      rename(sexual.orientation.male = SXQ292)%>%
      rename(sexual.orientation.female = SXQ294)%>%
      rename(male.sex.with.female.past.year= SXD510)%>%
      rename(male.sex.with.male.past.year = SXQ550)%>%
      rename(female.sex.with.male.past.year=SXD450)%>%
      rename(female.sex.with.female.past.year=SXQ490)

  }
  
  if(grepl("15.16_sexual_behavior", filename)) {
    data <- data %>%
      select(SEQN, SXD021,  SXQ809, SXD031, SXQ251, SXQ270, SXQ295, SXQ296, SXD510, SXQ550, SXD450, SXQ490)%>%
      rename(ID = SEQN)%>%
      rename(ever.had.sex =SXD021)%>%
      rename(men.ever.sex.with.men =SXQ809)%>%
      rename(age.at.first.sex = SXD031)%>%
      rename(number.sex.wo.condom.past.year = SXQ251)%>%
      rename(gonorrhea.past.year = SXQ270)%>%
      rename(sexual.orientation.male = SXQ296)%>%
      rename(sexual.orientation.female = SXQ295)%>%
      rename(male.sex.with.female.past.year= SXD510)%>%
      rename(male.sex.with.male.past.year = SXQ550)%>%
      rename(female.sex.with.male.past.year=SXD450)%>%
      rename(female.sex.with.female.past.year=SXQ490)

  }
  list(filename, data) 
  
})

################################################################################
#Merge all the years for demo and sexual behavior
################################################################################
#Do an inner join- only want people who took the sexual health survey and demos

demo = nhanes.clean[[1]]
demo = demo [[2]]
sex = nhanes.clean[[2]]
sex = sex [[2]]
total.07.08= inner_join(demo, sex, by="ID")

total.07.08 <- total.07.08 %>%
  mutate(men.ever.sex.with.men = ifelse(men.ever.sex.with.men == 99999, NA, men.ever.sex.with.men))%>%
  mutate(men.ever.sex.with.men = ifelse(men.ever.sex.with.men == 77777, NA, men.ever.sex.with.men))%>%
  mutate(fixed.msm = case_when(gender == "female" ~ NA,
                               gender == "male" & men.ever.sex.with.men > 0 ~ "1",
                               gender == "male" & men.ever.sex.with.men <= 0 ~"0"))%>%
  
  select(-men.ever.sex.with.men)%>%
  rename(men.ever.sex.with.men = fixed.msm)

demo = nhanes.clean[[3]]
demo = demo [[2]]
sex = nhanes.clean[[4]]
sex = sex [[2]]
total.09.10= inner_join(demo, sex, by="ID")

demo = nhanes.clean[[5]]
demo = demo [[2]]
sex = nhanes.clean[[6]]
sex = sex [[2]]
total.11.12= inner_join(demo, sex, by="ID")

demo = nhanes.clean[[7]]
demo = demo [[2]]
sex = nhanes.clean[[8]]
sex = sex [[2]]
total.13.14= inner_join(demo, sex, by="ID")

demo = nhanes.clean[[9]]
demo = demo [[2]]
sex = nhanes.clean[[10]]
sex = sex [[2]]
total.15.16= inner_join(demo, sex, by="ID")

nhanes.total = rbind(total.07.08, total.09.10, total.11.12, total.13.14, total.15.16)

#Add mappings for variables
nhanes.total$men.ever.sex.with.men = nhanes.mappings[nhanes.total$men.ever.sex.with.men]
nhanes.total$gonorrhea.past.year = gonorrhea.mappings[nhanes.total$gonorrhea.past.year]
nhanes.total$sexual.orientation.male = sexual.orientation.mappings[nhanes.total$sexual.orientation.male]
nhanes.total$sexual.orientation.female = sexual.orientation.mappings[nhanes.total$sexual.orientation.female]
nhanes.total$ever.had.sex = nhanes.mappings[nhanes.total$ever.had.sex]

#Subset data by only those who have had sex (ever.had.sex = yes)
nhanes.total = subset(nhanes.total, nhanes.total$ever.had.sex == "yes")

#Removing 12 individuals for who age.at.first.sex is NA (the rest of their surveys are also NA)
nhanes.total = subset(nhanes.total, !is.na(nhanes.total$age.at.first.sex))

#Formatting age.at.first.sex
nhanes.total$age.at.first.sex = ifelse(nhanes.total$age.at.first.sex == 99, "dont know", nhanes.total$age.at.first.sex)
nhanes.total$age.at.first.sex = ifelse(nhanes.total$age.at.first.sex == 77, "refused", nhanes.total$age.at.first.sex)
nhanes.total$age.at.first.sex = ifelse(nhanes.total$age.at.first.sex == 9, "0-9", nhanes.total$age.at.first.sex)

#Formatting partners in past year
# nhanes.total$male.sex.with.female.past.year = ifelse(nhanes.total$male.sex.with.female.past.year == 99999, "dont know", nhanes.total$male.sex.with.female.past.year)
# nhanes.total$male.sex.with.female.past.year = ifelse(nhanes.total$male.sex.with.female.past.year == 77777, "refused", nhanes.total$male.sex.with.female.past.year)
# 
# nhanes.total$male.sex.with.male.past.year = ifelse(nhanes.total$male.sex.with.male.past.year == 99999, "dont know", nhanes.total$male.sex.with.male.past.year)
# nhanes.total$male.sex.with.male.past.year = ifelse(nhanes.total$male.sex.with.male.past.year == 77777, "refused", nhanes.total$male.sex.with.male.past.year)
# 
# nhanes.total$female.sex.with.male.past.year = ifelse(nhanes.total$female.sex.with.male.past.year == 99999, "dont know", nhanes.total$female.sex.with.male.past.year)
# nhanes.total$female.sex.with.male.past.year = ifelse(nhanes.total$female.sex.with.male.past.year == 77777, "refused", nhanes.total$female.sex.with.male.past.year)
# 
# nhanes.total$female.sex.with.female.past.year = ifelse(nhanes.total$female.sex.with.female.past.year == 99999, "dont know", nhanes.total$female.sex.with.female.past.year)
# nhanes.total$female.sex.with.female.past.year = ifelse(nhanes.total$female.sex.with.female.past.year == 77777, "refused", nhanes.total$female.sex.with.female.past.year)

#There are 1370 MALES who report sexual orientation as homosexual but report no for MSM.  I'm going to change these to MSM (there's also 47 bisexual but I'm going to leave these)
 nhanes.total <- nhanes.total %>%
   mutate(msm.adjusted = case_when(gender == 'male' & sexual.orientation.male == 'homosexual' & men.ever.sex.with.men == 'no' ~ 'yes',
                                       gender == 'male' & men.ever.sex.with.men == 'yes' ~ 'yes',
                                       gender == 'male' & men.ever.sex.with.men == 'no' ~ 'no',
                                       gender == 'male' & is.na(men.ever.sex.with.men) ~ NA))
#Data Subset for Todd 
nhanes.subset = nhanes.total

################################################################################
##Metrics Todd wants 
################################################################################
#Cumulative Proportion of Age at sex initiation
cumulative.proportion.age.at.sex.initiation.all.years <- nhanes.subset%>%
  filter(age.at.first.sex != "dont know")%>%
  filter(age.at.first.sex != "refused")%>%
  filter(age < 24)%>% #adjust age of respondents to get this chart to look more like the next
  count(age.at.first.sex)%>%
  rename(age.at.first.sex.count=n)%>%
  mutate(proportion.age.at.first.sex.unweighted = round(age.at.first.sex.count/sum(age.at.first.sex.count), digits=2))%>%
  mutate(cumulative.proportion = cumsum(proportion.age.at.first.sex.unweighted))

#Proportion: for each age, what proportion of people who are that age had sex in the past year?
#what proportion of 25 years old had sex in past yr
proportion.sex.in.past.year.by.age.total <- nhanes.subset%>%
  mutate(sex.in.past.year = case_when(male.sex.with.female.past.year > 0 & male.sex.with.female.past.year < 7777
                                    | male.sex.with.male.past.year > 0 & male.sex.with.male.past.year < 7777
                                    | female.sex.with.male.past.year > 0 & female.sex.with.male.past.year < 7777
                                    | female.sex.with.female.past.year > 0 & female.sex.with.female.past.year < 7777 ~ "1", 
                                    TRUE ~ "0"))%>%
  group_by(age)%>%
  count(sex.in.past.year)%>%
  rename(count.sex.in.past.year = n)%>%
  mutate(proportion.count.sex.in.past.year.unweighted = round(count.sex.in.past.year/sum(count.sex.in.past.year), digits=2))%>%
  filter(sex.in.past.year == "1")

#Combining
snippet.1 <- cumulative.proportion.age.at.sex.initiation.all.years%>%
  rename(age = age.at.first.sex)%>%
  filter(age < 18)%>%
  rename(count.sex.in.past.year= age.at.first.sex.count)%>%
  select(age, count.sex.in.past.year)

snippet.2 <- proportion.sex.in.past.year.by.age.total%>%
  mutate(age = as.character(age))%>%
  select(age, count.sex.in.past.year)

sexual.availability.by.age <- rbind(snippet.1, snippet.2, by="age")%>%
  filter(age != "age")%>%
  mutate(count.sex.in.past.year = as.numeric(count.sex.in.past.year))%>%
  mutate(proportion= round(count.sex.in.past.year/sum(count.sex.in.past.year), digits=2))%>%
  mutate(cumulative.proportion = cumsum(proportion))
################################################################################
##Adjusting the initial tables to get proportions for meeting 3.7.24
################################################################################
#Proportion of Heterosexual Males who report Gonorrhea in Past Year
all.hetero.male.prep.indications <- nhanes.subset %>%
  filter(msm.adjusted != "yes")%>%
  filter(gender == "male") %>%
  filter(!is.na(gonorrhea.past.year)) %>% #REMOVING NA VALUES
  group_by(gonorrhea.past.year)%>%
  count(gonorrhea.past.year)%>%
  rename(count.gonorrhea.past.year = n)%>%
  group_by()%>%
  mutate(proportion.gonorrhea.past.year = round(count.gonorrhea.past.year/ sum(count.gonorrhea.past.year), digits=5))

#Proportion of Heterosexual Females who report Gonorrhea in Past Year
all.hetero.female.prep.indications <- nhanes.subset %>%
  filter(gender == "female") %>%
  filter(!is.na(gonorrhea.past.year)) %>% #REMOVING NA VALUES
  group_by(gonorrhea.past.year)%>%
  count(gonorrhea.past.year)%>%
  rename(count.gonorrhea.past.year = n)%>%
  group_by()%>%
  mutate(proportion.gonorrhea.past.year = round(count.gonorrhea.past.year/ sum(count.gonorrhea.past.year), digits=5))

#MSM with gonorrhea in past year
msm.gonorrhea.prep.indications <- nhanes.subset %>%
  filter(msm.adjusted == "yes")%>%
  filter(!is.na(gonorrhea.past.year)) %>% #REMOVING NA VALUES
  group_by(gonorrhea.past.year)%>%
  count(gonorrhea.past.year)%>%
  rename(count.gonorrhea.past.year = n)%>%
  group_by()%>%
  mutate(proportion.gonorrhea.past.year = round(count.gonorrhea.past.year/ sum(count.gonorrhea.past.year), digits=2))

####

#Proportion of MSM Reporting Casual Partner
proportion.msm.with.casual.partner.denom1 <- nhanes.subset %>%
  filter(msm.adjusted == "yes")%>%
  mutate(active.condomless.msm = ifelse(male.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless.msm)) %>% #REMOVING NA VALUES
  group_by(active.condomless.msm)%>%
  count(active.condomless.msm)%>%
  rename(count.active.condomless.msm = n)%>%
  group_by()%>%
  mutate(proportion.active.condomless.msm = round(count.active.condomless.msm/ sum(count.active.condomless.msm), digits=2))

#Heterosexual Male- casual partner
hetero.male.casual.partner <- nhanes.subset %>%
  filter(msm.adjusted != "yes")%>%
  filter(gender == "male") %>%
  mutate(active.condomless = ifelse(male.sex.with.female.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless)) %>% #REMOVING NA VALUES
  group_by(active.condomless)%>%
  count(active.condomless)%>%
  rename(count.active.condomless = n)%>%
  group_by()%>%
  mutate(proportion.active.condomless = round(count.active.condomless/ sum(count.active.condomless), digits=2))

#Heterosexual female- casual partner
hetero.female.casual.partner  <- nhanes.subset %>%
  filter(gender == "female")%>%
  mutate(active.condomless = ifelse(female.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless)) %>% #REMOVING NA VALUES
  group_by(active.condomless)%>%
  count(active.condomless)%>%
  rename(count.active.condomless = n)%>%
  group_by()%>%
  mutate(proportion.active.condomless = round(count.active.condomless/ sum(count.active.condomless), digits=2))%>%
  filter(active.condomless == "1")

#Heterosexual female with casual partner by age group
hetero.female.casual.partner.by.age  <- nhanes.subset %>%
  filter(gender == "female")%>%
  mutate(active.condomless = ifelse(female.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless)) %>% #REMOVING NA VALUES
  group_by(age.group, active.condomless)%>%
  count(active.condomless)%>%
  rename(count.active.condomless = n)%>%
  group_by()%>%
  mutate(proportion.active.condomless = round(count.active.condomless/ sum(count.active.condomless), digits=2))%>%
  filter(active.condomless == "1")

################################################################################
##3 datasets needed for regression
################################################################################

nhanes.msm <- nhanes.subset%>%
  filter(msm.adjusted == "yes")%>%
  mutate(sex.in.past.year.and.condomless = ifelse(male.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  select(ID, survey.year, age, race, gender, age.group, gonorrhea.past.year, sex.in.past.year.and.condomless)
#save(nhanes.msm, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/First Week/NHANES/nhanes.msm.RData") ##UPDATE LOCATION

nhanes.hetero.males <- nhanes.subset%>%
  filter(msm.adjusted != "yes")%>%
  filter(gender == "male") %>%
  mutate(sex.in.past.year.and.condomless = ifelse(male.sex.with.female.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  select(ID, survey.year, age, race, gender, age.group, gonorrhea.past.year, sex.in.past.year.and.condomless)
##Update location
#save(nhanes.hetero.males, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/First Week/NHANES/nhanes.hetero.males.RData") ##UPDATE LOCATION

nhanes.hetero.females <- nhanes.subset%>%
  filter(gender == "female")%>% #I think we are assuming anyone who is not MSM is heterosexual so that means all females?
  mutate(sex.in.past.year.and.condomless = ifelse(female.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  select(ID, survey.year, age, race, gender, age.group, gonorrhea.past.year, sex.in.past.year.and.condomless)
##Update location
#save(nhanes.hetero.females, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/First Week/NHANES/nhanes.hetero.females.RData") ##UPDATE LOCATION

