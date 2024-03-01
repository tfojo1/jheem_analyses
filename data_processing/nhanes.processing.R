library(haven)
library(dplyr)
################################################################################
##Read in NHANES Data
DATA.DIR.NHANES="Q:/data_clean/nhanes/raw_data"

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
#Proportion of Heterosexuals who report Gonorrhea in Past Year
proportion.heterosexual.with.gonorrhea.past.year <- nhanes.subset %>%
  mutate(heterosexual.indicator.one = ifelse(sexual.orientation.male == "heterosexual" | sexual.orientation.female== "heterosexual", "1", "0"))%>%
  mutate(heterosexual.indicator.two = case_when(gender == "male" & male.sex.with.female.past.year >0 & male.sex.with.female.past.year < 7777 ~ "1",
                                                gender == "female" & female.sex.with.male.past.year >0 & female.sex.with.male.past.year < 7777 ~ "1",
                                                TRUE ~ "0"))%>%
  mutate(heterosexual.indicator.final = case_when(is.na(heterosexual.indicator.one) & heterosexual.indicator.two == "1" ~ "1",
                                                   heterosexual.indicator.one == "1" & heterosexual.indicator.two == "1" ~ "1",
                                                   is.na(heterosexual.indicator.one) & heterosexual.indicator.two == "0" ~ "0"))%>%
  filter(heterosexual.indicator.final == "1")%>%  #Calculate recent gc for heterosexual
  filter(!is.na(gonorrhea.past.year)) %>% #REMOVING NA VALUES
  group_by(survey.year, gonorrhea.past.year)%>%
  count(gonorrhea.past.year)%>%
  rename(count.gonorrhea.past.year = n)%>%
  group_by(survey.year)%>%
  mutate(proportion.gonorrhea.past.year = round(count.gonorrhea.past.year/ sum(count.gonorrhea.past.year), digits=2))

#Proportion of MSM Reporting Casual Partner
proportion.msm.with.casual.partner <- nhanes.subset %>%
  filter(msm.adjusted == "yes")%>%
  mutate(active.condomless.msm = ifelse(male.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless.msm)) %>% #REMOVING NA VALUES
  group_by(survey.year, active.condomless.msm)%>%
  count(active.condomless.msm)%>%
  rename(count.active.condomless.msm = n)%>%
  group_by(survey.year)%>%
  mutate(proportion.active.condomless.msm = round(count.active.condomless.msm/ sum(count.active.condomless.msm), digits=2))

proportion.msm.with.casual.partner.race <- nhanes.subset %>%
  filter(msm.adjusted == "yes")%>%
  mutate(active.condomless.msm = ifelse(male.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless.msm)) %>% #REMOVING NA VALUES
  group_by(survey.year, race, active.condomless.msm)%>%
  count(active.condomless.msm)%>%
  rename(count.active.condomless.msm = n)%>%
  group_by(survey.year, race)%>%
  mutate(proportion.active.condomless.msm = round(count.active.condomless.msm/ sum(count.active.condomless.msm), digits=2))

proportion.msm.with.casual.partner.age <- nhanes.subset %>%
  filter(msm.adjusted == "yes")%>%
  mutate(active.condomless.msm = ifelse(male.sex.with.male.past.year > 1 & number.sex.wo.condom.past.year > 0, "1", "0"))%>%
  filter(!is.na(active.condomless.msm)) %>% #REMOVING NA VALUES
  group_by(survey.year, age.group, active.condomless.msm)%>%
  count(active.condomless.msm)%>%
  rename(count.active.condomless.msm = n)%>%
  group_by(survey.year, age.group)%>%
  mutate(proportion.active.condomless.msm = round(count.active.condomless.msm/ sum(count.active.condomless.msm), digits=2))

#Cumulative Proportion of Age at sex initiation
cumulative.proportion.age.at.sex.initiation.all.years <- nhanes.subset%>%
  filter(age.at.first.sex != "dont know")%>%
  filter(age.at.first.sex != "refused")%>%
  filter(age.at.first.sex < 31)%>% #Adjusted so Todd can see values in only under 30
  count(age.at.first.sex)%>%
  rename(age.at.first.sex.count=n)%>%
  mutate(proportion.age.at.first.sex.unweighted = round(age.at.first.sex.count/sum(age.at.first.sex.count), digits=2))%>%
  mutate(cumulative.proportion = cumsum(proportion.age.at.first.sex.unweighted))

cumulative.proportion.age.at.sex.initiation.by.year <- nhanes.subset%>%
  filter(survey.year == "2007-2008")%>%  #SELECT YEAR OF INTEREST- it's a lot to look at all years at once
  filter(age.at.first.sex != "dont know")%>%
  filter(age.at.first.sex != "refused")%>%
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
  mutate(proportion.count.sex.in.past.year.unweighted = round(count.sex.in.past.year/sum(count.sex.in.past.year), digits=2))

  #This is by year but it's a lot to look at
proportion.sex.in.past.year.by.age.by.year <- nhanes.subset%>%
  mutate(sex.in.past.year = case_when(male.sex.with.female.past.year > 0 & male.sex.with.female.past.year < 7777
                                      | male.sex.with.male.past.year > 0 & male.sex.with.male.past.year < 7777
                                      | female.sex.with.male.past.year > 0 & female.sex.with.male.past.year < 7777
                                      | female.sex.with.female.past.year > 0 & female.sex.with.female.past.year < 7777 ~ "1", 
                                      TRUE ~ "0"))%>%
  group_by(survey.year, age)%>%
  count(sex.in.past.year)%>%
  rename(count.sex.in.past.year = n)%>%
  mutate(proportion.count.sex.in.past.year.unweighted = round(count.sex.in.past.year/sum(count.sex.in.past.year), digits=2))

#Save

#save(nhanes.subset, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/NHANES/nhanes.subset.RData")
#save(cumulative.proportion.age.at.sex.initiation.all.years, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/NHANES/cumulative.proportion.age.at.sex.initiation.all.years.RData")

