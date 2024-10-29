#Putting National Level data from the Census into the Census Manager
#This is for the Syphilis Manager

#This is 2010-2023 data by age and sex

#Pulling this separately because the existing data in the Census Manager uses a combination of 
#grouped and single year age.  For SHIELD we can use grouped for all years.

DATA.DIR.NATIONAL.POP="../../data_raw/population/national"

nat.pop.files <- Sys.glob(paste0(DATA.DIR.NATIONAL.POP, '/*.xlsx'))

national.population <- lapply(nat.pop.files, function(x){
  list(filename=x, data=read_excel(x))
})


# Age -------------------------------------------------------------------
national.age = lapply(national.population, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$Age != 'Total')
  
  data<- data %>%
    select(Age,(one_of("popest_2010",  "popest_2011",  
                       "popest_2012",  "popest_2013",
                       "popest_2014", "popest_2015", 
                       "popest_2016", "popest_2017", 
                       "popest_2018","popest_2019", 
                       "popest_2020","popest_2021", 
                       "popest_2022","popest_2023")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("popest_2010",  "popest_2011",  
                               "popest_2012",  "popest_2013",
                               "popest_2014", "popest_2015", 
                               "popest_2016", "popest_2017", 
                               "popest_2018","popest_2019", 
                               "popest_2020","popest_2021", 
                               "popest_2022","popest_2023")),
                 names_to = c("outcome", 'year'),
                 names_sep = "_",
                 values_to = "value")
  
  data <- data %>%
    rename(age = Age)%>%
    mutate(outcome = "population")%>%
    mutate(location = "US")
  
  data= as.data.frame(data)
  list(filename, data) 
})

# Sex ---------------------------------------------------------------------
national.sex = lapply(national.population, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$Age == 'Total')
  
  data<- data %>%
    select(Age,(one_of("popest_male_2010", "popest_female_2010", "popest_male_2011", "popest_female_2011", 
                       "popest_male_2012", "popest_female_2012", "popest_male_2013", "popest_female_2013",
                       "popest_male_2014", "popest_female_2014", "popest_male_2015", "popest_female_2015",
                       "popest_male_2016", "popest_female_2016", "popest_male_2017", "popest_female_2017",
                       "popest_male_2018", "popest_female_2018", "popest_male_2019", "popest_female_2019",
                       "popest_male_2020", "popest_female_2020", "popest_male_2021", "popest_female_2021",
                       "popest_male_2022", "popest_female_2022", "popest_male_2023", "popest_female_2023")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("popest_male_2010", "popest_female_2010", "popest_male_2011", "popest_female_2011", 
                               "popest_male_2012", "popest_female_2012", "popest_male_2013", "popest_female_2013",
                               "popest_male_2014", "popest_female_2014", "popest_male_2015", "popest_female_2015",
                               "popest_male_2016", "popest_female_2016", "popest_male_2017", "popest_female_2017",
                               "popest_male_2018", "popest_female_2018", "popest_male_2019", "popest_female_2019",
                               "popest_male_2020", "popest_female_2020", "popest_male_2021", "popest_female_2021",
                               "popest_male_2022", "popest_female_2022", "popest_male_2023", "popest_female_2023")),
                 names_to = c("outcome", 'sex', 'year'),
                 names_sep = "_",
                 values_to = "value")
  
  data <- data %>%
    mutate(outcome = "population")%>%
    mutate(location = "US")%>%
    select(-Age)
  
  data= as.data.frame(data)
  list(filename, data) 
})

# Age+Sex -----------------------------------------------------------------
national.age.sex = lapply(national.population, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$Age != 'Total')
  
  data<- data %>%
    select(Age,(one_of("popest_male_2010", "popest_female_2010", "popest_male_2011", "popest_female_2011", 
                       "popest_male_2012", "popest_female_2012", "popest_male_2013", "popest_female_2013",
                       "popest_male_2014", "popest_female_2014", "popest_male_2015", "popest_female_2015",
                       "popest_male_2016", "popest_female_2016", "popest_male_2017", "popest_female_2017",
                       "popest_male_2018", "popest_female_2018", "popest_male_2019", "popest_female_2019",
                       "popest_male_2020", "popest_female_2020", "popest_male_2021", "popest_female_2021",
                       "popest_male_2022", "popest_female_2022", "popest_male_2023", "popest_female_2023")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("popest_male_2010", "popest_female_2010", "popest_male_2011", "popest_female_2011", 
                               "popest_male_2012", "popest_female_2012", "popest_male_2013", "popest_female_2013",
                               "popest_male_2014", "popest_female_2014", "popest_male_2015", "popest_female_2015",
                               "popest_male_2016", "popest_female_2016", "popest_male_2017", "popest_female_2017",
                               "popest_male_2018", "popest_female_2018", "popest_male_2019", "popest_female_2019",
                               "popest_male_2020", "popest_female_2020", "popest_male_2021", "popest_female_2021",
                               "popest_male_2022", "popest_female_2022", "popest_male_2023", "popest_female_2023")),
                 names_to = c("outcome", "sex", 'year'),
                 names_sep = "_",
                 values_to = "value")

    data <- data %>%
      rename(age = Age)%>%
      mutate(outcome = "population")%>%
      mutate(location = "US")
  
  data= as.data.frame(data)
  list(filename, data) 
})



# put ---------------------------------------------------------------------

#sex
national.sex.put = lapply(national.sex, `[[`, 2)

for (data in national.sex.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


#age
national.age.put = lapply(national.age, `[[`, 2)

for (data in national.age.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#age+sex
national.age.sex.put = lapply(national.age.sex, `[[`, 2)

for (data in national.age.sex.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
