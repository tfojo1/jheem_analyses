#This code sums the births from the prenatal care cdc wonder data
#This data will be used to help aggregate prenatal screening data from county to MSA
#because it's a proportion it requires a denominator
#This data is only for ages 15-44

#Calculate totals for: total, age, race/eth, age+race/eth

#This pulls cleaned datasets from the 'prenatal.care.cdc.wonder' code


# TOTAL -------------------------------------------------------------------
total.prenatal.screening = lapply(clean.total.prenatal, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(year, location, Births)%>%
    group_by(year, location)%>%
    mutate(value = sum(Births))%>%
    mutate(outcome = 'prenatal.screening.denominator')
  
  data= as.data.frame(data)
  
  list(filename, data)
})

total.prenatal.screening.put = lapply(total.prenatal.screening, `[[`, 2)

for (data in total.prenatal.screening.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

# AGE ---------------------------------------------------------------------
age.prenatal.screening = lapply(prenatal.age.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(year, location, age, Births)%>%
    group_by(year, location, age)%>%
    mutate(value = sum(Births))%>%
    mutate(outcome = 'prenatal.screening.denominator')
  
  data= as.data.frame(data)
  
  list(filename, data)
})

age.prenatal.screening.put = lapply(age.prenatal.screening, `[[`, 2)

for (data in age.prenatal.screening.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

# RACE.ETH ----------------------------------------------------------------
race.eth.prenatal.screening = lapply(prenatal.race.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(year, location, race, ethnicity, Births)%>%
    group_by(year, location, race, ethnicity)%>%
    mutate(value = sum(Births))%>%
    mutate(outcome = 'prenatal.screening.denominator')
  
  data= as.data.frame(data)
  
  list(filename, data)
})

race.eth.prenatal.screening.put = lapply(race.eth.prenatal.screening, `[[`, 2)

for (data in race.eth.prenatal.screening.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

# AGE + RACE.ETH ----------------------------------------------------------
age.race.eth.prenatal.screening = lapply(prenatal.data.fully.stratified, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(year, location, age, race, ethnicity, Births)%>%
    group_by(year, location, age, race, ethnicity)%>%
    mutate(value = sum(Births))%>%
    mutate(outcome = 'prenatal.screening.denominator')
  
  data= as.data.frame(data)
  
  list(filename, data)
})

age.race.eth.prenatal.screening.put = lapply(age.race.eth.prenatal.screening, `[[`, 2)

for (data in age.race.eth.prenatal.screening.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}


