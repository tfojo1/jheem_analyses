
# Upload ------------------------------------------------------------------

DATA.DIR.OLDER.SUPPRESSION="../../data_raw/sle/older.suppression.data"

older.suppression.files <- Sys.glob(paste0(DATA.DIR.OLDER.SUPPRESSION, '/*.csv'))

older.suppression <- lapply(older.suppression.files, function(x){
  list(filename=x, data=read.csv(x,  header=TRUE))
})


# Mappings ----------------------------------------------------------------

races = c('American Indian/Alaska Native' = 'American Indian/Alaska Native',
                  'Asian' = 'Asian',
                  'Black/African American' = 'Black/African American',
                  'Hispanic/Latino' = 'Hispanic/Latino',
                  'Native Hawaiian/Other Pacific Islander' = 'Native Hawaiian/Other Pacific Islander',
                  'White' = 'White',
                  'Multiple races' = 'Multiracial')

ages = c('13-24' = '13-24 years', 
         '25-34' = '25-34 years', 
         '35-44' = '35-44 years', 
         '45-54'= '45-54 years',
         '55+' = '55+ years')

# Clean -------------------------------------------------------------------

template = lapply(older.suppression, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    select(category, pct.suppressed, n, n.suppressed)%>% #use n and n.suppressed to calculate risk by group
    mutate(outcome = "suppression")%>%
    mutate(location = "US")%>%
    mutate(value = as.numeric(gsub(",", "", pct.suppressed)))%>%
    mutate(value = value/100)

  if(grepl("2010", filename)) {
    data$year = "2010"
  }
  if(grepl("2011", filename)) {
    data$year = "2011"
  }
  if(grepl("2012", filename)) {
    data$year = "2012"
  }
  if(grepl("2013", filename)) {
    data$year = "2013"
  }
  if(grepl("2014", filename)) {
    data$year = "2014"
  }
  if(grepl("2015", filename)) {
    data$year = "2015"
  }
  if(grepl("2016", filename)) {
    data$year = "2016"
  }
  if(grepl("2017", filename)) {
    data$year = "2017"
  }
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  
  list(filename, data) 
  
})

# Total ---------------------------------------------------------------------

nat.suppress.total = lapply(template, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    filter(category == 'Total')%>%
    select(year, location, outcome, value)
  
  list(filename, data) 
  
})
# Sex ---------------------------------------------------------------------

nat.suppress.sex = lapply(template, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    filter(category == 'Male' | category == 'Female')%>%
    mutate(sex = tolower(category))%>%
    select(year, location, outcome, value, sex)

  list(filename, data) 
  
})

# Race ---------------------------------------------------------------------

nat.suppress.race = lapply(template, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    filter(category == 'American Indian/Alaska Native' | category == 'Asian' | category == 'Black/African American' | category == 'Hispanic/Latino'| category == 'Native Hawaiian/Other Pacific Islander' | category == 'White' | category == 'Multiple races')%>%
    rename(race = category)%>%
    select(year, location, outcome, value, race)
  
  data$race = races[data$race]
  
  list(filename, data) 

})

# Age ---------------------------------------------------------------------

nat.suppress.age = lapply(template, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    filter(category == '13-24' | category == '25-34'| category == '35-44'| category == '45-54' | category == '55+')%>%
    mutate(age = category)%>%
    select(year, location, outcome, value, age)
  
  data$age = ages[data$age]
  
  list(filename, data) 
  
})

# Risk ---------------------------------------------------------------------

nat.suppress.risk = lapply(template, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    filter(category == 'msm' | category == 'idu.male' | category == 'idu.female' | category == 'idu.msm' | category == 'het.male' | category == 'het.female' | category == 'other.male' | category == 'other.female' )%>%
    rename(risk = category)%>%
    
    mutate(n = as.numeric(gsub(",", "", n)))%>%
    mutate(n.suppressed = as.numeric(gsub(",", "", n.suppressed)))%>%
    

    select(year, location, outcome, risk, n.suppressed, n)%>%

#Have to regroup the risk data to add filed together:
    mutate(heterosexual.total = sum(n.suppressed[risk =="het.male" | risk == 'het.female'])) %>%
    mutate(idu.total = sum(n.suppressed[risk =="idu.male" | risk == 'idu.female']))%>%
    mutate(other.total = sum(n.suppressed[risk =="other.male" | risk == 'other.female']))%>%
    mutate(msm.total = sum(n.suppressed[risk =="msm"]))%>%
    mutate(msm.idu.total = sum(n.suppressed[risk =="idu.msm"]))%>%

      mutate(heterosexual.n = sum(n[risk =="het.male" | risk == 'het.female'])) %>%
      mutate(idu.n = sum(n[risk =="idu.male" | risk == 'idu.female']))%>%
      mutate(other.n = sum(n[risk =="other.male" | risk == 'other.female']))%>%
      mutate(msm.n = sum(n[risk =="msm"]))%>%
      mutate(msm.idu.n = sum(n[risk =="idu.msm"]))%>%
  
    mutate(true.risk = c('msm', 'idu', 'msm_idu', 'heterosexual', 'other', NA, NA, NA))%>%
    mutate(regrouped.value = ifelse(true.risk == 'other', (other.total/other.n), ""))%>%
    mutate(regrouped.value = ifelse(true.risk == "heterosexual", (heterosexual.total/heterosexual.n), regrouped.value))%>%
    mutate(regrouped.value = ifelse(true.risk == "idu", (idu.total/idu.n), regrouped.value))%>%
    mutate(regrouped.value = ifelse(true.risk == "msm", (msm.total/msm.n), regrouped.value))%>%
    mutate(regrouped.value = ifelse(true.risk == "msm_idu", (msm.idu.total/msm.idu.n), regrouped.value))%>%
    select(year, location, outcome, true.risk, regrouped.value)%>%
    mutate(value = as.numeric(regrouped.value))%>%
    rename(risk = true.risk)%>%
    mutate(value = round(value, digits =2))%>%
    filter(!is.na(risk))

  
  list(filename, data) 
  
})


# Put ---------------------------------------------------------------------
#Fix on Monday: I'm not sure this is right for the ontology, idk where this data is from

nat.suppress.total.put = lapply(nat.suppress.total, `[[`, 2) 

for (data in nat.suppress.total.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}

nat.suppress.sex.put = lapply(nat.suppress.sex, `[[`, 2) 

for (data in nat.suppress.sex.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}

nat.suppress.race.put = lapply(nat.suppress.race, `[[`, 2) 

for (data in nat.suppress.race.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}

nat.suppress.age.put = lapply(nat.suppress.age, `[[`, 2) 

for (data in nat.suppress.age.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}


nat.suppress.risk.put = lapply(nat.suppress.risk, `[[`, 2)

for (data in nat.suppress.risk.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}
