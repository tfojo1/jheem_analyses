#This code pulls older data from the PDF CDC STI Surveillance Reports
#Note: I am pulling the most recent report of each year of data- so data for 1993 comes
#from the 1997 report.  

# National- Total Level Data ----------------------------------------------

DATA.DIR.PDF.REPORTS="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/national.totals"

pdf.reports <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS, '/*.csv'))

cdc.pdf.reports <- lapply(pdf.reports, function(x){
  list(filename=x, data=read.csv(x, skip=1, colClasses = "character"))
})

# National- Total Level Data -clean -------------------------------------------------------------------

national.totals = lapply(cdc.pdf.reports, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= as.data.frame(data)
  
  data <- data %>%
    pivot_longer(cols = contains("X"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)

  data$year = gsub("X", "", data$year)

  data <- data %>%
     filter(State.Area == "U.S. TOTAL")%>%
    mutate(location = "US")

  if(grepl("early.latent", filename)) {
    data$outcome = 'early.syphilis.diagnoses'
  }
  if(grepl("ps", filename)) {
    data$outcome = 'ps.syphilis.diagnoses'
  }
  if(grepl("late.latent", filename)) {
    data$outcome = 'unknown.duration.or.late.syphilis.diagnoses'
  }
  if(grepl("all.stages", filename)) {
  data$outcome = 'total.syphilis.diagnoses'
  }
  
  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:
  
  if(grepl("2003", filename)) {
    data<-data%>%
      filter(year == "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      filter(year == "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      filter(year == "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      filter(year == "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      filter(year == "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      filter(year == "1994")
  }
  if(grepl("1997", filename)) {
    data<-data%>%
      filter(year == "1993")
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

# National- Total Level Data- Put ---------------------------------------------------------------------
national.totals.put = lapply(national.totals, `[[`, 2)

for (data in national.totals.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}

# MSA - Total Level Data --------------------------------------------------

DATA.DIR.PDF.REPORTS.MSA="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/msa.totals"

pdf.reports.msa <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS.MSA, '/*.csv'))

cdc.pdf.reports.msa <- lapply(pdf.reports.msa, function(x){
  list(filename=x, data=read.csv(x, skip=1, colClasses = "character"))
})

# MSA - Total Level Data-Clean --------------------------------------------------
msa.totals = lapply(cdc.pdf.reports.msa, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= as.data.frame(data)
  
  data <- data %>%
    pivot_longer(cols = contains("X"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  data$year = gsub("X", "", data$year)


  if(grepl("early.latent", filename)) {
    data$outcome = 'early.syphilis.diagnoses'
  }
  if(grepl("ps", filename)) {
    data$outcome = 'ps.syphilis.diagnoses'
  }
  if(grepl("late.latent", filename)) {
    data$outcome = 'unknown.duration.or.late.syphilis.diagnoses'
  }
  if(grepl("all.stages", filename)) {
    data$outcome = 'total.syphilis.diagnoses'
  }

  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:

  if(grepl("2003", filename)) {
    data<-data%>%
      filter(year == "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      filter(year == "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      filter(year == "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      filter(year == "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      filter(year == "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      filter(year == "1994")
  }
  if(grepl("1997", filename)) {
    data<-data%>%
      filter(year == "1993")
  }
  
  data <- data %>%
    filter(City != "U.S. CITY TOTAL") %>%
    filter(City != "TOTAL") %>%
    mutate(City = ifelse(City == "St Louis, MO", "St. Louis, MO", City))%>% #format certain locations
    mutate(City = ifelse(City == "St Paul, MN", "St. Paul, MN", City))%>%
    mutate(City = ifelse(City == "St Petersburg, FL", "St. Petersburg, FL", City))%>%
    mutate(city.name = substr(City,1,nchar(City)-4))%>%
    mutate(location.first.pass = locations::get.location.code(city.name, "CBSA"))%>%
    mutate(location.second.pass = locations::get.location.code(City, "CBSA"))%>%
    mutate(location =ifelse(grepl('c', location.first.pass),location.second.pass, location.first.pass))%>%
    
    mutate(location = ifelse(city.name == "Austin", "C.12420", location))%>%
    mutate(location = ifelse(city.name == "Charlotte", "C.16740", location))%>%
    mutate(location = ifelse(city.name == "Cleveland", "C.17460", location))%>%
    mutate(location = ifelse(city.name == "Dayton", "C.19430", location))%>%
    mutate(location = ifelse(city.name == "Miami", "C.33100", location))%>%
    mutate(location = ifelse(city.name == "Norfolk", "C.47260", location))%>%
    mutate(location = ifelse(city.name == "Philadelphia", "C.37980", location))%>%
    mutate(location = ifelse(city.name == "Portland", "C.38900", location))%>%
    mutate(location = ifelse(city.name == "Washington", "C.47900", location))%>%
    mutate(location = ifelse(city.name == "New York City", "C.35620", location))%>%
    filter(City != "Yonkers, NY")%>%
    mutate(location = as.character(location))%>%
    select(year, location, outcome, value)
  
  data= as.data.frame(data)
  list(filename, data)
})

# MSA - Total Level Data- Put --------------------------------------------------
msa.totals.put = lapply(msa.totals, `[[`, 2)

for (data in msa.totals.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}

# MSA - Stratified Data ---------------------------------------------------
DATA.DIR.PDF.REPORTS.MSA.STRATIFIED="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/msa.stratified"

pdf.reports.msa.strata <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS.MSA.STRATIFIED, '/*.csv'))

stratified.msa.raw <- lapply(pdf.reports.msa.strata, function(x){
  list(filename=x, data=read.csv(x, skip=1, colClasses = "character"))
})

# MSA Stratified Data - Clean ---------------------------------------------
stratified.msa = lapply(stratified.msa.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= as.data.frame(data)
  
  data <- data %>%
    pivot_longer(cols = contains("X"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  data$year = gsub("X", "", data$year)
  
  
  if(grepl("early.latent", filename)) {
    data$outcome = 'early.syphilis.diagnoses'
  }
  if(grepl("ps", filename)) {
    data$outcome = 'ps.syphilis.diagnoses'
  }
  if(grepl("late.latent", filename)) {
    data$outcome = 'unknown.duration.or.late.syphilis.diagnoses'
  }
  if(grepl("all.stages", filename)) {
    data$outcome = 'total.syphilis.diagnoses'
  }
  
  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:
  
  if(grepl("2003", filename)) {
    data<-data%>%
      filter(year == "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      filter(year == "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      filter(year == "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      filter(year == "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      filter(year == "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      filter(year == "1994")
  }
  if(grepl("1997", filename)) {
    data<-data%>%
      filter(year == "1993")
  }
  
  data <- data %>%
    filter(City != "U.S. CITY TOTAL") %>%
    filter(City != "TOTAL") %>%
    mutate(City = ifelse(City == "St Louis, MO", "St. Louis, MO", City))%>% #format certain locations
    mutate(City = ifelse(City == "St Paul, MN", "St. Paul, MN", City))%>%
    mutate(City = ifelse(City == "St Petersburg, FL", "St. Petersburg, FL", City))%>%
    mutate(city.name = substr(City,1,nchar(City)-4))%>%
    mutate(location.first.pass = locations::get.location.code(city.name, "CBSA"))%>%
    mutate(location.second.pass = locations::get.location.code(City, "CBSA"))%>%
    mutate(location =ifelse(grepl('c', location.first.pass),location.second.pass, location.first.pass))%>%
    
    mutate(location = ifelse(city.name == "Austin", "C.12420", location))%>%
    mutate(location = ifelse(city.name == "Charlotte", "C.16740", location))%>%
    mutate(location = ifelse(city.name == "Cleveland", "C.17460", location))%>%
    mutate(location = ifelse(city.name == "Dayton", "C.19430", location))%>%
    mutate(location = ifelse(city.name == "Miami", "C.33100", location))%>%
    mutate(location = ifelse(city.name == "Norfolk", "C.47260", location))%>%
    mutate(location = ifelse(city.name == "Philadelphia", "C.37980", location))%>%
    mutate(location = ifelse(city.name == "Portland", "C.38900", location))%>%
    mutate(location = ifelse(city.name == "Washington", "C.47900", location))%>%
    mutate(location = ifelse(city.name == "New York City", "C.35620", location))%>%
    filter(City != "Yonkers, NY")%>%
    mutate(location = as.character(location))%>%
   select(year, location, outcome, value)
  
  if(grepl("msa_female", filename)) {
  data$sex = 'female'
  }
  if(grepl("msa_male", filename)) {
    data$sex = 'male'
  }
  data= as.data.frame(data)
  list(filename, data)
})
# MSA Stratified Data - Put ---------------------------------------------
stratified.msa.put = lapply(stratified.msa, `[[`, 2)

for (data in stratified.msa.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}




# National- Stratified Data ----------------------------------------------

DATA.DIR.PDF.REPORTS.NATIONAL.STRAT="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/national.stratified/ps.syphilis_national_age_sex_race"

national.stratified.reports <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS.NATIONAL.STRAT, '/*.xlsx'))

national.stratified.one <- lapply(national.stratified.reports, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

# National - Stratified Data - Clean ---------------------------------------

#age group alone
national.age.group = lapply(national.stratified.one, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

  data <- data %>%
    rename(age = 'Age Group')%>%
    rename(value = Total)%>%
    mutate(location = "US")%>%
    mutate(outcome = 'ps.syphilis.diagnoses')%>%
    filter(age != 'Total')%>%
    mutate(age = if_else(age == '10-14 years', "0-10 years", age))%>% #Decided 2-12 to change this ontology to make it align with 0-10 ontology for other syphilis data
    select(outcome, location, value, age)

  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:
  
  if(grepl("2003", filename)) {
    data<-data%>%
      mutate(year = "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      mutate(year = "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      mutate(year = "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      mutate(year = "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      mutate(year = "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      mutate(year = "1994")
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

#Race
national.race = lapply(national.stratified.one, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    select(`White, NH Total`, `Black, NH Total`, `Hispanic Total`, `Asian Pacific Islander Total`, `American Indian Alaska Native Total`)%>%
    pivot_longer(cols = contains("Total"),
                 names_to = "race",
                 values_to = "value")%>%
    mutate(outcome = 'ps.syphilis.diagnoses')%>%
    mutate(location = "US")%>%
    mutate(race = case_when(race == "White, NH Total" ~ "white, non hispanic",
                            race == "Black, NH Total" ~ "black, non hispanic",
                            race == "Hispanic Total" ~ "hispanic",
                            race == "Asian Pacific Islander Total" ~ 'asian pacific islander',
                            race == "American Indian Alaska Native Total" ~ "american indian alaska native"))
  
  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:
  
  if(grepl("2003", filename)) {
    data<-data%>%
      mutate(year = "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      mutate(year = "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      mutate(year = "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      mutate(year = "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      mutate(year = "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      mutate(year = "1994")
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

#age, sex, race
national.age.race.sex = lapply(national.stratified.one, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    select(`Age Group`, `White, NH Male`, `Black, NH Male`, `Hispanic Male`, `Asian Pacific Islander Male`, `American Indian Alaska Native Male`,
           `White, NH Female`, `Black, NH Female`, `Hispanic Female`, `Asian Pacific Islander Female`, `American Indian Alaska Native Female`)%>%
    pivot_longer(cols = ends_with("ale"),
                 names_to = c("race"),
                 values_to = "value")%>%
    mutate(sex = tolower(word(race, -1)))%>%
    mutate(race = gsub("Male", "", race))%>%
    mutate(race = gsub("Female", "", race))%>%
    mutate(outcome = 'ps.syphilis.diagnoses')%>%
    mutate(location = "US") %>%
    rename(age = `Age Group`)%>%
    filter(age != "Total")%>%
    mutate(age = if_else(age == '10-14 years', "0-10 years", age))%>% #Decided 2-12 to change this ontology to make it align with 0-10 ontology for other syphilis data
    mutate(race = case_when(race == "White, NH " ~ "white, non hispanic",
                            race == "Black, NH " ~ "black, non hispanic",
                            race == "Hispanic " ~ "hispanic",
                            race == "Asian Pacific Islander " ~ 'asian pacific islander',
                            race == "American Indian Alaska Native " ~ "american indian alaska native"))

  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:

  if(grepl("2003", filename)) {
    data<-data%>%
      mutate(year = "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      mutate(year = "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      mutate(year = "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      mutate(year = "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      mutate(year = "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      mutate(year = "1994")
  }

  data= as.data.frame(data)
  list(filename, data)
})


# National - Stratified Data - Put ---------------------------------------

#age group
national.age.group.put = lapply(national.age.group, `[[`, 2)

for (data in national.age.group.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}

#race
national.race.put = lapply(national.race, `[[`, 2)

for (data in national.race.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}

#age race sex 
national.age.race.sex.put = lapply(national.age.race.sex, `[[`, 2)

for (data in national.age.race.sex.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}


# National - Sex Alone ----------------------------------------------------
DATA.DIR.PDF.REPORTS.NATIONAL.SEX="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/national.stratified/ps.syphilis_national_sex"

national.sex.files <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS.NATIONAL.SEX, '/*.csv'))

national.sex <- lapply(national.sex.files, function(x){
  list(filename=x, data=read.csv(x, skip=1, colClasses = "character"))
})


# National - Sex Alone - Clean --------------------------------------------
national.sex.clean = lapply(national.sex, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= as.data.frame(data)
  
  data <- data %>%
    pivot_longer(cols = contains("X"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  data$year = gsub("X", "", data$year)
  
  data <- data %>%
    filter(State.Area == "U.S. TOTAL")%>%
    mutate(location = "US")

  if(grepl("ps", filename)) {
    data$outcome = 'ps.syphilis.diagnoses'
  }
  
  if(grepl("_female", filename)) {
    data$sex = 'female'
  }
  if(grepl("_male", filename)) {
    data$sex = 'male'
  }

  #Pull the most recent report of a particular year. Ex. 2003 Report has the most recent values for 1999:
  if(grepl("2003", filename)) {
    data<-data%>%
      filter(year == "1999")
  }
  if(grepl("2002", filename)) {
    data<-data%>%
      filter(year == "1998")
  }
  if(grepl("2001", filename)) {
    data<-data%>%
      filter(year == "1997")
  }
  if(grepl("2000", filename)) {
    data<-data%>%
      filter(year == "1996")
  }
  if(grepl("1999", filename)) {
    data<-data%>%
      filter(year == "1995")
  }
  if(grepl("1998", filename)) {
    data<-data%>%
      filter(year == "1994")
  }
  if(grepl("1997", filename)) {
    data<-data%>%
      filter(year == "1993")
  }
  
  data= as.data.frame(data)
  list(filename, data)
  
  data= as.data.frame(data)
  list(filename, data)
})
# National - Sex Alone - Put --------------------------------------------
national.sex.clean.put = lapply(national.sex.clean, `[[`, 2)

for (data in national.sex.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports')
}


# National Level Neurosyphilis --------------------------------------------
#Reports for neurosyphilis are from the current year of report (not reported retrospectively like the other outcomes)
DATA.DIR.NEURO="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/neurosyphilis"

pdf.reports.neuro <- Sys.glob(paste0(DATA.DIR.NEURO, '/*.csv'))

cdc.pdf.reports.neuro <- lapply(pdf.reports.neuro, function(x){
  list(filename=x, data=read.csv(x, skip=1))
})

neurosyphilis.clean = lapply(cdc.pdf.reports.neuro, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    select(disease, `total.total`)%>%
    filter(disease == "Neurosyphilis")%>%
    mutate(location= "US")%>%
    mutate(outcome = "cns.syphilis.diagnoses")%>%
    rename(value = `total.total`)%>%
    mutate(value = as.numeric(value))
  
  if(grepl("2003", filename)) {
    data$year = "2003"
  }
  if(grepl("2002", filename)) {
    data$year = "2002"
  }
  if(grepl("2001", filename)) {
    data$year = "2001"
  }
  if(grepl("2000", filename)) {
    data$year = "2000"
  }
  if(grepl("1999", filename)) {
    data$year = "1999"
  }
  if(grepl("1998", filename)) {
    data$year = "1998"
  }
  if(grepl("1997", filename)) {
    data$year = "1997"
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

neurosyphilis.clean.put = lapply(neurosyphilis.clean, `[[`, 2)

for (data in neurosyphilis.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
    details = 'CDC STI Surveillance Reports. Neurosyphilis cases are not included with Total Syphilis cases but are included in the late and late latent syphilis cases.')
}
