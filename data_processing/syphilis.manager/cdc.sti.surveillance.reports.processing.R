#This is the processing for the national Sexually Transmitted Disease Surveillance PDF Reports


# Read in excel files -----------------------------------------------------

DATA.DIR.CDC.REPORTS="../../data_raw/syphilis.manager/cdc.syphilis.reports"

cdc.reports.files <- Sys.glob(paste0(DATA.DIR.CDC.REPORTS, '/*.xlsx'))

cdc.reports.raw <- lapply(cdc.reports.files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})


# Clean -------------------------------------------------------------------
#male
primary.secondary.male= lapply(cdc.reports.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = "US"
  data$year = substr(filename, 54, 57)
  data$sex = 'male'

  data <- data %>%
    select(Disease, location, year, sex, `Total Male`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Male')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))

  
data= as.data.frame(data)

list(filename, data)
})

#female
primary.secondary.female= lapply(cdc.reports.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = "US"
  data$year = substr(filename, 54, 57)
  data$sex = 'female'
  
  data <- data %>%
    select(Disease, location, year, sex, `Total Female`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Female')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))
  
  
  data= as.data.frame(data)
  
  list(filename, data)
})

#total
primary.secondary.total = lapply(cdc.reports.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = "US"
  data$year = substr(filename, 54, 57)
  
  data <- data %>%
    select(Disease, location, year, `Total Total`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Total')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))
  
  
  data= as.data.frame(data)
  
  list(filename, data)
})


# Put ---------------------------------------------------------------------

male.put = lapply(primary.secondary.male, `[[`, 2)

for (data in male.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/std/stats17/2017-STD-Surveillance-Report_CDC-clearance-9.10.18.pdf',
    details = 'CDC STI Surveillance Reports')
}

female.put = lapply(primary.secondary.female, `[[`, 2)

for (data in male.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/std/stats17/2017-STD-Surveillance-Report_CDC-clearance-9.10.18.pdf',
    details = 'CDC STI Surveillance Reports')
}
