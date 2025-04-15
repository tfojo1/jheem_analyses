#This is the processing for the national Sexually Transmitted Disease Surveillance PDF Reports


# Read in excel files -----------------------------------------------------

DATA.DIR.CDC.REPORTS="Q:/data_raw/syphilis.manager/cdc.syphilis.reports/table.a2.2003.2017"

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
  data$year = str_sub(filename, -9, -6)
  data$sex = 'male'

  data <- data %>%
    select(Disease, location, year, sex, `Total Male`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Male')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))%>%
    mutate(outcome = paste(outcome, ".diagnoses"))%>%
    mutate(outcome = gsub(" ", "", outcome))

  
data= as.data.frame(data)

list(filename, data)
})

#female
primary.secondary.female= lapply(cdc.reports.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = "US"
  data$year = str_sub(filename, -9, -6)
  data$sex = 'female'
  
  data <- data %>%
    select(Disease, location, year, sex, `Total Female`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Female')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))%>%
    mutate(outcome = paste(outcome, ".diagnoses"))%>%
    mutate(outcome = gsub(" ", "", outcome))
  
  
  data= as.data.frame(data)
  
  list(filename, data)
})

#total
primary.secondary.total = lapply(cdc.reports.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location = "US"
  data$year = str_sub(filename, -9, -6)
  
  data <- data %>%
    select(Disease, location, year, `Total Total`)%>%
    filter(Disease == "Primary Syphilis" | Disease == 'Secondary Syphilis')%>%
    rename(outcome = Disease)%>%
    rename(value = 'Total Total')%>%
    mutate(outcome = tolower(outcome))%>%
    mutate(outcome = gsub(" ", ".", outcome))%>%
    mutate(outcome = paste(outcome, ".diagnoses"))%>%
    mutate(outcome = gsub(" ", "", outcome))
  
  
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

for (data in female.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/std/stats17/2017-STD-Surveillance-Report_CDC-clearance-9.10.18.pdf',
    details = 'CDC STI Surveillance Reports')
}

total.put = lapply(primary.secondary.total, `[[`, 2)

for (data in total.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/std/stats17/2017-STD-Surveillance-Report_CDC-clearance-9.10.18.pdf',
    details = 'CDC STI Surveillance Reports')
}