
# Congenital Syphilis (State 2000-2022) -----------------------------------
DATA.DIR.CONGENITAL="../../data_raw/syphilis.manager/congenital_syphilis"
congenital_files <- Sys.glob(paste0(DATA.DIR.CONGENITAL, '/*.csv'))
congenital.syphilis.raw <- lapply(congenital_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

##
congenital.syphilis.clean = lapply(congenital.syphilis.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = 'congenital.syphilis'
  
  data <- data %>%
    mutate(value= ifelse(grepl("Data not available", Cases), NA,
                         ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  data= as.data.frame(data)
  
  data<-data %>% select(outcome, year, location, value)
  
  list(filename, data)
})



# Late or Unknown Duration Syphilis (State 2000-2022)  --------------------------------------

DATA.DIR.LATE="../../data_raw/syphilis.manager/unknown_or_late_duration_syphilis"
late_files <- Sys.glob(paste0(DATA.DIR.LATE, '/*.csv'))
late.syphilis.raw <- lapply(late_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

late.syphilis.clean = lapply(late.syphilis.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = 'unknown.duration.or.late.syphilis'
  
  data <- data %>%
    mutate(value= ifelse(grepl("Data not available", Cases), NA,
                         ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  data= as.data.frame(data)
  
  data<-data %>% 
    select(outcome, year, location, value)%>%
    filter(!is.na(value)) #It seems like you can pull this data pretty far back but it's NA for most earlier years
  
  list(filename, data)
})


# Put ---------------------------------------------------------------------

late_syphilis_data = lapply(late.syphilis.clean, `[[`, 2)

for (data in late_syphilis_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}

cong_syphilis_data = lapply(congenital.syphilis.clean, `[[`, 2)

for (data in cong_syphilis_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}
