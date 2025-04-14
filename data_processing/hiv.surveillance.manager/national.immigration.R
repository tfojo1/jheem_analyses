#This codes puts national immigration data into the hiv surveillance manager which is later used in the syphilis manager

# TOTAL -------------------------------------------------------------------

DATA.DIR.NAT.IMM="Q:/data_raw/syphilis.manager/national.immigration"

national.immigration.files <- Sys.glob(paste0(DATA.DIR.NAT.IMM, '/*.xlsx'))

national.immigration.total <- lapply(national.immigration.files, function(x){
    list(filename=x, data=read_excel(x, sheet= 1, col_types = c("text", "text", "text", "numeric")))
})

national.immigration.total.clean = lapply(national.immigration.total, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data = subset(data, !is.na(data$value))
    
    data = as.data.frame(data)
    list(filename, data) 
})

national.total.put = lapply(national.immigration.total.clean, `[[`, 2)  

for (data in national.total.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'census.immigration.national',
        source = 'census.population',  
        dimension.values = list(),
        url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
        details = 'Geographic Mobility by Selected Characteristics in the United States')
}


# SEX ---------------------------------------------------------------------
DATA.DIR.IMM.SEX="Q:/data_raw/syphilis.manager/national.immigration/sex"

nat.imm.sex <- Sys.glob(paste0(DATA.DIR.IMM.SEX, '/*.xlsx'))

national.immigration.sex <- lapply(nat.imm.sex, function(x){
    list(filename=x, data=read_excel(x, sheet = "Data", range = "A19:B20", col_names = FALSE))
})

national.immigration.sex.clean = lapply(national.immigration.sex, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        rename(sex = '...1')%>%
        rename(value = '...2')%>%
        mutate(location = "US")%>%
        mutate(outcome = "immigration")
    
    data$value = as.numeric(gsub(",", '', data$value))
    data$sex = tolower(data$sex)
    data$year = filename
    data$year = substring(data$year,58, 61)
    
    data = as.data.frame(data)
    list(filename, data) 
    
})

national.sex.put = lapply(national.immigration.sex.clean, `[[`, 2)  

for (data in national.sex.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'census.immigration.national',
        source = 'census.population',  
        dimension.values = list(),
        url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
        details = 'Geographic Mobility by Selected Characteristics in the United States')
}

# AGE ---------------------------------------------------------------------

DATA.DIR.IMM.AGE="Q:/data_raw/syphilis.manager/national.immigration/age"

nat.imm.age <- Sys.glob(paste0(DATA.DIR.IMM.AGE, '/*.xlsx'))

national.immigration.age <- lapply(nat.imm.age, function(x){
    list(filename=x, data=read_excel(x, sheet = "Data", range = "A84:B98", col_names = FALSE))
})

national.immigration.age.clean = lapply(national.immigration.age, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        rename(age = '...1')%>%
        rename(value = '...2')%>%
        mutate(location = "US")%>%
        mutate(outcome = "immigration")
    
    data$value = as.numeric(gsub(",", '', data$value))
    
    data$age = gsub(' to ', '-', data$age)
    data$age = gsub('75 years and over', '75+ years', data$age)
    data$age = gsub(' and ', '-', data$age)
    
    data$year = filename
    data$year = substring(data$year,58, 61)
    
    data = as.data.frame(data)
    list(filename, data) 
    
})

national.age.put = lapply(national.immigration.age.clean, `[[`, 2)  

for (data in national.age.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'census.immigration.national',
        source = 'census.population',  
        dimension.values = list(),
        url = 'https://data.census.gov/table/ACSDT1Y2023.B07001?q=Residential%20Mobility',
        details = 'Geographic Mobility by Selected Characteristics in the United States')
}

