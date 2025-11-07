#Source = ACS
# Outcome = medicaid.total; uninsured.total ---------------------

DATA.DIR.MEDICAID.ACS="Q:/data_raw/medicaid/total_medicaid"

medicaid.acs <- Sys.glob(paste0(DATA.DIR.MEDICAID.ACS, '/*.csv'))

medicaid.acs.data <- lapply(medicaid.acs, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character", skip=2, nrow=53))
})

#MEDICAID
# Total Level Data: outcome = medicaid.total -------------------------------------------------------------------
medicaid.acs.total = lapply(medicaid.acs.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    data<-data %>% select(location, total)
    
    #Create year:
    data$year = str_sub(filename, 37, 40) 
    
    #Create outcome:
    data$outcome = 'uninsured.total'
    
    #Clean value:
    #data$value = ifelse(data$value == "N/A", NA, data$value)
    data$value = as.numeric(data$total)
    
    #Clean Location:
    data$formatted.location = state.abb[match(data$location, state.name)]
    data$formatted.location = ifelse(data$location== "Puerto Rico", "PR", data$formatted.location)
    data$formatted.location = ifelse(data$location == "District of Columbia", "DC", data$formatted.location)
    data$formatted.location = ifelse(data$location == "United States", "US", data$formatted.location)
    data$location = data$formatted.location
    
    data= as.data.frame(data)
    
    list(filename, data)
})

# Stratified Data; outcome = medicaid.total -------------------------------------------------------------------

medicaid.acs.data.stratified = lapply(medicaid.acs.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    data<-data %>% select(-footnotes, -total)
    
    if (grepl("sex", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("male", "female"),
                         names_to = "sex",
                         values_to = "value")
    }
    if (grepl("age", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("children 0-18", "adults 19-64"),
                         names_to = "age",
                         values_to = "value")%>%
            mutate(age = str_sub(age, start=-5, end=-1))
        
        data$age = gsub(" ", "", data$age)
        
    }
    if (grepl("race", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("white", "black", "hispanic", "asian/native hawaiian or pacific islander", "american indian or alaska native", "multiple races"),
                         names_to = "race",
                         values_to = "value")
    }
    
    #Create year:
    data$year = str_sub(filename, 37, 40) 
    
    #Create outcome:
    data$outcome = 'uninsured.total'
    
    #Clean value:
    data$value = ifelse(data$value == "N/A", NA, data$value)
    data$value = as.numeric(data$value)
    
    #Clean Location:
    data$formatted.location = state.abb[match(data$location, state.name)]
    data$formatted.location = ifelse(data$location== "Puerto Rico", "PR", data$formatted.location)
    data$formatted.location = ifelse(data$location == "District of Columbia", "DC", data$formatted.location)
    data$formatted.location = ifelse(data$location == "United States", "US", data$formatted.location)
    data$location = data$formatted.location
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#MEDICAID PUT::

medicaid.acs.total.put = lapply(medicaid.acs.total, `[[`, 2)

for (data in medicaid.acs.total.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'acs',
        source = 'acs',
        url = 'https://www.kff.org/state-category/health-coverage-uninsured/',
        details = 'ACS Data from Census accessed through KFF')
}

medicaid.acs.data.stratified.put = lapply(medicaid.acs.data.stratified, `[[`, 2)

for (data in medicaid.acs.data.stratified.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'acs',
        source = 'acs',
        dimension.values.to.distribute = list(race=c('multiple races')), 
        url = 'https://www.kff.org/state-category/health-coverage-uninsured/',
        details = 'ACS Data from Census accessed through KFF') }


#UNINSURED

DATA.DIR.UNINSURED.ACS="Q:/data_raw/medicaid/total_uninsured"

uninsured.acs <- Sys.glob(paste0(DATA.DIR.UNINSURED.ACS, '/*.csv'))

uninsured.acs.data <- lapply(uninsured.acs, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character", skip=2, nrow=53))
})

#uninsured
# Total Level Data: outcome = uninsured.total -------------------------------------------------------------------
uninsured.acs.total = lapply(uninsured.acs.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    data<-data %>% select(location, total)
    
    #Create year:
    data$year = str_sub(filename, 37, 40) 
    
    #Create outcome:
    data$outcome = 'uninsured.total'
    
    #Clean value:
    #data$value = ifelse(data$value == "N/A", NA, data$value)
    data$value = as.numeric(data$total)
    
    #Clean Location:
    data$formatted.location = state.abb[match(data$location, state.name)]
    data$formatted.location = ifelse(data$location== "Puerto Rico", "PR", data$formatted.location)
    data$formatted.location = ifelse(data$location == "District of Columbia", "DC", data$formatted.location)
    data$formatted.location = ifelse(data$location == "United States", "US", data$formatted.location)
    data$location = data$formatted.location
    
    data= as.data.frame(data)
    
    list(filename, data)
})

# Stratified Data; outcome = uninsured.total -------------------------------------------------------------------

uninsured.acs.data.stratified = lapply(uninsured.acs.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    data<-data %>% select(-footnotes, -total)
    
    if (grepl("sex", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("male", "female"),
                         names_to = "sex",
                         values_to = "value")
    }
    if (grepl("age", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("children 0-18", "adults 19-64"),
                         names_to = "age",
                         values_to = "value")%>%
            mutate(age = str_sub(age, start=-5, end=-1))
        data$age = gsub(" ", "", data$age)
        
    }
    if (grepl("race", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("white", "black", "hispanic", "asian/native hawaiian or pacific islander", "american indian or alaska native", "multiple races"),
                         names_to = "race",
                         values_to = "value")
    }
    
    #Create year:
    data$year = str_sub(filename, 37, 40) 
    
    #Create outcome:
    data$outcome = 'uninsured.total'
    
    #Clean value:
    data$value = ifelse(data$value == "N/A", NA, data$value)
    data$value = as.numeric(data$value)
    
    #Clean Location:
    data$formatted.location = state.abb[match(data$location, state.name)]
    data$formatted.location = ifelse(data$location== "Puerto Rico", "PR", data$formatted.location)
    data$formatted.location = ifelse(data$location == "District of Columbia", "DC", data$formatted.location)
    data$formatted.location = ifelse(data$location == "United States", "US", data$formatted.location)
    data$location = data$formatted.location
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#UNINSURED PUT:
uninsured.acs.total.put = lapply(uninsured.acs.total, `[[`, 2)

for (data in uninsured.acs.total.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'acs',
        source = 'acs',
        url = 'https://www.kff.org/state-category/health-coverage-uninsured/',
        details = 'ACS Data from Census accessed through KFF')
}

uninsured.acs.data.stratified.put = lapply(uninsured.acs.data.stratified, `[[`, 2)

for (data in uninsured.acs.data.stratified.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'acs',
        source = 'acs',
        dimension.values.to.distribute = list(race=c('multiple races')), 
        url = 'https://www.kff.org/state-category/health-coverage-uninsured/',
        details = 'ACS Data from Census accessed through KFF') }
