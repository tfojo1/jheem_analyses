#This is for outcome = Total.Medicaid data from CMS

DATA.DIR.CMS="Q:/data_raw/medicaid/age_distribution"

medicaid.cms <- Sys.glob(paste0(DATA.DIR.CMS, '/*.csv'))

medicaid.cms.data <- lapply(medicaid.cms, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character", skip=2, nrow=52))
})

#MEDICAID
# STRATIFIED Level Data: outcome = medicaid.total -------------------------------------------------------------------
medicaid.cms.stratified = lapply(medicaid.cms.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    
    if (grepl("age", filename, fixed = TRUE)){
        data <- data %>%
            pivot_longer(cols=c("0-18", "19-26", "27-44", "45-64", "65 plus", "unknown"),
                         names_to = "age",
                         values_to = "value")}
    
    #Create year:
     data$year = str_sub(filename, 39, 42) 
     
     #Create outcome:
     data$outcome = 'medicaid.total'
     
    #Clean value:
    data$value = ifelse(data$value == "NSD", NA, data$value)
    data$value = gsub(",", "",data$value)
    data$value = as.numeric(data$value)
    
    #Clean Location:
    data$formatted.location = state.abb[match(data$location, state.name)]
    data$formatted.location = ifelse(data$location== "Puerto Rico", "PR", data$formatted.location)
    data$formatted.location = ifelse(data$location == "District of Columbia", "DC", data$formatted.location)
    data$formatted.location = ifelse(data$location == "United States", "US", data$formatted.location)
    data$location = data$formatted.location
    
    #Reformat age:
    data$age = ifelse(data$age == "65 plus", "65+", data$age)
    
    data= as.data.frame(data)
    
    list(filename, data)
})


# TOTAL Level Data: outcome = medicaid.total -------------------------------------------------------------------

medicaid.cms.total = lapply(medicaid.cms.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    colnames(data) <- tolower(colnames(data))
    data<-data %>% select(location, total)
    
    #Create year:
    data$year = str_sub(filename, 39,42) 
    
    #Create outcome:
    data$outcome = 'medicaid.total'
    
    #Clean value:
    data$value = ifelse(data$total == "NSD", NA, data$total)
    data$value = gsub(",", "",data$value)
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


# PUT: --------------------------------------------------------------------

medicaid.cms.stratified.put = lapply(medicaid.cms.stratified, `[[`, 2)

for (data in medicaid.cms.stratified.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cms',
        source = 'cms',
        dimension.values.to.distribute = list(age=c('unknown')), 
        url = 'https://www.kff.org/state-category/medicaid-chip/',
        details = 'CMS Data accessed through KFF')
        }


medicaid.cms.total.put = lapply(medicaid.cms.total, `[[`, 2)

for (data in medicaid.cms.total.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cms',
        source = 'cms',
        url = 'https://www.kff.org/state-category/medicaid-chip/',
        details = 'CMS Data accessed through KFF')
        }
