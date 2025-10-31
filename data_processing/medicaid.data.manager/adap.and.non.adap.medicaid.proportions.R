
# Outcome = proportion.nonadap.rw.clients.on.medicaid ---------------------

DATA.DIR.MEDICAID.NONADAP="Q:/data_raw/ryan.white.pdf.tables/medicaid/non.adap.medicaid.proportion"

medicaid.non.adap.tables <- Sys.glob(paste0(DATA.DIR.MEDICAID.NONADAP, '/*.csv'))

medicaid.non.adap <- lapply(medicaid.non.adap.tables, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character"))
})


###
medicaid.non.adap.proportion = lapply(medicaid.non.adap, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data %>%
        select(-contains('count'))%>%
        pivot_longer(cols = contains("percent"),
                     names_to = "location",
                     values_to = "value")%>%
        filter(`health care coverage` == "Medicaid")
    
    data$location = gsub("\\D", "", data$location) 
    data$location = paste("rw.region.", data$location)
    data$location = gsub(" ", "", data$location)

    data$outcome = 'proportion.nonadap.rw.clients.on.medicaid'
    data$value = as.numeric(data$value)
    data$value = (data$value/100)
    
    data$year = str_sub(filename, -8 ,-5) 
    
    data= as.data.frame(data)
    list(filename, data)
})

# ###
# medicaid.non.adap.proportion.put = lapply(medicaid.non.adap.proportion, `[[`, 2)
# 
# for (data in medicaid.non.adap.proportion.put) {
#     data.manager$put.long.form(
#         data = data,
#         ontology.name = 'ryan.white.pdfs',
#         source = 'ryan.white.program',
#         url = 'https://ryanwhite.hrsa.gov/data/reports',
#         details = 'Ryan White Downloaded PDF Reports')
# }


# Outcome = proportion.adap.rw.clients.on.medicaid ---------------------

DATA.DIR.MEDICAID.ADAP="Q:/data_raw/ryan.white.pdf.tables/medicaid/adap.medicaid.proportion"

medicaid.adap.tables <- Sys.glob(paste0(DATA.DIR.MEDICAID.ADAP, '/*.csv'))

medicaid.adap <- lapply(medicaid.adap.tables, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character"))
})

###
medicaid.adap.proportion = lapply(medicaid.adap, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data %>%
        select(-contains('count'))%>%
        pivot_longer(cols = contains("percent"),
                     names_to = "location",
                     values_to = "value")%>%
        filter(`health care coverage` == "Medicaid")
    
    data$location = gsub("\\D", "", data$location) 
    data$location = paste("rw.region.", data$location)
    data$location = gsub(" ", "", data$location)
    
    data$outcome = 'proportion.adap.rw.clients.on.medicaid'
    data$value = as.numeric(data$value)
    data$value = (data$value/100)
    
    data$year = str_sub(filename, -8 ,-5) 
    
    data= as.data.frame(data)
    list(filename, data)
})

# ###
# medicaid.adap.proportion.put = lapply(medicaid.adap.proportion, `[[`, 2)
# 
# for (data in medicaid.adap.proportion.put) {
#     data.manager$put.long.form(
#         data = data,
#         ontology.name = 'ryan.white.pdfs',
#         source = 'ryan.white.program',
#         url = 'https://ryanwhite.hrsa.gov/data/reports',
#         details = 'Ryan White Downloaded PDF Reports')
# }
