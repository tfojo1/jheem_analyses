
DATA.DIR.MEDICAID.LOSS="Q:/data_raw/medicaid/medicaid.enrollees.losing.coverage"

medicaid.loss.data.files <- Sys.glob(paste0(DATA.DIR.MEDICAID.LOSS, '/*.csv'))

medicaid.loss.data <- lapply(medicaid.loss.data.files, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE))
})


# Clean: ------------------------------------------------------------------

medicaid.loss.data.clean = lapply(medicaid.loss.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = 'medicaid.enrollees.losing.coverage'
    data$year = as.character(data$year)
    
    data$location = state.abb[match(data$state, state.name)]
    data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
    data$location = ifelse(data$state == "United States", "US", data$location)
    
    data$value = data$`estimated number of people losing medicaid coverage by 2034`
    
    data= as.data.frame(data)
    
    list(filename, data)
})

# Put: --------------------------------------------------------------------

medicaid.loss.data.clean.put = lapply(medicaid.loss.data.clean, `[[`, 2)

for (data in medicaid.loss.data.clean.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cms', #This is just total level data so I did not create a specific ontology
        source = 'jec',
        url = 'https://www.jec.senate.gov/public/_cache/files/d5fb1359-92a6-47ac-8fae-aeffb1de2f6e/jec-fact-sheet-on-state-by-state-impacts-of-health-care-cuts.pdf',
        details = 'Report: State-by-State Data- 13.7 Million People Would Lose Health Insurance from Medicaid, ACA Cuts')
}