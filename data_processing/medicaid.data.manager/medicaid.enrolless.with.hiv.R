DATA.DIR.MEDICAID.ENROLLEES="Q:/data_raw/medicaid/medicaid.enrollees.with.hiv"

medicaid.enrollees.file <- Sys.glob(paste0(DATA.DIR.MEDICAID.ENROLLEES, '/*.csv'))

medicaid.enrollees.plwh <- lapply(medicaid.enrollees.file, function(x){
    list(filename=x, data=read.csv(x, check.names = FALSE))
})


# Total Level Data: outcome = medicaid.enrollees.with.hiv -------------------------------------------------------------------
medicaid.enrollees.plwh.clean = lapply(medicaid.enrollees.plwh, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
  
    data$outcome = 'medicaid.enrollees.with.hiv'
    data$year = as.character(data$year)
    
    data$location = state.abb[match(data$state, state.name)]
    data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
    
    data$value = data$`medicaid only`
    
    data= as.data.frame(data)
    
    list(filename, data)
})


# Put ---------------------------------------------------------------------

medicaid.enrollees.plwh.clean.put = lapply(medicaid.enrollees.plwh.clean, `[[`, 2)

for (data in medicaid.enrollees.plwh.clean.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cms', #I didn't create an ontology for this since it's just total level
        source = 'dhhs',
        url = 'https://oig.hhs.gov/documents/evaluation/2949/OEI-05-22-00240-Complete%20Report.pdf',
        details = 'DHHS Report Office of Inspector General: One Quarter of Medicaid Enrollees with HIV May Not Have Received Critical Services in 2021') }