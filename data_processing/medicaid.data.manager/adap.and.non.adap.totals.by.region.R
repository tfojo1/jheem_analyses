library(readxl)
# Outcome = adap.clients + non.adap.clients ---------------------

DATA.DIR.MEDICAID.TOTALS="Q:/data_raw/ryan.white.pdf.tables/medicaid/region.totals"

medicaid.total.tables <- Sys.glob(paste0(DATA.DIR.MEDICAID.TOTALS, '/*.xlsx'))

medicaid.total <- lapply(medicaid.total.tables, function(x){
    list(filename=x, data=read_excel(x))
})

# clean -------------------------------------------------------------------

medicaid.total.clean = lapply(medicaid.total, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$year = as.character(data$year)
    
    data= as.data.frame(data)
    list(filename, data)
})

# Put ---------------------------------------------------------------------
# 
# medicaid.total.put = lapply(medicaid.total.clean, `[[`, 2)
# 
# for (data in medicaid.total.put) {
#     data.manager$put.long.form(
#         data = data,
#         ontology.name = 'ryan.white.pdfs',
#         source = 'ryan.white.program',
#         url = 'https://ryanwhite.hrsa.gov/data/reports',
#         details = 'Ryan White Downloaded PDF Reports')
# }



