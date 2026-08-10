#This code puts data pulled manually from older MMWR reports
#The purpose of this is to capture more data used to calculate
#The proportion of msm with ps.syphilis diagnoses

#==================================================================
#Read in Data 
#==================================================================
DATA.DIR.MMWR <- file.path(
    Q_ROOT,
    "data_raw/syphilis.manager/mmwr"
)

mmwr.files <- Sys.glob(paste0(DATA.DIR.MMWR, "/*.xlsx"))

mmwr.data <- list()

for (file in mmwr.files) {
    
    file_name <- file_path_sans_ext(basename(file))
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
        
        if (sheet == "details") next
        
        sheet_name <- paste(file_name, sheet, sep = "_")
        
        mmwr.data[[sheet_name]] <- list(
            filename = sheet_name,
            data = read_excel(file, sheet = sheet)
        )}}

#==================================================================
#Clean
#==================================================================

mmwr.data.clean = lapply(mmwr.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$year = as.character(data$year)
    
    data= as.data.frame(data)
    
    list(filename, data)
})


#==================================================================
#Put
#==================================================================

mmwr.data.clean.put = lapply(mmwr.data.clean, `[[`, 2)

for (data in mmwr.data.clean.put) {
data.manager$put.long.form(
    data = data,
    ontology.name = 'mmwr',
    source = 'mmwr',
    url = 'https://www.gapha.org/wp-content/uploads/2016/08/93-Syphilis-in-Georgia-2009-2014.pdf',
    details = 'MMWR Data Tables')
}

