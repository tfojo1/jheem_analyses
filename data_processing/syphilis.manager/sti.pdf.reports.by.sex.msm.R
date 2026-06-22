#This code processes data from the PDF CDC STI reports
#These tables are ps.syphilis diagnoses stratified by sex 
#For state, national, and MSA when available
#There is also a few reports of MSM
#This data was extracted from AI and not tabula- this is why this
#code is separate from other PDF processing codes


library(readxl)
library(purrr)
library(tools)

DATA.DIR.PDF =file.path(Q_ROOT, "data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/sex.and.msm")

pdf.reports.by.sex <- Sys.glob(paste0(DATA.DIR.PDF, '/*.xlsx'))



pdf.reports.by.sex.raw <- list()

for (file in pdf.reports.by.sex) {
    
    file_name <- file_path_sans_ext(basename(file))
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
        
        all_data[[paste(file_name, sheet, sep = "_")]] <-
            read_excel(file, sheet = sheet, skip=2)
    }
}