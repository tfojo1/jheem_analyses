#This code is for processing data on HIV testing extracted from CDC PDF reports
#on data from the National HIV Behavior Surveillance System
#The tables are broken into testing by MSM or heterosexual
#This data is by MSA


###################################################################
#ADD IN DENOMINATOR VALUES
#Also need to create ontologies and sources#
###################################################################


#==================================================================
#Read in Data Tables
#==================================================================
DATA.DIR.NHBSS=file.path(Q_ROOT, "data_raw/syphilis.manager/nhbss")

nhbss.data <- Sys.glob(paste0(DATA.DIR.NHBSS, '/*.csv'))


nhbss.data.raw <- map(nhbss.files, function(file) {
    sheets <- excel_sheets(file)
    
    map(sheets, function(sheet) {
        list(
            filename = paste0(basename(file), "_", sheet),
            data = read_excel(file, sheet = sheet)
        )
    }) %>%
        set_names(paste0(basename(file), "_", sheets))
}) %>%
    flatten()

#==================================================================
#Clean
#==================================================================
nhbss.data.clean = lapply(nhbss.data.raw, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    if(grepl("sti", filename)) {
        data$oucome = "proportion.tested.for.syphilis.high.risk"
    }
    if(grepl("heterosexual", filename)) {
        data$oucome = "proportion.tested.for.hiv.high.risk"
    }
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#==================================================================
#Put
#==================================================================
# early.syphilis.put = lapply(syphilis.data.early.clean2, `[[`, 2)
# 
# for (data in early.syphilis.put) {
#     
#     data.manager$put.long.form(
#         data = data,
#         ontology.name = 'cdc.sti',
#         source = 'cdc.sti',
#         dimension.values.to.distribute = list(race=c('multiracial', 'unknown'), age=('Unknown')),
#         url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
#         details = 'CDC Atlas Plus')
# }

# 
# #Read un tabs:
# library(readxl)
# library(purrr)
# 
# DATA.DIR.NHBSS <- file.path(Q_ROOT, "data_raw/syphilis.manager/nhbss/test")
# 
# nhbss.files <- Sys.glob(file.path(DATA.DIR.NHBSS, "*.xlsx"))
# 
# nhbss.data.raw <- map(nhbss.files, function(file) {
#     sheets <- excel_sheets(file)
#     
#     map(sheets, function(sheet) {
#         read_excel(file, sheet = sheet)
#     }) %>%
#         set_names(paste0(basename(file), "_", sheets))
# }) %>%
#     flatten()
# 
# 
# ##USE THIS;
# 
# nhbss.data.raw <- map(nhbss.files, function(file) {
#     sheets <- excel_sheets(file)
#     
#     map(sheets, function(sheet) {
#         list(
#             filename = paste0(basename(file), "_", sheet),
#             data = read_excel(file, sheet = sheet)
#         )
#     }) %>%
#         set_names(paste0(basename(file), "_", sheets))
# }) %>%
#     flatten()
# 
# 
