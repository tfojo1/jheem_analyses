#This code is for processing data on HIV testing extracted from CDC PDF reports
#on data from the National HIV Behavior Surveillance System
#The tables are broken into testing by MSM or heterosexual
#This data is by MSA


###################################################################
#ADD IN DENOMINATOR VALUES
#Also need to create ontologies and sources#
#then also add STI data
###################################################################


#==================================================================
#Read in Data Tables
#==================================================================
DATA.DIR.NHBSS=file.path(Q_ROOT, "data_raw/syphilis.manager/nhbss")

nhbss.files <- Sys.glob(paste0(DATA.DIR.NHBSS, '/*.xlsx'))

nhbss.data.raw <- map(nhbss.files, function(file) {
    sheets <- excel_sheets(file)[-1]   # <-- skip first sheet
    
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
    
        data$outcome = "proportion.tested.for.hiv.high.risk"
        data$year = as.character(data$year)
        
        if(grepl("msa", filename)) {
            data <- data %>%
            mutate(msa.new = str_remove(msa, ",.*$"))%>%
            mutate(location.fix = case_when(
                str_detect(msa.new, regex("Miami", ignore_case = TRUE)) ~ "C.33100",
                str_detect(msa.new, regex("Philadelphia", ignore_case = TRUE)) ~ "C.37980",
                str_detect(msa.new, regex("Washington", ignore_case = TRUE)) ~ "C.47900",
                str_detect(msa.new, regex("Portland", ignore_case = TRUE)) ~ "C.38900",
                str_detect(msa.new, regex("New York", ignore_case = TRUE)) ~ "C.35620",
                str_detect(msa.new, regex("Norfolk", ignore_case = TRUE)) ~ "C.47260"
            ))

            data$location = ifelse(!is.na(data$location.fix), data$location.fix, locations::get.location.code(data$msa.new, "CBSA"))
        }

        if (!grepl("msa", filename)) {
            data$location = "US"
        }
        
        
        if(grepl("race", filename)) {
            data$race = tolower(data$race)
            # data$race <- gsub("[^[:alpha:]/ ]", "", data$race)
            # data$race <- iconv(data$race, from = "UTF-8", to = "ASCII", sub = "")
            
            data <- data %>%
                mutate(new.race = case_when(race == "american indian/alaska native" ~ "american indian/alaska native",
                                            race == "hispanic/latino" ~ "hispanic",
                                            race == "black" ~ "black",
                                            race == "black/african american" ~ "black",
                                            race == "white" ~ "white",
                                            race == "other" ~ "multiple races", #This will get redistributed in the put statement anyway
                                            race == "multiple races" ~ "multiple races", #This will get redistributed in the put statement anywat
                    
                                            
                                            race == "asian" ~ "asian/native hawaiian/other pacific islander", #These are the groups you'll combine later
                                            race == "native hawaiian/other pacific islander" ~ "asian/native hawaiian/other pacific islander",
                                            race == "asian/native hawaiian/other pacific islander" ~ "asian/native hawaiian/other pacific islander"))
            
            #Group those with asian and PI separate together so you can use one ontology:
            data <- data %>%
                mutate(
                    `tested past 12 months count` =
                        readr::parse_number(as.character(`tested past 12 months count`)),
                    total = readr::parse_number(as.character(total))
                )
            
            data <- data %>%
                group_by(
                    across(any_of(c("sex", "new.race")))
                ) %>%
                mutate(
                    sum.tested = sum(`tested past 12 months count`, na.rm = TRUE),
                    sum.total  = sum(total, na.rm = TRUE),
                    new.proportion = sum.tested / sum.total
                )
            
            }
        # if(grepl("age", filename)) {
        #     
        # }
        

        #data$value = (data$tested.past.12.months.percent / 100)
        
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#==================================================================
#Put-   REDISTRIBUTE OTHER RACE CATEGORY
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
