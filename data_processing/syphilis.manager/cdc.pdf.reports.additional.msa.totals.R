#This code processes any remaining tables from the PDF CDC STI reports that hadn't been
#pulled previously.  This includes MSA total level data (all stages, ps, early, late, and a few congenital)
#from 2000-2019.  There is also a table of congenital syphilis in the US by race.
#This is a separate code because AI pulled the tables from the reports so the formatting is different.


DATA.DIR.ADD.MSAS =file.path(Q_ROOT, "data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/additional.msa.totals")

add.msa.files <- Sys.glob(paste0(DATA.DIR.ADD.MSAS, '/*.xlsx'))

last.msa.files.raw <- list()

for (file in add.msa.files) {
    
    file_name <- file_path_sans_ext(basename(file))
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
        
        sheet_name <- paste(file_name, sheet, sep = "_")
        
        last.msa.files.raw[[sheet_name]] <- list(
            filename = sheet_name,
            data = read_excel(file, sheet = sheet, skip =1)
        )}}


# Clean -------------------------------------------------------------------

last.msa.files.clean = lapply(last.msa.files.raw, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    names(data)[1] <- "msa"
    
    #create outcomes:
    if(grepl("congenital", filename)) {
        data$outcome = 'congenital.syphilis.diagnoses'
    }
    
    if(grepl("ps", filename)) {
        data$outcome = 'ps.syphilis.diagnoses'
    }
    
    if(grepl("early", filename)) {
        data$outcome = 'early.syphilis.diagnoses'
    }
    
    if(grepl("late", filename)) {
        data$outcome = 'unknown.duration.or.late.syphilis.diagnoses'
    }
    
    if(grepl("all.stages", filename)) {
        data$outcome = 'total.syphilis.diagnoses'
    }
    
    #Pull the newest report for the year:
    report_year <- suppressWarnings(as.numeric(str_extract(filename, "\\d{4}")))
    
    # Make all Cases columns numeric (non-numeric text becomes NA)
    data <- data %>%
        mutate(
            across(
                starts_with("Cases "),
                ~ suppressWarnings(as.numeric(.))
            )
        )
    
    case_year <- report_year - 4
    
    data <- data %>%
        mutate(
            year = case_year,
            value = .data[[paste0("Cases ", case_year)]]
        ) %>%
        select(-starts_with("Cases "))
    

    data$year = as.character(data$year)
    
    #Get location codes for MSAs:
        data <- data%>%
        mutate(location = case_when(
            !is.na(msa) & str_detect(msa, "Atlanta") ~ "C.12060",
            !is.na(msa) & str_detect(msa, "Austin") ~ "C.12420",
            !is.na(msa) & str_detect(msa, "Baltimore") ~ "C.12580",
            !is.na(msa) & str_detect(msa, "Boston") ~ "C.14460",
            !is.na(msa) & str_detect(msa, "Buffalo") ~ "C.15380",
            !is.na(msa) & str_detect(msa, "Charlotte") ~ "C.16740",
            !is.na(msa) & str_detect(msa, "Chicago") ~ "C.16980",
            !is.na(msa) & str_detect(msa, "Cincinnati") ~ "C.17140",
            !is.na(msa) & str_detect(msa, "Cleveland") ~ "C.17460",
            !is.na(msa) & str_detect(msa, "Denver") ~ "C.19740",
            !is.na(msa) & str_detect(msa, "Detroit") ~ "C.19820",
            !is.na(msa) & str_detect(msa, "Hartford") ~ "C.25540",
            !is.na(msa) & str_detect(msa, "Houston") ~ "C.26420",
            !is.na(msa) & str_detect(msa, "Indianapolis") ~ "C.26900",
            !is.na(msa) & str_detect(msa, "Las Vegas") ~ "C.29820",
            !is.na(msa) & str_detect(msa, "Los Angeles") ~ "C.31080",
            !is.na(msa) & str_detect(msa, "Louisville") ~ "C.31140",
            !is.na(msa) & str_detect(msa, "Miami") ~ "C.33100",
            !is.na(msa) & str_detect(msa, "Milwaukee") ~ "C.33340",
            !is.na(msa) & str_detect(msa, "Nashville") ~ "C.34980",
            !is.na(msa) & str_detect(msa, "New Orleans") ~ "C.35380",
            !is.na(msa) & str_detect(msa, "New York") ~ "C.35620",
            !is.na(msa) & str_detect(msa, "Orlando") ~ "C.36740",
            !is.na(msa) & str_detect(msa, "Phoenix") ~ "C.38060",
            !is.na(msa) & str_detect(msa, "Portland") ~ "C.38900",
            !is.na(msa) & str_detect(msa, "Providence") ~ "C.39300",
            !is.na(msa) & str_detect(msa, "Sacramento") ~ "C.40900",
            !is.na(msa) & str_detect(msa, "San Antonio") ~ "C.41700",
            !is.na(msa) & str_detect(msa, "San Diego") ~ "C.41740",
            !is.na(msa) & str_detect(msa, "San Francisco") ~ "C.41860",
            
            !is.na(msa) & str_detect(msa, "Birmingham") ~ "C.13820",
            !is.na(msa) & str_detect(msa, "Dallas") ~ "C.19100",
            !is.na(msa) & str_detect(msa, "Dayton") ~ "C.19430",
            
            !is.na(msa) & str_detect(msa, "Fort Worth") ~ "C.19100", #Dallas
            !is.na(msa) & str_detect(msa, "Jersey City") ~ "C.35620", #NYC
            !is.na(msa) & str_detect(msa, "Newark") ~ "C.35620", #NYC
            !is.na(msa) & str_detect(msa, "Norfolk") ~ "C.47260",
            !is.na(msa) & str_detect(msa, "Minneapolis") ~ "C.33460",
            !is.na(msa) & str_detect(msa, "Oakland") ~ "C.41860", #SF
            
            !is.na(msa) & str_detect(msa, "Omaha") ~ "C.36540",
            !is.na(msa) & str_detect(msa, "Philadelphia") ~ "C.37980",
            
            !is.na(msa) & str_detect(msa, "San Jose") ~ "C.41940",
            !is.na(msa) & str_detect(msa, "Seattle") ~ "C.42660",
            !is.na(msa) & str_detect(msa, "St Louis") ~ "C.41180",
            !is.na(msa) & str_detect(msa, "Tampa") ~ "C.45300",
            
            !is.na(msa) & str_detect(msa, "St Paul") ~ "C.33460", #Part of Minneapolis
            !is.na(msa) & str_detect(msa, "St Petersburg") ~ "C.45300", #Part of Tampa
            !is.na(msa) & str_detect(msa, "Washington") ~ "C.47900",
            !is.na(msa) & str_detect(msa, "Raleigh") ~ "C.39580",
            
            !is.na(msa) & str_detect(msa, "Yonkers") ~ "remove",
            
            TRUE ~ as.character(locations::get.location.code(msa, "CBSA"))))%>%
            
            filter(location != "remove")
        
    
    data = as.data.frame(data)
    
    list(filename, data) 
    
})

# Put ---------------------------------------------------------------------

last.msa.files.clean.put = lapply(last.msa.files.clean, `[[`, 2)

for (data in last.msa.files.clean.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.sti',
        source = 'cdc.sti.surveillance.reports',
        url = 'https://www.cdc.gov/sti-statistics/index.html',
        details = 'CDC STI Surveillance Reports')
}
