#This code processes data from the PDF CDC STI reports
#These tables are ps.syphilis diagnoses stratified by sex 
#For state, national, and MSA when available
#There is also a few reports of MSM
#This data was extracted from AI and not tabula- this is why this
#code is separate from other PDF processing codes

#==================================================================
#Read in Data Tables
#==================================================================
DATA.DIR.PDF <- file.path(
    Q_ROOT,
    "data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/sex.and.msm"
)

pdf.reports.by.sex <- Sys.glob(paste0(DATA.DIR.PDF, "/*.xlsx"))

pdf.reports.by.sex.raw <- list()

for (file in pdf.reports.by.sex) {
    
    file_name <- file_path_sans_ext(basename(file))
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
        
        sheet_name <- paste(file_name, sheet, sep = "_")
        
        pdf.reports.by.sex.raw[[sheet_name]] <- list(
            filename = sheet_name,
            data = read_excel(file, sheet = sheet, skip = 2)
        )}}

#==================================================================
#Clean
#==================================================================

pdf.reports.by.sex.clean = lapply(pdf.reports.by.sex.raw, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #LOCATION
    if(grepl("state", filename)) {
        names(state.abb) <- state.name
        data$location <- dplyr::case_when(
            data$State == "District of Columbia" ~ "DC",
            data$State == "US" ~ "US",
            TRUE ~ state.abb[data$State]
        )}
    
    if(grepl("msa", filename)) {
            
            data$location <- case_when(
                !is.na(data$MSA) & str_detect(data$MSA, "Atlanta") ~ "C.12060",
                !is.na(data$MSA) & str_detect(data$MSA, "Austin") ~ "C.12420",
                !is.na(data$MSA) & str_detect(data$MSA, "Baltimore") ~ "C.12580",
                !is.na(data$MSA) & str_detect(data$MSA, "Boston") ~ "C.14460",
                !is.na(data$MSA) & str_detect(data$MSA, "Buffalo") ~ "C.15380",
                !is.na(data$MSA) & str_detect(data$MSA, "Charlotte") ~ "C.16740",
                !is.na(data$MSA) & str_detect(data$MSA, "Chicago") ~ "C.16980",
                !is.na(data$MSA) & str_detect(data$MSA, "Cincinnati") ~ "C.17140",
                !is.na(data$MSA) & str_detect(data$MSA, "Cleveland") ~ "C.17460",
                !is.na(data$MSA) & str_detect(data$MSA, "Denver") ~ "C.19740",
                !is.na(data$MSA) & str_detect(data$MSA, "Detroit") ~ "C.19820",
                !is.na(data$MSA) & str_detect(data$MSA, "Hartford") ~ "C.25540",
                !is.na(data$MSA) & str_detect(data$MSA, "Houston") ~ "C.26420",
                !is.na(data$MSA) & str_detect(data$MSA, "Indianapolis") ~ "C.26900",
                !is.na(data$MSA) & str_detect(data$MSA, "Las Vegas") ~ "C.29820",
                !is.na(data$MSA) & str_detect(data$MSA, "Los Angeles") ~ "C.31080",
                !is.na(data$MSA) & str_detect(data$MSA, "Louisville") ~ "C.31140",
                !is.na(data$MSA) & str_detect(data$MSA, "Miami") ~ "C.33100",
                !is.na(data$MSA) & str_detect(data$MSA, "Milwaukee") ~ "C.33340",
                !is.na(data$MSA) & str_detect(data$MSA, "Nashville") ~ "C.34980",
                !is.na(data$MSA) & str_detect(data$MSA, "New Orleans") ~ "C.35380",
                !is.na(data$MSA) & str_detect(data$MSA, "New York") ~ "C.35620",
                !is.na(data$MSA) & str_detect(data$MSA, "Orlando") ~ "C.36740",
                !is.na(data$MSA) & str_detect(data$MSA, "Phoenix") ~ "C.38060",
                !is.na(data$MSA) & str_detect(data$MSA, "Portland") ~ "C.38900",
                !is.na(data$MSA) & str_detect(data$MSA, "Providence") ~ "C.39300",
                !is.na(data$MSA) & str_detect(data$MSA, "Sacramento") ~ "C.40900",
                !is.na(data$MSA) & str_detect(data$MSA, "San Antonio") ~ "C.41700",
                !is.na(data$MSA) & str_detect(data$MSA, "San Diego") ~ "C.41740",
                !is.na(data$MSA) & str_detect(data$MSA, "San Francisco") ~ "C.41860",
                
                !is.na(data$MSA) & str_detect(data$MSA, "Birmingham") ~ "C.13820",
                !is.na(data$MSA) & str_detect(data$MSA, "Dallas") ~ "C.19100",
                !is.na(data$MSA) & str_detect(data$MSA, "Dayton") ~ "C.19430",
                
                !is.na(data$MSA) & str_detect(data$MSA, "Fort Worth") ~ "C.19100", #Dallas
                !is.na(data$MSA) & str_detect(data$MSA, "Jersey City") ~ "C.35620", #NYC
                !is.na(data$MSA) & str_detect(data$MSA, "Newark") ~ "C.35620", #NYC
                !is.na(data$MSA) & str_detect(data$MSA, "Norfolk") ~ "C.47260",
                !is.na(data$MSA) & str_detect(data$MSA, "Minneapolis") ~ "C.33460",
                !is.na(data$MSA) & str_detect(data$MSA, "Oakland") ~ "C.41860", #SF
                
                !is.na(data$MSA) & str_detect(data$MSA, "Omaha") ~ "C.36540",
                !is.na(data$MSA) & str_detect(data$MSA, "Philadelphia") ~ "C.37980",
                
                !is.na(data$MSA) & str_detect(data$MSA, "San Jose") ~ "C.41940",
                !is.na(data$MSA) & str_detect(data$MSA, "Seattle") ~ "C.42660",
                !is.na(data$MSA) & str_detect(data$MSA, "St Louis") ~ "C.41180",
                !is.na(data$MSA) & str_detect(data$MSA, "Tampa") ~ "C.45300",
                
                !is.na(data$MSA) & str_detect(data$MSA, "St Paul") ~ "C.33460", #Part of Minneapolis
                !is.na(data$MSA) & str_detect(data$MSA, "St Petersburg") ~ "C.45300", #Part of Tampa
                !is.na(data$MSA) & str_detect(data$MSA, "Washington") ~ "C.47900",
                !is.na(data$MSA) & str_detect(data$MSA, "Raleigh") ~ "C.39580",
                
                !is.na(data$MSA) & str_detect(data$MSA, "Yonkers") ~ "remove",
                
                TRUE ~ as.character(locations::get.location.code(data$MSA, "CBSA"))
            )}
    
    data <- data %>%
        subset(data$location != "remove")
     

    #STRATA
    if(grepl("_women", filename)) {
        data$sex = "female"
    }
    if(grepl("_men", filename)) {
        data$sex = "male"
    }
    if(grepl("_msm", filename)) {
        data$sex = "male"
        data$risk = "msm"
    }
    
  #Pull most recent report of each year:
    
    report_year <- suppressWarnings(as.numeric(str_extract(filename, "\\d{4}")))
    
    # Make all Cases columns numeric (non-numeric text becomes NA)
    data <- data %>%
        mutate(
            across(
                starts_with("Cases "),
                ~ suppressWarnings(as.numeric(.))
            )
        )
    
    # Pull the most recent report of each year of data
    if (report_year != 2023) {
        
        case_year <- report_year - 4
        
        data <- data %>%
            mutate(
                year = case_year,
                value = .data[[paste0("Cases ", case_year)]]
            ) %>%
            select(-starts_with("Cases "))
    }
    
    # Pivot 2023 to select multiple years of data - will need to change this if we get more recent data
    if (report_year == 2023) {
        
        data <- data %>%
            pivot_longer(
                cols = starts_with("Cases "),
                names_to = "year",
                names_prefix = "Cases ",
                values_to = "value"
            ) %>%
            mutate(
                year = as.numeric(year)
            )}

    
    #Sum together (Oakland + SF), (Newark + Jersey City + NYC), (Dallas + Forth Worth)
    if(grepl("msa", filename)) {
    data <- data %>%
        group_by(location)%>%
        mutate(value.new = sum(value))
    }
        
    
    
    
    data$outcome = "ps.syphilis.diagnoses"
    data$year = as.character(data$year)
    data= as.data.frame(data)
    
    list(filename, data)
})

#==================================================================
#Put
#==================================================================

pdf.reports.by.sex.clean.put = lapply(pdf.reports.by.sex.clean, `[[`, 2)

for (data in pdf.reports.by.sex.clean.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.pdf.report',
        source = 'cdc.sti.surveillance.reports',
        url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/07/1997-Surveillance-Report.pdf',
        details = 'CDC STI Surveillance Reports')
}