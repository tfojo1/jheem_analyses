#This code is for processing data on HIV testing extracted from CDC PDF reports
#on data from the National HIV Behavior Surveillance System
#The tables are broken into testing by MSM or heterosexual
#This data is by MSA


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
#Clean- create a template
#==================================================================
nhbss.data.clean = lapply(nhbss.data.raw, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    #message("Processing: ", filename)
    
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
            data <- data %>%
                mutate(new.race = case_when(race == "american indian/alaska native" ~ "american indian/alaska native",
                                            race == "hispanic/latino" ~ "hispanic",
                                            race == "black" ~ "black",
                                            race == "black/african american" ~ "black",
                                            race == "white" ~ "white",
                                            race == "other" ~ "other race", 
                                            race == "multiple races" ~ "other race", 
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
                )%>%
                select(-race)%>%
                rename(race = new.race)
            
            }
        if(grepl("age", filename)) { #Need to combine any age groupings that are misaligned
            data <- data %>%
                mutate(new.age = case_when (age == "18-19" ~ "18-24 years",
                                            age == "20-24" ~ "18-24 years",
                                            age == "18-24" ~ "18-24 years",
                                            age == "25-29" ~ "25-29 years",
                                            age == "30-39" ~ "30-39 years",
                                            age == "40-49" ~ "40-49 years",
                                            age == "50-60" ~ "50+ years",
                                            age == "50+" ~ "50+ years"))
            
            data <- data %>% #recalculating the age groups to combine into one ontology
                group_by(
                    across(any_of(c("sex", "new.age")))
                ) %>%
                mutate(
                    sum.tested = sum(`tested past 12 months count`, na.rm = TRUE),
                    sum.total  = sum(total, na.rm = TRUE),
                    new.proportion = sum.tested / sum.total
                )%>%
                select(-age)%>%
                rename(age = new.age)
        }
        
    data$location = as.character(data$location)
        
    data= as.data.frame(data)
    
    list(filename, data)
})

#==================================================================
#Calculation Outcome: proportion.tested.for.hiv.high.risk
#==================================================================

nhbss.data.proportion = lapply(nhbss.data.clean, function(file){
    
    data=file[[2]] 
    filename = file[[1]]

data$outcome = "proportion.tested.for.hiv.high.risk"

if ("new.proportion" %in% names(data)) {
    # use existing column
    data$value <- data$new.proportion

} else if ("tested.past.12.months.percent" %in% names(data)) {
    # create from percent
    data$value <- data$tested.past.12.months.percent / 100

} else {
    # fallback so code never crashes
    data$value <- NA
    message("No usable column in: ", filename)
}

data= as.data.frame(data)
list(filename, data)
})

#==================================================================
#Calculation Outcome: proportion.tested.for.hiv.high.risk.n (denominator)
#==================================================================
nhbss.data.denominator = lapply(nhbss.data.clean, function(file){
    
    data=file[[2]] 
    filename = file[[1]]
    
    data$outcome = "proportion.tested.for.hiv.high.risk.n"

    if ("sum.total" %in% names(data)) {
        # use existing column
        data$value <- data$sum.total
        
    } else if ("total" %in% names(data)) {
        # create from percent
        data$value <- data$total 
        
    } else {
        # fallback so code never crashes
        data$value <- NA
        message("No usable column in: ", filename)
    }
    
    data= as.data.frame(data)
    list(filename, data)
})

#==================================================================
#Put
#==================================================================
nhbss.data.proportion.put = lapply(nhbss.data.proportion, `[[`, 2)

for (data in nhbss.data.proportion.put) {

    data.manager$put.long.form(
        data = data,
        ontology.name = 'nhbs',
        source = 'nhbs',
        #dimension.values.to.distribute = list(race=c('multiple races')), #I don't think this works bc it's a proportion
        url = 'https://www.cdc.gov/hiv-data/nhbs/hiv-risk-prevention-testing-msm.html',
        details = 'National HIV Behavioral Surveillance PDF Report')
}

nhbss.data.denominator.put = lapply(nhbss.data.denominator, `[[`, 2)

for (data in nhbss.data.denominator.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'nhbs',
        source = 'nhbs',
        #dimension.values.to.distribute = list(race=c('multiple races')), #I don't think this works bc it's a proportion
        url = 'https://www.cdc.gov/hiv-data/nhbs/hiv-risk-prevention-testing-msm.html',
        details = 'National HIV Behavioral Surveillance PDF Report')
}


#==================================================================
#Add in STI Testing Data (2011 MSM only)
#==================================================================

DATA.DIR.NHBSS.STI=file.path(Q_ROOT, "data_raw/syphilis.manager/nhbss/sti")

nhbss.sti.files <- Sys.glob(paste0(DATA.DIR.NHBSS.STI, '/*.xlsx'))

nhbss.sti.data.raw <- map(nhbss.sti.files, function(file) {
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
#Clean STI Testing Data Template
#==================================================================
nhbss.sti.clean = lapply(nhbss.sti.data.raw, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    #message("Processing: ", filename)
    
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
        
    }
    if(grepl("age", filename)) { #Need to combine any age groupings that are misaligned
        data <- data %>%
            mutate(new.age = case_when (age == "18-24" ~ "18-24 years",
                                        age == "25-29" ~ "25-29 years",
                                        age == "30-39" ~ "30-39 years",
                                        age == "40-49" ~ "40-49 years",
                                        age == "50-60" ~ "50+ years",
                                        age == "50+" ~ "50+ years"))
        
    }
    
    data$location = as.character(data$location)
    
    data= as.data.frame(data)
    
    list(filename, data)
})


#==================================================================
#Outcome = proportion.tested.for.syphilis.high.risk
#==================================================================
nhbss.sti.proportion = lapply(nhbss.sti.clean, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data$outcome = "proportion.tested.for.syphilis.high.risk"
    data$value = as.numeric(data$tested.for.syphilis.past.12.months.percent/100)

    data= as.data.frame(data)
    list(filename, data)
})
    
#==================================================================
#Outcome = proportion.tested.for.syphilis.high.risk.n (denominator)
#==================================================================
    nhbss.sti.denominator = lapply(nhbss.sti.clean, function(file){
        
        data=file[[2]]
        filename = file[[1]]
        
        data$outcome = "proportion.tested.for.syphilis.high.risk.n"
        data$value = as.numeric(data$total)
        
        data= as.data.frame(data)
        list(filename, data)
    })
        
#==================================================================
#Put STI Testing Data
#==================================================================
nhbss.sti.proportion.put = lapply(nhbss.sti.proportion, `[[`, 2)

for (data in nhbss.sti.proportion.put) {

    data.manager$put.long.form(
        data = data,
        ontology.name = 'nhbs',
        source = 'nhbs',
        #dimension.values.to.distribute = list(race=c('multiple races')), #I don't think this works bc it's a proportion
        url = 'https://www.cdc.gov/hiv-data/nhbs/hiv-risk-prevention-testing-msm.html',
        details = 'National HIV Behavioral Surveillance PDF Report')
}

nhbss.sti.denominator.put = lapply(nhbss.sti.denominator, `[[`, 2)

for (data in nhbss.sti.denominator.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'nhbs',
        source = 'nhbs',
        #dimension.values.to.distribute = list(race=c('multiple races')), #I don't think this works bc it's a proportion
        url = 'https://www.cdc.gov/hiv-data/nhbs/hiv-risk-prevention-testing-msm.html',
        details = 'National HIV Behavioral Surveillance PDF Report')
}