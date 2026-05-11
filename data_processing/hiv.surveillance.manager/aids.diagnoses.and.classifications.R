#This code processes data from Atlas Plus on
#AIDS classifications (diagnoses) and AIDS deaths
#Downloaded 5-11-26
#State level; aids.diagnoses; aids.deaths; 2000-2023 for most

#=============================================================================
#Read in Data (ontology age 65+)
#=============================================================================

DATA.DIR.ATLAS.AIDS.CLASS= "Q:/data_raw/aids_diagnoses/atlas.plus"

aids.class.files <- Sys.glob(paste0(DATA.DIR.ATLAS.AIDS.CLASS, '/*.csv'))

aids.class.data <- lapply(aids.class.files, function(x){
    lines <- readLines(x, n = 50)
    header_line <- grep("Indicator", lines)[1]
    skip <- header_line - 1
    
    list(
        filename = x,
        data = read.csv(x,
                        skip = skip,
                        header = TRUE,
                        colClasses = c(FIPS = "character"))
    )
})

#=============================================================================
#Mappings
#=============================================================================

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

#=============================================================================
#Clean - Age 65+
#=============================================================================
aids.class.data.clean = lapply(aids.class.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$Geography <- trimws(gsub("[^A-Za-z ]", "", data$Geography))
    
    data <- data%>%
        #mutate(Geography = gsub("[[:punct:]]", "", Geography))%>%
        mutate(location = state.abb[match(data$Geography, state.name)])%>%
        mutate(location = ifelse(Geography  == "District of Columbia", "DC", location))%>%
        mutate(year = as.character(Year))%>%
        mutate(Cases = ifelse(Cases == "Data suppressed", NA, Cases))%>%
        mutate(value = readr::parse_number(Cases))
    
    
    if(grepl("classification", filename)) {
        data$outcome = "aids.diagnoses"
    }
    
    if(grepl("deaths", filename)) {
        data$outcome= "aids.deaths"
    }
    
    
    if(grepl("sex", filename)) {
        data$sex = tolower(data$Sex)
    }
    if(grepl("race", filename)) {
        data$race = tolower(data$`Race.Ethnicity`)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$`Transmission.Category`]
    }
    if(grepl("male", filename)) {
        data$sex = tolower(data$Sex)
    }
    if(grepl("female", filename)) {
        data$sex = tolower(data$Sex)
    }
    
    if(grepl("age", filename)) {
        data$age = paste(data$Age.Group, "years")
    }
    
    data = as.data.frame(data)
    
    list(filename, data)
    
})

#=============================================================================
#Put- Age 65+
#=============================================================================

aids.class.data.clean.put = lapply(aids.class.data.clean, `[[`, 2)  

for (data in aids.class.data.clean.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.new',
        source = 'cdc.atlas.plus.aids',
        dimension.values = list(),
        dimension.values.to.distribute = list(race=c('multiracial')),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

#=============================================================================
#Read in additional data for the same outcomes
#Certain datapoints were available by more stratified age ontology
#Age 85+
#=============================================================================

DATA.DIR.ATLAS.AIDS.CLASS.AGE2= "Q:/data_raw/aids_diagnoses/atlas.plus/separate.age.ontology"

aids.class.files.age2 <- Sys.glob(paste0(DATA.DIR.ATLAS.AIDS.CLASS.AGE2, '/*.csv'))

aids.class.data.age2 <- lapply(aids.class.files.age2, function(x){
    lines <- readLines(x, n = 50)
    header_line <- grep("Indicator", lines)[1]
    skip <- header_line - 1
    
    list(
        filename = x,
        data = read.csv(x,
                        skip = skip,
                        header = TRUE,
                        colClasses = c(FIPS = "character"))
    )
})

## 

aids.class.data.clean.age2 = lapply(aids.class.data.age2, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$Geography <- trimws(gsub("[^A-Za-z ]", "", data$Geography))
    
    data <- data %>%
        mutate(
            #Geography = gsub("[[:punct:]]", "", Geography),
            location = state.abb[match(Geography, state.name)],
            location = ifelse(Geography == "District of Columbia", "DC", location),
            year = as.character(Year),
            Cases = ifelse(Cases %in% c("Data suppressed", "Data not available"), NA, Cases),
            value = readr::parse_number(as.character(Cases))
        )
    
    
    if(grepl("classification", filename)) {
        data$outcome = "aids.diagnoses"
    }
    
    if(grepl("deaths", filename)) {
        data$outcome= "aids.deaths"
    }
    
    
    if(grepl("sex", filename)) {
        data$sex = tolower(data$Sex)
    }
    if(grepl("race", filename)) {
        data$race = tolower(data$`Race.Ethnicity`)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$`Transmission.Category`]
    }
    
    
    if(grepl("age1", filename)) {
        data = subset(data, data$Age.Group != "65+") #Removing this if it exists to accomodate prescence of the other ages
        data = subset(data, data$Age.Group != "55-64")
        
        data$age = paste(data$Age.Group, "years")
    }
    if(grepl("age2", filename)) {
        data = subset(data, data$Age.Group != "50-54")
        
        data$age = paste(data$Age.Group, "years")
    }
    
    data = as.data.frame(data)
    
    list(filename, data)
    
})

#=============================================================================
#Put- Age 85+
#=============================================================================

aids.class.data.clean.age2.put = lapply(aids.class.data.clean.age2, `[[`, 2)  

for (data in aids.class.data.clean.age2.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.new.2',
        source = 'cdc.atlas.plus.aids',
        dimension.values = list(),
        dimension.values.to.distribute = list(race=c('multiracial')),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}