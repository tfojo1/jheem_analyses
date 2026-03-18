DATA.DIR.CDC.WONDER.DEATHS="Q:/data_raw/syphilis.manager/cdc.wonder.deaths"

wonder_files <- Sys.glob(paste0(DATA.DIR.CDC.WONDER.DEATHS, '/*.csv'))

wonder.data <- lapply(wonder_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})

# Mappings ----------------------------------------------------------------

wonder.age.mappings = c('13-24' = '13-24 years',
                          '25-34' = '25-34 years',
                          '35-44' = '35-44 years',
                          '45-54' = '45-54 years',
                          '55-64' = '55-64 years',
                          '65+' = '65+ years')

# Clean -------------------------------------------------------------------

wonder.data.clean = lapply(wonder.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$value = data$Deaths
    data$year = as.character(data$Year)
    data$outcome = "deaths"
    
    
    #Location conditionals#
    if(grepl("state", filename)) {
        data <- data%>%
        mutate(location = state.abb[match(State, state.name)],
               location = ifelse(State == "District of Columbia", "DC", location))
    }
    
    if(grepl("county", filename)) {
        data <- data%>%
        mutate(location = str_pad(County.Code, width = 5, side = "left", pad = "0"))%>%
            filter(location != "51515")%>% #Removing two historic VA counties- we also remove these in hiv surveillance manager
            filter(location != "51560")
    }
    
    #Demographic conditionals#
    
    if(grepl("sex", filename)) {
        data$sex = tolower(data$Sex)
    }
    
    if(grepl("_age", filename)) {

        data <- data %>%
            filter(Five.Year.Age.Groups != "Not Stated")%>% #There's a few people without age
            mutate(fixed.age = case_when(
                Five.Year.Age.Groups %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years") ~ "0-14 years",
                Five.Year.Age.Groups %in% c("15-19 years") ~ "15-19 years",
                Five.Year.Age.Groups %in% c("20-24 years") ~ "20-24 years",
                Five.Year.Age.Groups %in% c("25-29 years") ~ "25-29 years",
                Five.Year.Age.Groups %in% c("30-34 years") ~ "30-34 years",
                Five.Year.Age.Groups %in% c("35-39 years") ~ "35-39 years",
                Five.Year.Age.Groups %in% c("40-44 years") ~ "40-44 years",
                Five.Year.Age.Groups %in% c("45-49 years", "50-54 years") ~ "45-54 years",
                Five.Year.Age.Groups %in% c("55-59 years", "60-64 years " ) ~ "55-64 years",
                Five.Year.Age.Groups %in% c("65-69 years","70-74 years","75-79 years", "80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "65+ years"))%>%
            group_by(year, location, fixed.age)%>%
            mutate(fixed.death.count = sum(value))
    }
    if(grepl("_raceeth", filename)) {
        data$race = tolower(data$Race)
        data$race = ifelse(data$`Hispanic.Origin` == "Hispanic or Latino", "hispanic", data$race)
    }
    
    data= as.data.frame(data)
    
    list(filename, data)
    
})


# Put ---------------------------------------------------------------------

wonder.data.put = lapply(wonder.data.clean, `[[`, 2)

for (data in wonder.data.put ) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.wonder.adjusted',
        source = 'cdc_wonder',
        #dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='not stated'),
        url = 'https://wonder.cdc.gov/',
        details = 'CDC Wonder')
}

