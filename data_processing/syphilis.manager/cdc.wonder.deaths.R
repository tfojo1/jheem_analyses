DATA.DIR.CDC.WONDER.DEATHS="Q:/data_raw/syphilis.manager/cdc.wonder.deaths"

wonder_files <- Sys.glob(paste0(DATA.DIR.CDC.WONDER.DEATHS, '/*.csv'))

wonder.data <- lapply(wonder_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})

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
            mutate(fixed.age = ifelse(Five.Year.Age.Groups %in% c("85-89 years", "90-94 years", "95-99 years", "100+ years"), "85+ years", Five.Year.Age.Groups))%>%
            group_by(year, location, fixed.age)%>%
            mutate(fixed.death.count = sum(value))%>%
            select(-value)%>%
            rename(value = fixed.death.count)
    }
    
    if(grepl("bridgedrace", filename)) {
        data$race = tolower(data$Race)
        data$ethnicity = tolower(data$Hispanic.Origin)
    }
    
    if(grepl("singlerace", filename)) {
        data$original.race = tolower(data$Single.Race.6)
        data$ethnicity = tolower(data$Hispanic.Origin)

        data <- data %>%
            mutate(race = case_when(
                original.race == "american indian or alaska native" ~ "american indian or alaska native",
                original.race == "asian" ~ "asian or pacific islander",
                original.race == "black or african american" ~ "black or african american",
                original.race == "native hawaiian or other pacific islander" ~ "asian or pacific islander", #sum this
                original.race == "white" ~ "white",
                original.race == "more than one race" ~ "more than one race",
                original.race == "not available" ~ "not available"
            ))%>%
            group_by(year, location, ethnicity, race)%>%
            mutate(fixed.race.count = sum(value))%>%
            select(-value)%>%
            rename(value = fixed.race.count)
    }
    
    data= as.data.frame(data)
    
    list(filename, data)
    
})

# Put ---------------------------------------------------------------------

wonder.data.put = lapply(wonder.data.clean, `[[`, 2)

for (data in wonder.data.put ) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'census.cdc.wonder.births.deaths',
        source = 'cdc_wonder',
        dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='not stated'),
        url = 'https://wonder.cdc.gov/',
        details = 'CDC Wonder')
}

