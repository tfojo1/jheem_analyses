#This code processes data from CDC Wonder for state level deaths (all deaths, not HIV specific)
#This mirrors how we pulled data for 'metro.deaths' and will be used in state level models

#outcomes: state.deaths and state.deaths.denominator

#data is stratified 4 ways and uses year ranges

#ontology = metro.deaths
#source = cdc_wonder

DATA.DIR.STATE.DEATH="Q:/data_raw/births_deaths/deaths/state.deaths"

state_death_files <- Sys.glob(paste0(DATA.DIR.STATE.DEATH, '/*.xlsx'))

state.death.data <- lapply(state_death_files, function(x){
    list(filename=x, data=read_excel(x))
})

# CLEAN -------------------------------------------------------------------
state.death.data.clean = lapply(state.death.data, function(file){
    
    data=file[["data"]]  
    filename = file[["filename"]]
    
    names(state.abb) <- state.name
    data$location =ifelse(data$State == "District of Columbia", "DC", state.abb[data$State]) 
    
    if(grepl("2001.2010", filename)) {
        data$year = "2001-2010"
    }
    if(grepl("2011.2020", filename)) {
        data$year = "2011-2020"
    }
    
    data$outcome= "state.deaths"
    data$race = tolower(data$Race)
    data$ethnicity = tolower(data$`Hispanic Origin`)
    data$sex = tolower(data$Sex)
    
    #Remove suppressed death values
    data = subset(data, data$Deaths != "Suppressed")
    data$value = as.numeric(data$'Deaths')
    
    data$age = data$`Five-Year Age Groups`
    data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
    
    data = subset(data, data$ethnicity != "not stated")
    data = subset(data, data$age != "Not Stated")
    
    data <- data %>%
        select(outcome, year, location, value, sex, age, race, ethnicity)

    
    data = as.data.frame(data)
    list(filename, data)  
    
})

# PUT ---------------------------------------------------------------------

state.death.data.clean.put = lapply(state.death.data.clean, `[[`, 2)

for (data in state.death.data.clean.put ) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'metro.deaths',
        source = 'cdc_wonder',
        url = 'https://wonder.cdc.gov/',
        details = 'CDC Wonder')
}


# ADD DENOMINATOR ---------------------------------------------------------

state.death.denominator = lapply(state.death.data, function(file){
    
    data=file[["data"]]  
    filename = file[["filename"]]
    
    names(state.abb) <- state.name
    data$location =ifelse(data$State == "District of Columbia", "DC", state.abb[data$State]) 
    
    if(grepl("2001.2010", filename)) {
        data$year = "2001-2010"
    }
    if(grepl("2011.2020", filename)) {
        data$year = "2011-2020"
    }
    
    data$outcome= "state.deaths.denominator"
    data$race = tolower(data$Race)
    data$ethnicity = tolower(data$`Hispanic Origin`)
    data$sex = tolower(data$Sex)
    
    #Remove suppressed death values
    data = subset(data, data$Population != "Not Applicable")
    data$value = as.numeric(data$'Population')
    
    data$age = data$`Five-Year Age Groups`
    data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
    
    data = subset(data, data$ethnicity != "not stated")
    data = subset(data, data$age != "Not Stated")
    
    data <- data %>%
        select(outcome, year, location, value, sex, age, race, ethnicity)
    
    
    data = as.data.frame(data)
    list(filename, data)  
    
})

# PUT ---------------------------------------------------------------------

state.death.denominator.put = lapply(state.death.denominator, `[[`, 2)

for (data in state.death.denominator.put ) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'metro.deaths',
        source = 'cdc_wonder',
        url = 'https://wonder.cdc.gov/',
        details = 'CDC Wonder')
}
