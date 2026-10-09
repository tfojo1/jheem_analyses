#===============================================================================

#This code processes county level total population and deaths from the Census

#===============================================================================

#Read:
DATA.DIR.CENSUS.COUNTY="Q:/data_raw/population/county_00.25"

census_county_files <- Sys.glob(paste0(DATA.DIR.CENSUS.COUNTY, '/*.csv'))

data.list.county.pop <- lapply(census_county_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})

#Clean - County Level Total Population 2000-2025:

#(Note: I am removing popestimate2010 from the 2000-2010 file and popestimate2020 from the 2010-2019 file in order
#to pull the newer estimates from more recent files.)

data.list.county = lapply(data.list.county.pop, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    if(grepl("00.10", filename)) {
        data <- data %>% select(-POPESTIMATE2010) #Remove out of date cols
    }
    
    if(grepl("10.19", filename)) {
        data <- data %>% select(-POPESTIMATE2020, -DEATHS2020) #Remove out of date cols
    }
    
    data= subset(data, data$COUNTY != "0")   #Removes state level
    
    data$county_code = as.numeric(data$COUNTY)
    data$state_code = as.numeric(data$STATE)
    data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
    data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
    
    #Combine county and county codes into FIPS- change FIPS to 'location'
    data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
    data$location = data$FIPS
    
    #Pivot:
    data<- data %>%
        select(location,contains("POPESTIMATE"), contains("DEATHS"))%>%
        rename_with(~ sub("(\\d{4})$", "_\\1", .x))%>% #insert _ between population and year
        pivot_longer(cols=c(contains("POPESTIMATE"), contains("DEATHS")),
                     names_to = c("outcome", "year"),
                     names_sep = "_",
                     values_to = "value")%>%
        mutate(outcome = case_when(outcome == "POPESTIMATE" ~ "population",
                                   outcome == "DEATHS" ~ "deaths"))

    data = subset(data, data$location != "51515") #Removing county we took out of locations package (invalid location)

    data= as.data.frame(data)
    
    list(filename, data)
})
    
#===============================================================================

#Splitting into 2 datasets by outcome

#This is so they can be put separately because the parent source is different.

#===============================================================================

county.population.clean = lapply(data.list.county, function(file){
    data=file[[2]]
    filename = file[[1]]
    data= subset(data, data$outcome == "population")
    data= as.data.frame(data)
    list(filename, data)  
})

county.deaths.clean = lapply(data.list.county, function(file){
    data=file[[2]]
    filename = file[[1]]
    data= subset(data, data$outcome == "deaths")
    data= as.data.frame(data)
    list(filename, data)
})

#===============================================================================

#Put County Level Total Population Data and Deaths 2000-2025

#===============================================================================
#Population- County
county_pop = lapply(county.population.clean, `[[`, 2)

for (data in county_pop) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Reporting- 2000-2019 data are intercensal estimates; 2020-2023 data is from the Vintage 2023')
}

#Deaths- County
county_deaths = lapply(county.deaths.clean, `[[`, 2)

for (data in county_deaths) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.deaths',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Reporting')
}



#===============================================================================

#US Total Population + Deaths

#Sum together county level data to get national level data

#===============================================================================

#Population:
us.total.pop = lapply(county.population.clean, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        group_by(year)%>%
        mutate(total = sum(value))%>%
        select(-location, -value)%>%
        rename(value = total)%>%
        mutate(location = "US")
    
    data= as.data.frame(data)
    
    data<- data[!duplicated(data), ]
    
    list(filename, data)  
})

#Deaths:
us.total.deaths = lapply(county.deaths.clean, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        group_by(year)%>%
        mutate(total = sum(value))%>%
        select(-location, -value)%>%
        rename(value = total)%>%
        mutate(location = "US")
    
    data= as.data.frame(data)
    
    data<- data[!duplicated(data), ]
    
    list(filename, data) 
})


#Put:

#Population - US
us.total.pop.put = lapply(us.total.pop, `[[`, 2)

for (data in us.total.pop.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Reporting')
}


#Population - Deaths
us.total.deaths.put = lapply(us.total.deaths, `[[`, 2)

for (data in us.total.deaths.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.deaths',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Reporting')
}

