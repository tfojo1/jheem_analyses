

#===============================================================================

#This code processes postcensal death estimates by county from the Census
#Total level for years 2000-2025

#===============================================================================

DATA.DIR.CENSUS.DEATHS="Q:/data_raw/population/NEW_population/data/deaths"

census_deaths_files <- Sys.glob(paste0(DATA.DIR.CENSUS.DEATHS, '/*.csv'))

census.deaths <- lapply(census_deaths_files, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})


census.deaths.clean = lapply(census.deaths, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]

    
    #Combine county and county codes into FIPS- change FIPS to 'location'
    data= subset(data, data$COUNTY != "0")   #Removes state level
    data$county_code = as.numeric(data$COUNTY)
    data$state_code = as.numeric(data$STATE)
    data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
    data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
    data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
    data$location = data$FIPS
    
    #Pivot:
    data<- data %>%
        select(location,contains("DEATHS"))%>%
        rename_with(~ sub("(\\d{4})$", "_\\1", .x))%>% #insert _ between death and year
        pivot_longer(cols=c( contains("DEATHS")),
                     names_to = c("outcome", "year"),
                     names_sep = "_",
                     values_to = "value")%>%
        mutate(outcome = case_when(outcome == "DEATHS" ~ "deaths"))
    
    #Replace older estimates with newer estimates for overlapping years:
    if(grepl("2000.2010", filename)) {
        data = subset(data, data$year != "2010")
    }
    if(grepl("2010.2020", filename)) {
        data = subset(data, data$year != "2020")
    }
    
    data = subset(data, data$location != "51515") #Removing county we took out of locations package (invalid location)
    
    data= as.data.frame(data)
    
    list(filename, data)
})

census.deaths.clean.put = lapply(census.deaths.clean, `[[`, 2)

for (data in census.deaths.clean.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.deaths',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Postcensal Death Estimates')
}
