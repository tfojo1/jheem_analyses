#===============================================================================
#This code processes single year age group county level population data from 
#the Census.  This single year data is only available for years 2020-2025 in a vintage.

#(Todd has other historic single year age group data in SAS files processed in a 
#separate code)

#Stratifications include: age alone; age+sex

#===============================================================================

DATA.DIR.CENSUS.SINGLE.YEAR="Q:/data_raw/population/NEW_population/data/stratified/singleyearage.sex_2020.2025.vintage.county"

census_county_single_year_age <- Sys.glob(paste0(DATA.DIR.CENSUS.SINGLE.YEAR, '/*.csv'))

census.single.year.age <- lapply(census_county_single_year_age, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})

#Create template first, then create age alone, then sex+age:
template = lapply(census.single.year.age, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Combine county and county codes into FIPS- change FIPS to 'location'
    data$county_code = as.numeric(data$COUNTY)
    data$state_code = as.numeric(data$STATE)
    data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
    data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
    data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
    data$location = data$FIPS
    
    data <- data%>%
        mutate(age = case_when(AGE == "0" ~ '< 1 year',
                               AGE == "1" ~ '1 year',
                               AGE == "85" ~ '85+ years',
                               TRUE ~ paste(AGE, "years")),
               outcome = 'population',
               year = case_when( YEAR == "2" ~ "2020",
                                 YEAR == "3" ~ "2021",
                                 YEAR == "4" ~ "2022",
                                 YEAR == "5" ~ "2023",
                                 YEAR == "6" ~ "2024",
                                 YEAR == "7" ~ "2025",
                                 YEAR == "1" ~ "remove"))%>%
        filter(year != "remove")
    
    data = subset(data, data$location != "51515") #Removing county we took out of locations package (invalid location)
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Age Alone:
census.age.alone.clean = lapply(template, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data%>%
        mutate(value = TOT_POP)%>%
        select(outcome, year, location, age, value)
    
data= as.data.frame(data)

list(filename, data)
})

#Age+Sex:
census.age.sex.clean = lapply(template, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data%>%
        pivot_longer(cols = c("TOT_MALE", "TOT_FEMALE"),
                     names_to = "sex",
                    values_to = "value"
        )%>%
        mutate(sex = tolower( substring(sex, 5)))%>%
        select(outcome, year, location, sex, age, value)

    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Put:
census.age.alone.clean.put = lapply(census.age.alone.clean, `[[`, 2)

for (data in census.age.alone.clean.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'single.year.age.census',
        source = 'census.population',
        url = 'www.census.gov',
        details = 'Census Vintage Data')
}

census.age.sex.clean.put = lapply(census.age.sex.clean, `[[`, 2)

for (data in census.age.sex.clean.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'single.year.age.census',
        source = 'census.population',
        url = 'www.census.gov',
        details = 'Census Vintage Data')
}
