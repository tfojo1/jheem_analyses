#===============================================================================

#This code processes Population data from the Census

#Including total county level data for 2000-2025

#The older estimates are intercensal and the newer estimates are from a vintage (2020-2025)

#Data was downloaded from the Census website 10-12-16. 

#===============================================================================



#===============================================================================
#County Level Total Data for 2000-2009

#This file has a different format than recent years
#===============================================================================

DATA.DIR.2000.CENSUS="Q:/data_raw/population/NEW_population/data/total"

census_county_2000 <- Sys.glob(paste0(DATA.DIR.2000.CENSUS, '/*.csv'))

census.2000 <- lapply(census_county_2000, function(x){
    list(filename=x, data=read.csv(x, header=TRUE))
})


census.2000.clean = lapply(census.2000, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data %>% select(-POPESTIMATE2010) #This will be replaced by more recent data.

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
        select(location,contains("POPESTIMATE"))%>%
        rename_with(~ sub("(\\d{4})$", "_\\1", .x))%>% #insert _ between population and year
        pivot_longer(cols=c(contains("POPESTIMATE")),
                     names_to = c("outcome", "year"),
                     names_sep = "_",
                     values_to = "value")%>%
        mutate(outcome = case_when(outcome == "POPESTIMATE" ~ "population"))
    
    data = subset(data, data$location != "51515") #Removing county we took out of locations package (invalid location)
    
    data= as.data.frame(data)
    
    list(filename, data)
})

census.2000.clean.put = lapply(census.2000.clean, `[[`, 2)

for (data in census.2000.clean.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Intercensal Estimates')
}

#===============================================================================
#County Level Total Data for 2010-2025

#This file has a different format than recent years
#===============================================================================
DATA.DIR.CENSUS.COUNTY.TOTAL="Q:/data_raw/population/NEW_population/data/total"

census_county_total_files <- Sys.glob(paste0(DATA.DIR.CENSUS.COUNTY.TOTAL, '/*.xlsx'))

census.county.total.data <- lapply(census_county_total_files, function(x){
    list(filename=x, data=read_excel(x, skip = 4))
})

#Note: one of these datasets is a vintage and the other is intercensal but
#they share the same structure.
census.county.total.data.clean = lapply(census.county.total.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Pivot:
    data<- data %>%
        select(-contains("april"))%>% #Remove the april estimates
        pivot_longer(
            cols = -location,
            names_to = "year",
            values_to = "value"
        ) %>%
        mutate(
            location = str_remove(location, "^\\."),
            year = str_remove(year, "\\D")
        ) %>%
        select(location, year, value)%>%
        
        mutate(
            location = ifelse(location == "United States",
                              "US, US",
                              location)
        ) %>%
        separate(location, into = c("county", "state"), sep = ", ")%>%
        mutate(outcome = "population")
    
    #Remove older years of data:
    if(grepl("2010.2020", filename)) {
        data <- data %>% filter(year != "2020")
    }
    
    
#Set up location- need to find codes from names:
    data <- data %>%
        mutate(state = recode(state, !!!setNames(state.abb, state.name)))%>%
        mutate(
            state_fips = case_when(
                state == "AL" ~ "01",
                state == "AK" ~ "02",
                state == "AZ" ~ "04",
                state == "AR" ~ "05",
                state == "CA" ~ "06",
                state == "CO" ~ "08",
                state == "CT" ~ "09",
                state == "DE" ~ "10",
                state == "DC" ~ "11",
                state == "FL" ~ "12",
                state == "GA" ~ "13",
                state == "HI" ~ "15",
                state == "ID" ~ "16",
                state == "IL" ~ "17",
                state == "IN" ~ "18",
                state == "IA" ~ "19",
                state == "KS" ~ "20",
                state == "KY" ~ "21",
                state == "LA" ~ "22",
                state == "ME" ~ "23",
                state == "MD" ~ "24",
                state == "MA" ~ "25",
                state == "MI" ~ "26",
                state == "MN" ~ "27",
                state == "MS" ~ "28",
                state == "MO" ~ "29",
                state == "MT" ~ "30",
                state == "NE" ~ "31",
                state == "NV" ~ "32",
                state == "NH" ~ "33",
                state == "NJ" ~ "34",
                state == "NM" ~ "35",
                state == "NY" ~ "36",
                state == "NC" ~ "37",
                state == "ND" ~ "38",
                state == "OH" ~ "39",
                state == "OK" ~ "40",
                state == "OR" ~ "41",
                state == "PA" ~ "42",
                state == "RI" ~ "44",
                state == "SC" ~ "45",
                state == "SD" ~ "46",
                state == "TN" ~ "47",
                state == "TX" ~ "48",
                state == "UT" ~ "49",
                state == "VT" ~ "50",
                state == "VA" ~ "51",
                state == "WA" ~ "53",
                state == "WV" ~ "54",
                state == "WI" ~ "55",
                state == "WY" ~ "56",
                state == "US" ~ "US",
                TRUE ~ NA_character_
            )
        )%>%
    mutate(location = locations::get.location.code(county, "COUNTY"))

    data <- data %>%
        mutate(
            correct_fips = map2_chr(location, state_fips, ~ {
                x <- as.character(.x)
                x <- x[str_sub(x, 1, 2) == .y]

                if (length(x) > 0) x[1] else NA_character_
            })
      )%>%
        mutate(correct_fips = case_when( is.na(correct_fips) & state == "US" ~ "US",
                                         is.na(correct_fips) & location == "11001" ~ "11001",
                                         is.na(correct_fips) & county == "Doña Ana County" ~ "35013",
                                         is.na(correct_fips) & county == "Capitol Planning Region" ~ "09110",
                                         
                                         is.na(correct_fips) & county == "Fairfield County" ~ "09110", #These are the pre 2020 names but the post 2020 code - ask todd
                                         is.na(correct_fips) & county == "Hartford County" ~ "09120",
                                         is.na(correct_fips) & county == "Litchfield County" ~ "09130",
                                         is.na(correct_fips) & county == "Middlesex County" ~ "09140",
                                         
                                         is.na(correct_fips) & county == "New Haven County" ~ "09150",
                                         is.na(correct_fips) & county == "New London County" ~ "09160",
                                         is.na(correct_fips) & county == "Tolland County" ~ "09170",
                                         is.na(correct_fips) & county == "Windham County" ~ "09180",
                                         
                                         TRUE ~ correct_fips))%>%
        select(-location)%>%
        rename(location = correct_fips)

    data= as.data.frame(data)
    
    list(filename, data)
})

#===============================================================================
#Put

#Note: Putting these separately because the 'details' differ (to explain type of estimate)
#===============================================================================
put.2010.2019.intercensal = census.county.total.data.clean[[1]][[2]]
put.2020.2025.vintage = census.county.total.data.clean[[2]][[2]]


#Put: 2010-2019 (intercensal)

    census.manager$put.long.form(
        data = put.2010.2019.intercensal,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Intercensal Estimates')


#Put: 2020-2025 (vintage)

    census.manager$put.long.form(
        data = put.2020.2025.vintage,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'www.census.gov',
        details = 'Census Vintage')
