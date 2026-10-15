#===============================================================================

#This code processes stratified county level population data from Census.

#The datasets are organized differently:
#Single year age data is only available for 2020-2025 from a vintage.
#The rest of data stratified by age is 5 year age groups.

#Stratified datasets include: age+sex+race+eth

#Data is saved in folders by year range with files for each state

#This puts data with a 4 way stratification: age+race+sex+ethnicity

#===============================================================================


#Can i just pull everything from the most stratified file?


age.sex.race.eth.folders <- c(
    intercensal.00.10 = "Q:/data_raw/population/NEW_population/data/stratified/age.sex.raceeth/2000.2010.intercensal.county.age.sex.race.eth",
    intercensal.10.20 = "Q:/data_raw/population/NEW_population/data/stratified/age.sex.raceeth/2010.2020.intercensal.age.sex.race.eth",
    vintage.20.25 = "Q:/data_raw/population/NEW_population/data/stratified/age.sex.raceeth/2020.2025.vintage.county.age.sex.race.eth"
)

all.age.sex.race.eth.data <- unlist(
    lapply(names(age.sex.race.eth.folders), function(folder_name) {
        
        folder_path <- age.sex.race.eth.folders[[folder_name]]
        
        files <- list.files(
            folder_path,
            pattern = "\\.csv$",
            full.names = TRUE
        )
        
        lapply(files, function(file) {
            
            list(
                filename = paste(
                    folder_name,
                    tools::file_path_sans_ext(basename(file)),
                    sep = "_"
                ),
                data = read_csv(file)
            )
            
        }) |>
            set_names(paste(
                folder_name,
                tools::file_path_sans_ext(basename(files)),
                sep = "_"
            ))
        
    }),
    recursive = FALSE
)

#===============================================================================

#Age + Sex + Race + Eth

#===============================================================================

four.way.stratified = lapply(all.age.sex.race.eth.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Add Location:
    data <- data%>%
    mutate(STATE = str_pad(STATE, width=2, side="left", pad="0"))%>%
        mutate(COUNTY = str_pad(COUNTY, width=3, side="left", pad="0"))%>%
        mutate(location = paste(STATE, COUNTY, sep=""))
        
    #Format Years:
    if(grepl("00.10", filename)) {
        data <- data %>%
            filter(!(YEAR %in% c("1", "12", "13")))%>% #Remove April estimates
            mutate(year = case_when(YEAR == "2" ~ "2000",
                                          YEAR == "3" ~ "2001",
                                          YEAR == "4" ~ "2002",
                                          YEAR == "5" ~ "2003",
                                          YEAR == "6" ~ "2004",
                                          YEAR == "7" ~ "2005",
                                          YEAR == "8" ~ "2006",
                                          YEAR == "9" ~ "2007",
                                          YEAR == "10" ~ "2008",
                                          YEAR == "11" ~ "2009"))
    }
    if(grepl("10.20", filename)) {
        data <- data %>%
            filter(!(YEAR %in% c("1", "12")))%>% #Remove April estimates
            mutate(year = case_when(YEAR == "2" ~ "2010",
                                          YEAR == "3" ~ "2011",
                                          YEAR == "4" ~ "2012",
                                          YEAR == "5" ~ "2013",
                                          YEAR == "6" ~ "2014",
                                          YEAR == "7" ~ "2015",
                                          YEAR == "8" ~ "2016",
                                          YEAR == "9" ~ "2017",
                                          YEAR == "10" ~ "2018",
                                          YEAR == "11" ~ "2019"))
    }
    if(grepl("20.25", filename)) {
        data <- data %>%
            filter(YEAR != "1")%>% #Remove April estimates
            mutate(year = case_when(YEAR == "2" ~ "2020",
                                          YEAR == "3" ~ "2021",
                                          YEAR == "4" ~ "2022",
                                          YEAR == "5" ~ "2023",
                                          YEAR == "6" ~ "2024",
                                          YEAR == "7" ~ "2025"))
    } 
    
    #Format Age: (Note 2000-2009 is different than more recent years)
    if(grepl("20", filename)) {
        data <- data %>%
            filter(AGEGRP != "0")%>% #Remove total age group; years 2010-2025 have a different age structure for totals and age0
            mutate(age = case_when (AGEGRP == "1" ~ "0-4 years",
                                    AGEGRP == "2" ~ "5-9 years",
                                    AGEGRP == "3"~ "10-14 years",
                                    AGEGRP == "4"~ "15-19 years",
                                    AGEGRP == "5"~ "20-24 years",
                                    AGEGRP == "6" ~ "25-29 years",
                                    AGEGRP == "7" ~ "30-34 years",
                                    AGEGRP == "8" ~ "35-39 years",
                                    AGEGRP == "9" ~ "40-44 years",
                                    AGEGRP == "10" ~ "45-49 years",
                                    AGEGRP == "11" ~ "50-54 years",
                                    AGEGRP == "12" ~ "55-59 years",
                                    AGEGRP == "13" ~ "60-64 years",
                                    AGEGRP == "14" ~ "65-69 years",
                                    AGEGRP == "15" ~ "70-74 years",
                                    AGEGRP == "16" ~ "75-79 years",
                                    AGEGRP == "17" ~ "80-84 years",
                                    AGEGRP == "18" ~ "85+ years"))
    } 

    if(grepl("00.10", filename)) {
        data <- data %>%
            filter(AGEGRP != "99")%>% #Remove total age group; years 2010-2025 have a different age structure for totals and age0
            mutate(age = case_when (AGEGRP == "0" ~ "0 years",
                                    AGEGRP == "1" ~ "1-4 years",
                                    AGEGRP == "2" ~ "5-9 years",
                                    AGEGRP == "3"~ "10-14 years",
                                    AGEGRP == "4"~ "15-19 years",
                                    AGEGRP == "5"~ "20-24 years",
                                    AGEGRP == "6" ~ "25-29 years",
                                    AGEGRP == "7" ~ "30-34 years",
                                    AGEGRP == "8" ~ "35-39 years",
                                    AGEGRP == "9" ~ "40-44 years",
                                    AGEGRP == "10" ~ "45-49 years",
                                    AGEGRP == "11" ~ "50-54 years",
                                    AGEGRP == "12" ~ "55-59 years",
                                    AGEGRP == "13" ~ "60-64 years",
                                    AGEGRP == "14" ~ "65-69 years",
                                    AGEGRP == "15" ~ "70-74 years",
                                    AGEGRP == "16" ~ "75-79 years",
                                    AGEGRP == "17" ~ "80-84 years",
                                    AGEGRP == "18" ~ "85+ years"))
            
    } 
    
    #Pivot for Race/Sex:
        data <- data %>%
        select(location, year, age, NHWA_MALE, NHWA_FEMALE,
               NHBA_MALE, NHBA_FEMALE, NHIA_MALE, NHIA_FEMALE,
               NHAA_MALE, NHAA_FEMALE, NHNA_MALE, NHNA_FEMALE,
               NHTOM_MALE, NHTOM_FEMALE, 
               
               HWA_MALE, HWA_FEMALE,
               HBA_MALE, HBA_FEMALE, HIA_MALE, HIA_FEMALE,
               HAA_MALE, HAA_FEMALE, HNA_MALE, HNA_FEMALE,
               HTOM_MALE, HTOM_FEMALE
               )%>%

            pivot_longer(
                cols = -c(location, year, age),
                names_to = c("race", "sex"),
                names_sep = "_",
                values_to = "value")%>%
            
            mutate(sex = tolower(sex),
                   
                   ethnicity = case_when((race == "NHWA" | race == "NHBA" |race == "NHIA" | race == "NHAA"| race == "NHNA" | race == "NHTOM") ~ "not hispanic",
                                         (race == "HWA" | race == "HBA" |race == "HIA" | race == "HAA"| race == "HNA" | race == "HTOM") ~ "hispanic"),
                   
                   race = case_when(race == "NHWA" ~ "white",
                                    race == "NHBA" ~ "black",
                                    race == "NHIA" ~ "american indian and alaska native",
                                    race == "NHAA" ~ "asian",
                                    race == "NHNA" ~ "native hawaiian and other pacific islander",
                                    race == "NHTOM" ~ "multiracial",
                                    race == "HWA" ~ "white",
                                    race == "HBA" ~ "black",
                                    race == "HIA" ~ "american indian and alaska native",
                                    race == "HAA" ~ "asian",
                                    race == "HNA" ~ "native hawaiian and other pacific islander",
                                    race == "HTOM" ~ "multiracial"))%>%
            
            group_by(year, location, sex, age, race, ethnicity)%>%
            mutate(new.value = sum(value))%>%
            select(-value)%>%
            rename(value = new.value)

        
        #Sum together the 0 and 1-4 age groups:
        if(grepl("00.10", filename)) {
            data <- data %>%
                  mutate(age = ifelse(age == "0 years" | age == "1-4 years", "0-4 years", age))%>%
                group_by(year, location, sex, race, ethnicity, age)%>%
                mutate(new.value = sum(value))%>%
                select(-value)%>%
                rename(value = new.value)
        }
    
    data$outcome = "population"
    
    data = subset(data, data$location != "51515") #Removing county we took out of locations package (invalid location)
    
        
    data= as.data.frame(data)
    
    list(filename, data)
})

#Put separately based on estimate type:
#Create two separate lists:

stratified.intercensal <- four.way.stratified[
    grepl("intercensal", names(four.way.stratified))
]

stratified.vintage <- four.way.stratified[
    grepl("vintage", names(four.way.stratified))
]


#Put:
stratified.intercensal.put = lapply(stratified.intercensal, `[[`, 2)

for (data in stratified.intercensal.put) {

census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values.to.distribute = list(race=('multiracial')),
    url = 'www.census.gov',
    details = 'Census Intercensal Data')

}


stratified.vintage.put = lapply(stratified.vintage, `[[`, 2)

for (data in stratified.vintage.put) {

census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values.to.distribute = list(race=('multiracial')),
    url = 'www.census.gov',
    details = 'Census Vintage Data')
}


#===============================================================================
#can you just take two way from the file above?
#===============================================================================


