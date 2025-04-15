#This code processes death data from the Census.  This is total deaths (not hiv/aids specific)
#Adding this on 3-31-25 to the HIV Surveillance manager bc we need state level data
#This has state level deaths from 2010-2023
#Removing 2010 data because it doesn't represent a full year

DATA.DIR.STATE.DEATHS="Q:/data_raw/population/deaths.state.level.10.23"

state.death.files <- Sys.glob(paste0(DATA.DIR.STATE.DEATHS, '/*.csv'))

state.death.data <- lapply(state.death.files, function(x){
    list(filename=x, data=read.csv(x,  header=TRUE))
})


# clean -------------------------------------------------------------------

state.death.data.clean = lapply(state.death.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data %>%
        filter(NAME != "United States" 
               & NAME != 'Northeast Region' 
               & NAME != 'Midwest Region'
               & NAME != 'South Region' 
               & NAME != 'West Region' 
               & NAME != 'New England'
               & NAME != 'Middle Atlantic'
               & NAME != 'East North Central'
               & NAME != 'West North Central'
               & NAME != 'South Atlantic'
               & NAME != 'East South Central'
               & NAME != 'West South Central'
               & NAME != 'Mountain'
               & NAME != 'Pacific' )%>%
        select(NAME, contains("DEATHS"))%>%
        pivot_longer(
            cols = contains("DEATHS"),
            names_to = "year",
            values_to = "value")%>%
        mutate(year = as.character(gsub('DEATHS', "", year)))%>%
        mutate(location = state.abb[match(`NAME`, state.name)])%>%
        mutate(location = ifelse(`NAME` == "Puerto Rico", "PR", location))%>%
        mutate(location = ifelse(`NAME` == "District of Columbia", "DC", location))%>%
        mutate(outcome = 'deaths')%>%
        filter(year != "2010")%>% #Both 2010 and 2020 are not full years of data
        filter(year != "2020")
    
    data = as.data.frame(data)
    list(filename, data) 
})


# Put ---------------------------------------------------------------------

state.death.data.clean.put = lapply(state.death.data.clean, `[[`, 2)

for (data in state.death.data.clean.put) {

    data.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values = list(),
        url = 'https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-total.html',
        details = 'Census Population Estimates')
}
