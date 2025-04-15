#This code processes data from Atlas Plus
#AIDS Classifications, by state, 2000-2007 (total and one way strata)
#This is for the state level models

#outcome = aids.diagnoses

DATA.DIR.ATLAS.AIDS.CLASS= "Q:/data_raw/aids_diagnoses/atlas.plus"

aids.class.files <- Sys.glob(paste0(DATA.DIR.ATLAS.AIDS.CLASS, '/*.csv'))

aids.class.data <- lapply(aids.class.files, function(x){
    skip=8
    list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})


# Mapping -----------------------------------------------------------------

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

# CLEAN -------------------------------------------------------------------

aids.class.data.clean = lapply(aids.class.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data%>%
    mutate(outcome = 'aids.diagnoses')%>%
    mutate(location = state.abb[match(data$Geography, state.name)])%>%
    mutate(location = ifelse(Geography  == "District of Columbia", "DC", location))%>%
    mutate(year = as.character(Year))%>%
    mutate(value = as.numeric(gsub(",", "", Cases)))
    
    if(grepl("_sex", filename)) {
    data$sex = tolower(data$Sex)
    }
    if(grepl("_age", filename)) {
        data$age = paste(data$Age.Group, "years")
    }
    if(grepl("_race", filename)) {
        data$race = tolower(data$`Race.Ethnicity`)
    }
    if(grepl("_risk", filename)) {
        data$risk = risk.mappings[data$`Transmission.Category`]
    }
    
     data = as.data.frame(data)
           
    list(filename, data)
   
})


# PUT ---------------------------------------------------------------------

#source = cdc.atlas.plus.aids
#ontology =  cdc.new

#DISTRIBUTE RACE IN PUT

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