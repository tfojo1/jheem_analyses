#This code processes data from state health departments (STI Surveillance Reports)

DATA.DIR.STATE.HEALTH=file.path(Q_ROOT, "data_raw/syphilis.manager/state.health.department/final.compiled.datasets")

shd.files <- Sys.glob(paste0(DATA.DIR.STATE.HEALTH, '/*.xlsx'))

state.health.dept.data <- list()

for (file in shd.files) {
    
    file_name <- file_path_sans_ext(basename(file))
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
        
        sheet_name <- paste(file_name, sheet, sep = "_")
        
        state.health.dept.data[[sheet_name]] <- list(
            filename = sheet_name,
            data = read_excel(file, sheet = sheet)
        )}}

# Clean -------------------------------------------------------------------

state.health.dept.data.clean = lapply(state.health.dept.data, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
data$year = as.character(data$year)
data$location = as.character(data$location)

data$value = ifelse(data$value == "NA", NA, data$value)
data$value <- as.numeric(gsub(",", "", data$value))

#Group together race for CA (Asian w PI and NA)
if(grepl("california.state_race", filename)) {
    data<- data%>%
        mutate(race= case_when(race == "Asian" ~ "asian/pacific islander/native hawaiian",
                                race == "Asian/Pacific Islander" ~ "asian/pacific islander/native hawaiian",
                                race == "Native Hawaiian/Other Pacific Islander" ~ "asian/pacific islander/native hawaiian",
                                TRUE ~ race ))%>%
                   group_by(outcome, year, race)%>%
                   mutate(new.value = sum(value))%>%
                    select(-value)%>%
        rename(value = new.value)%>%
        mutate(race = str_to_lower((race)))
}

    data = as.data.frame(data)
    
    list(filename, data) 
    
})

# Put ---------------------------------------------------------------------
#SHOULD THIS BE A SEPARATE ONTOLOGY?


###REDISTRIBUTE RACE AND AGE FOR UNKNOWN AND MULTIRACIAL#############

state.health.dept.data.clean.put = lapply(state.health.dept.data.clean, `[[`, 2)  

for (data in state.health.dept.data.clean.put) {

    data.manager$put.long.form(
        data = data,
        ontology.name = 'state.health.dept',
        source = 'lhd',
        dimension.values = list(),
        url = 'na',
        details = 'Data pulled from State Health Department websites')
}
