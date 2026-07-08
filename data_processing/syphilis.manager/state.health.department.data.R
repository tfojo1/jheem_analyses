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

#Group together races
if(grepl("_race", filename)) {
    data<- data%>%
        mutate(race = str_to_lower((race)))%>%
        mutate(race= case_when(race == "american indian or alaska native" ~ "other",
                            race == "asian" ~ "other",
                            race == "multi race" ~ "other",
                            race == "native hawaiian or other pacific islander" ~ "other",
                            race == "unknown race" ~ "other",
                            race == "white, non-hispanic" ~ "white",
                               
                            race == "asian + native hawaiian/pacific islander + american indian/alaska native" ~ "other",
                               
                            race == "american indian/alaska native" ~ "other",
                            race == "asian/pacific islander/native hawaiian" ~ "other",
                            race == "other race/not specified" ~ "other",
                            
                            race == "native hawaiian/other pacific islander" ~ "other",
                            race == "asian/pacific islander" ~ "other",
        
                            TRUE ~ race ))%>%
        
                   group_by(outcome, year, race)%>%
                   mutate(new.value = sum(value, na.rm = T))%>%
                     select(-value)%>%
         rename(value = new.value)
}

#Group together age
if (grepl("_age", filename)) {
    
    group_vars <- c("outcome", "year", "age")
    
    if ("sex" %in% names(data)) {
        group_vars <- c("outcome", "year", "sex", "age")
    }
    
        data<- data %>%
        group_by(across(all_of(group_vars))) %>%
        mutate(new.value = sum(value, na.rm = TRUE)) %>%
        select(-value) %>%
        rename(value = new.value)%>%
        mutate(age = paste(age, "years"))
}

if (grepl("sex", filename)) {
 data$sex = tolower(data$sex)
 data <- data%>%
     filter(sex != "unknown sex")
}

    data = as.data.frame(data)
    
    list(filename, data) 
    
})

# Put ---------------------------------------------------------------------
#SHOULD THIS BE A SEPARATE ONTOLOGY?

state.health.dept.data.clean.put = lapply(state.health.dept.data.clean, `[[`, 2)  

for (data in state.health.dept.data.clean.put) {

    data.manager$put.long.form(
        data = data,
        ontology.name = 'state.health.dept',
        source = 'lhd',
        dimension.values.to.distribute = list(age = 'Unknown Age years'), #don't need to redistribute race bc i put it all in other
        dimension.values = list(),
        url = 'na',
        details = 'Data pulled from State Health Department websites')
}
