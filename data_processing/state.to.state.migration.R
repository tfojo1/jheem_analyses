#This code processing state to state migration data from the Census.

DATA.DIR.IMM.STATE="../../data_raw/movement/state.to.state"

state.imm.files <- Sys.glob(paste0(DATA.DIR.IMM.STATE, '/*.xlsx'))

state.imm.data <- lapply(state.imm.files, function(x){
  list(filename=file_path_sans_ext(basename(x), compression=FALSE), data=read_excel(x, range = "A9:DO62"))
})

#Immigration
state.imm.data.clean = lapply(state.imm.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$location <- state.abb[match(data$`Current residence`, state.name)]
  
  data$year = str_sub(filename, -4, -1)
  
  data <- data %>%
    mutate(outcome= "immigration")%>%
    filter(`Current residence` != "United States")%>%
    mutate(value = `total estimate`)%>%
    mutate(location = ifelse(`Current residence` == "Puerto Rico", "PR", location))%>%
    mutate(location = ifelse(`Current residence` == "District of Columbia", "DC", location))%>%
    select(outcome, year, location, value)

  data = as.data.frame(data)
  
  list(filename, data) 
  
})

#Emigration
state.emm.data.clean = lapply(state.imm.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Current residence` != "United States") #We are going to sum the columns so this needs to be removed
 
  data = select(data, contains("estimate"))  

   data <- data %>%
     select(-`population 1 year and over estimate`, -`Same house 1 year ago estimate`, -`Same state of residence 1 year ago estimate`, -`total estimate`, -`US Island Area estimate`, -`Foreign Country estimate`, -`Total abroad estimate`)%>%
    pivot_longer(
      cols = contains("estimate"),
      names_to = "location.original",
      values_to = "estimate"
    )%>%
    mutate(location.original = gsub(" estimate", "", location.original))%>%
    mutate(estimate = ifelse(estimate == "N/A", NA, estimate))%>%
    mutate(estimate = ifelse(estimate == "N/A2", NA, estimate))%>%
    mutate(estimate = ifelse(estimate == "N/A3", NA, estimate))%>%
    mutate(estimate = as.numeric(estimate))%>%
    group_by(location.original)%>%
    mutate(value = sum(estimate, na.rm=T))

  data$year = str_sub(filename, -4, -1)
  data$outcome = "emigration"
  data$location.new <- state.abb[match(data$location.original, state.name)]
  
  data<-data%>%
  mutate(location.new = ifelse(location.original == "Puerto Rico", "PR", location.new))%>%
  mutate(location.new = ifelse(location.original == "District of Columbia", "DC", location.new))%>%
  mutate(location.new = ifelse(location.original == "PR", "PR", location.new))%>%
  mutate(location.new = ifelse(location.original == "DC", "DC", location.new))%>%
  ungroup()%>%
  select(-location.original)%>%
  rename(location = location.new)%>%
  select(outcome, year, location, value)
  
  data<- data[!duplicated(data), ]
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})

#Put

state.imm.data.clean.put = lapply(state.imm.data.clean, `[[`, 2)  

for (data in state.imm.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census', #Note: the census.immigration ontology has year ranges (MSA data) but this data has single years (state data)
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}

state.emm.data.clean.put = lapply(state.emm.data.clean, `[[`, 2)  

for (data in state.emm.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}



