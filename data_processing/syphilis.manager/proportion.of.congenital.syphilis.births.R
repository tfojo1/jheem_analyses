#This code pulls LHD and Atlas Plus data from Section 2 and birth data from Section 3 to create a proportion of congenital syphilis births
#This is available at 2 geographies: state and MSA
#For state data, the proportion = (# state level congenital syphilis births from Atlas Plus) / (# state level births)
#For MSA data, the proportion = (# MSA level congenital syphilis births from LHD data) / (# MSA level births)

#Create outcome for births:
data.manager$register.outcome(
  'births.denominator.for.congenital.syphilis.proportion',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births- Denominator for Congenital Syphilis Proportion',
    axis.name = 'Births- Denominator for Congenital Syphilis Proportion',
    units = 'births',
    description = "Births- Denominator for Congenital Syphilis Proportion"))


# First add birth data (this is from CDC Wonder, by county, state, this is the Denominator) --------

DATA.DIR.BIRTHS="../../data_raw/syphilis.manager/births.for.congenital.syphilis.proportion"

births.files <- Sys.glob(paste0(DATA.DIR.BIRTHS, '/*.xlsx'))

birth.data <- lapply(births.files, function(x){
  list(filename=x, data=read_excel(x))})


birth.data.clean = lapply(birth.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("state", filename)) {
    data$location <- state.abb[match(data$State, state.name)]
  }
  
  if(grepl("county", filename)) {
    data$location= str_pad(data$`County Code`, width=5, side="left", pad="0")
    data$location = as.character(data$location)
  }
  
  data$outcome = 'births.denominator.for.congenital.syphilis.proportion'
  data$year = as.character(data$Year)
  data$value = data$Births
  
  data <- data %>% select(year, location, outcome, value)
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})
  
birth.data.clean.put = lapply(birth.data.clean, `[[`, 2)  

for (data in lhd.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'na',
    details = 'https://wonder.cdc.gov/natality.html')
}

# LHD Data Processing (For MSA Level, numerator) --------------------------

lhd.data.clean = lapply(lhd.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    mutate(year = as.character(year))%>%
    filter(LHD.Value != "Not Available")%>%
    mutate(value = as.numeric(LHD.Value))%>%
    filter(outcome != "cns.syphilis.diagnoses")%>%  #removing these for now
    filter(outcome != "primary.syphilis.diagnoses")%>%
    filter(outcome != "secondary.syphilis.diagnoses")
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})

lhd.data.clean.put = lapply(lhd.data.clean, `[[`, 2)  

for (data in lhd.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'lhd',
    dimension.values = list(),
    url = 'na',
    details = 'Data pulled from Local Health Department websites')
}


# Register New Outcomes and Sources ---------------------------------------
data.manager$register.outcome(
  'proportion.of.congenital.syphilis.births', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion of Congenital Syphilis Births',
    axis.name = 'Proportion of Congenital Syphilis Births',
    units = '%',
    description = "Proportion of Congenital Syphilis Births"), denominator.outcome = 'FILL IN THIS PART') 

# Pull data ---------------------------------------------------------------
#STATE
state.congenital = as.data.frame.table(syphilis.manager$data$congenital.syphilis.diagnoses$estimate$cdc.sti$cdc.sti$year__location)%>% rename(state.congenital.cases = Freq)

state.births = as.data.frame.table()

#MSA
msa.congenital = as.data.frame.table()

msa.births = as.data.frame.table(syphilis.manager$data)


# Calculate Proportion ----------------------------------------------------



# Put ---------------------------------------------------------------------

# WHAT SHOULD THESE DETAILS BE?
syphilis.manager$put.long.form(
  data = data,
  ontology.name = 'cdc.sti',
  source = '',
  url = '',
  details = '')
