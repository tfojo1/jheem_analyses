#This code pulls LHD and Atlas Plus data from Section 2 and birth data from Section 3 to create a proportion of congenital syphilis births
#This is available at 2 geographies: state and MSA

#For state data, the proportion = (# state level congenital syphilis births from Atlas Plus) / (# state level births)
#For MSA data, the proportion = (# MSA level congenital syphilis births from LHD data) / (# MSA level births)

#source = cdc.sti (state) ; lhd (msa)
#ontology =  cdc.sti (this doesn't really matter bc it's not stratified data)

# Register New Outcomes and Sources ---------------------------------------
syphilis.manager$register.outcome(
  'births.denominator.for.congenital.syphilis.proportion',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births- Denominator for Congenital Syphilis Proportion',
    axis.name = 'Births- Denominator for Congenital Syphilis Proportion',
    units = 'births',
    description = "Births- Denominator for Congenital Syphilis Proportion"))

syphilis.manager$register.outcome(
    'proportion.of.congenital.syphilis.births',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of Congenital Syphilis Births',
        axis.name = 'Proportion of Congenital Syphilis Births',
        units = '%',
        description = "Proportion of Congenital Syphilis Births"), denominator.outcome = 'births.denominator.for.congenital.syphilis.proportion')


# First add birth data (this is from CDC Wonder, by county, state, this is the Denominator) --------

DATA.DIR.BIRTHS=Sys.getenv("BIRTH_DATA_DIR", "Q:/data_raw/syphilis.manager/births.for.congenital.syphilis.proportion")

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
    data$location = ifelse(data$State == "District of Columbia", "DC", data$location)
  }
  
  if(grepl("county", filename)) {
    data$location= str_pad(data$`County Code`, width=5, side="left", pad="0")
    data$location = as.character(data$location)
    data$flag = ifelse(grepl("Unidentified", data$County, ignore.case = T), "1", "0") #Creating flag to identify the 'unidentified counties'
    data = subset(data, data$flag != "1")#removing unidentified counties
    data = subset(data, data$location != "12025") #removing invalid location
  }
  
  data$outcome = 'births.denominator.for.congenital.syphilis.proportion'
  data$year = as.character(data$Year)
  data$value = data$Births
  
  data <- data %>% select(year, location, outcome, value)
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})
  
birth.data.clean.put = lapply(birth.data.clean, `[[`, 2)  

for (data in birth.data.clean.put) {
  
  syphilis.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Fertility Data')
}


# Aggregate births to MSA level -------------------------------------------
source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors
source('../jheem2/R/HELPERS_array_helpers.R') 

source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations

put.msa.data.as.new.source(outcome = 'births.denominator.for.congenital.syphilis.proportion',
                           from.source.name= 'cdc.wonder.natality',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           aggregate.counts.with.whatever.we.have = T,
                           data.manager= syphilis.manager)


# LHD Data Processing (For MSA Level, numerator) --------------------------

msa.congenital = as.data.frame.table(syphilis.manager$data$congenital.syphilis.diagnoses$estimate$lhd$cdc.sti$year__location, stringsAsFactors = F)%>% rename(msa.congenital.cases = Freq)%>% filter(!is.na(msa.congenital.cases))

msa.births = as.data.frame.table(syphilis.manager$data$births.denominator.for.congenital.syphilis.proportion$estimate$cdc.aggregated.county$cdc.fertility$year__location)%>% rename(count.msa.births = Freq)

all.msa.congenital.data = left_join(msa.congenital, msa.births, by = c("location", "year"))

all.msa.congenital.data <- all.msa.congenital.data %>%
    mutate(value = msa.congenital.cases/count.msa.births)%>%
    mutate(outcome = 'proportion.of.congenital.syphilis.births')


# Data Processing, pull existing data (For State Level, numerator)  ---------------------------------------------------------------

state.congenital = as.data.frame.table(syphilis.manager$data$congenital.syphilis.diagnoses$estimate$cdc.sti$cdc.sti$year__location, stringsAsFactors = F)%>% rename(state.congenital.cases = Freq)

state.births = as.data.frame.table(syphilis.manager$data$births.denominator.for.congenital.syphilis.proportion$estimate$cdc.wonder.natality$cdc.fertility$year__location, stringsAsFactors = F)%>% rename(count.state.births = Freq)

all.state.congenital.data = left_join(state.congenital, state.births, by = c("location", "year"))

all.state.congenital.data <- all.state.congenital.data %>%
    mutate(value = state.congenital.cases/count.state.births)%>%
    mutate(outcome = 'proportion.of.congenital.syphilis.births')


# Put ---------------------------------------------------------------------
#I'm going to put the MSA data separate from the state data since they have different sources

# Put state
syphilis.manager$put.long.form(
  data = all.state.congenital.data,
  ontology.name = 'cdc.sti',
  source = 'cdc.sti',
  url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
  details = 'Congenital Syphilis cases by state is from Atlas Plus (numerator of proportion); births are from CDC Wonder (denominator for proportion)')

#Put msa
syphilis.manager$put.long.form(
    data = all.msa.congenital.data,
    ontology.name = 'cdc.sti',
    source = 'lhd',
    url = 'https://health.maryland.gov/phpa/OIDPCS/CSTIP/Pages/sti-data-statistics.aspx', #There's not a single URL for the LHD data
    details = 'Congenital Syphilis cases by MSA is from various local health department websites (numerator of proportion); births are from CDC Wonder (denominator for proportion)')
