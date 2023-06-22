

library(jheem2)
library(tidyverse)


###Initialize data manager (surveillance manager) and establish ontology###

data.manager = create.data.manager('test', description='a data manager to test with')
data.manager$register.outcome(
  'new',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'New Diagnoses',
    axis.name = 'New Diagnoses (n)',
    units = 'cases',
    description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

data.manager$register.ontology(
  'cdc',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Mutiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
    ))


################################################################################
###State Level Data###

DATA.DIR.STATE.DIAGNOSES="../../data_raw/state/diagnoses"
DATA.DIR.STATE.DEATHSPREVALENCE="../../data_raw/state/deaths_prevalence"
DATA.DIR.STATE.SLE="../../data_raw/state/sle"
DATA.DIR.STATE.KNOWLEDGE="../../data_raw/state/knowledge"


atlas_state_files <- Sys.glob(paste0(DATA.DIR.STATE.DIAGNOSES, '/*.csv'))

data.list <- lapply(atlas_state_files, read.csv, skip=8, header=TRUE)





###Define and apply the 'mappings'###

outcome.mappings = c('HIV diagnoses'='new') #Removing this bc ontology should match what is in CDC said Todd


risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')


#record possible values for the incomplete dimensions, year and location
locations = c()
years = c()

### "Clean" data from Atlas Plus###

data.list.clean = lapply(data.list, function(data){
  
  # This introduces NAs for the other outcomes, but we remove that data anyways
  
  data$outcome = outcome.mappings[data$Indicator]
  data = data[!is.na(data$outcome),]
  
  names(data)[names(data)=='Year'] = 'year'   
  data$year = substring(data$year,1, 4)                                          
  data$year = as.character(data$year)
 
  names(state.abb) <- state.name 
  names(data)[names(data)=='Geography'] = 'state'
  data$location = state.abb[data$state]                                         
  
  data$location[data$state %in% c("District of Columbia")] = "DC"
  
 # names(data)[names(data)=='Sex'] = 'sex'
# data$sex = tolower(data$sex)
  
 # data$risk = risk.mappings[data$Transmission.Category]                         
        
  names(data)[names(data)=='Race.Ethnicity'] = 'race.ethnicity'
 data$race.ethnicity = tolower(data$race.ethnicity)
  
  # MIGHT NEED TO CONVERT SOME TO NA
  data$Cases[data$Cases %in% c("Data suppressed")] = NA                          
  data$value = as.numeric(gsub(",", '', data$Cases))     
  
  #Check how this should be coded#
  data$rate = (data$Rate.per.100000)
  data$rate[data$Rate.per.100000 %in% c("Data not available")] = NA       

  
  #Check out this should be coded#
  data$Population[data$Population %in% c("NA")] = NA                             
  data$population = as.numeric(gsub(",", '', data$Population)) 
  
  data 
  
} )
  



###Put in data manager### 

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),                        #optional#
    url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
    details = 'CDC Reporting')                                                  #Methodology footnotes here if there is substantial change#
  
  #Record years and locations
 locations = union(locations, unlist(unique(data['location'])))                 #Question: How is this section used?#
 years = union(years, unlist(unique(data['year'])))



################################################################################
###MSA Level Data###

DATA.DIR.MSA.DEATHSPREVALENCE="../../data_raw/major_msa/deaths_prevalence"
DATA.DIR.MSA.DIGANOSES="../../data_raw/major_msa/diagnoses"

atlas_msa_files_deaths_prevalence <- Sys.glob(paste0(DATA.DIR.MSA.DEATHSPREVALENCE, '/*.csv'))
data.list.one <- lapply(atlas_msa_files_deaths_prevalence, read.csv, skip=10, header=TRUE)

atlas_msa_files_diagnoses <- Sys.glob(paste0(DATA.DIR.MSA.DIGANOSES, '/*.csv'))
data.list.two <- lapply(atlas_msa_files_diagnoses, read.csv, skip=8, header=TRUE)



################################################################################

################################################################################
###EHE County Level Data###

DATA.DIR="../../data_raw"
DATA.DIR.STATE="../../data_raw/state/diagnoses"

atlas_state_files <- Sys.glob(paste0(DATA.DIR.STATE, '/*.csv'))

data.list <- lapply(atlas_state_files, read.csv, skip=8, header=TRUE)

################################################################################

################################################################################
###All County Level Data###

DATA.DIR="../../data_raw"
DATA.DIR.STATE="../../data_raw/state/diagnoses"

atlas_state_files <- Sys.glob(paste0(DATA.DIR.STATE, '/*.csv'))

data.list <- lapply(atlas_state_files, read.csv, skip=8, header=TRUE)

################################################################################


###Save surveillance manager####
  
  surveilance.manager= data.manager #Add this here so you don't have to change data.manager throughout entire code#
  
  save(surveillance.manager, file="../../cached/surveillance.manager.rdata")


