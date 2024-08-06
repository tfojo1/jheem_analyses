options(error=NULL)

# Read in SDH Data --------------------------------------------------------

DATA.DIR.SDH="../../data_raw/syphilis.manager/sdh"

sdh_files <- Sys.glob(paste0(DATA.DIR.SDH, '/*.csv'))

data.list.sdh <- lapply(sdh_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})


# SHD Outcome Mappings ----------------------------------------------------

shd.mappings = c('Uninsured'='uninsured',
                     'Vacant housing' = 'vacant.housing',
                     'Population 25 years and older w/o HS diploma' = 'not.highschool.graduate',
                     'Households living below the federal poverty level' = 'below.fpl',
                     'Population living in a rural area' = 'rural.area',
                     'Urbanization level' = 'urbanization.level')


year.mappings = c('5-year estimates (2018-2022)'='2018-2022',
                 '2010' = '2010',
                 '2013' = '2013', 
                 '2020 (COVID-19 Pandemic)' = '2020')


# Cleaning ----------------------------------------------------------------

shd.clean  = lapply(data.list.sdh, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

  data$outcome = shd.mappings[data$Indicator]
  data = data[!is.na(data$outcome),]
  data$location = as.character(data$FIPS)
  data$Year = as.character(data$Year)
  data$year = year.mappings[data$Year]
  
  #Outcome conditionals#
   if(grepl("prop", filename)) {
     data = subset(data, !is.na(data$Percent)) #Need to remove Percent values that are character values
     data = subset(data, data$Percent != "Data not available")
     data$formatted.percent = as.numeric(data$Percent)
     data$value = (data$formatted.percent/100)
   }
  
  if(grepl("urbanization", filename)) {     #This is categorical- need to figure out what to do with it
    data$value = data$Urbanization.Level
  }

   data <- data %>%
     select(outcome, year, location, value)
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})


# Put SHD Data ------------------------------------------------------------

#I THINK YOU NEED 3 DIFFERENT PUTS BECAUSE YOU HAVE 3 DIFFERENT SOURCES IN THE SDH DATA##

##Update once you know how to structure the ontologoes#

# prep_atlas_all = lapply(data.list.clean.atlas.prep, `[[`, 2)  
# 
# for (data in prep_atlas_all) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'cdc',
#     source = 'cdc.prep',
#     dimension.values = list(),
#     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
#     details = 'CDC Atlas Plus data')
# }
