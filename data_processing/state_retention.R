#library(readxl)

################################################################################
        ###Read in Scraped PDF Tables###
          ###State Retention Data
################################################################################ 

DATA.DIR.RETENTION="../../data_raw/retention"

retention_files <- Sys.glob(paste0(DATA.DIR.RETENTION, '/*.xlsx'))

data.list.retention <- lapply(retention_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
                ###Clean State Retention Data###
################################################################################
data.list.retention.clean = lapply(data.list.retention, function(file){

  data=file[["data"]]
  filename = file[["filename"]]

  data$outcome = "retention"
  
  data= subset(data, data$state != "Total")
  
  #Create Year#
  if(grepl("2011", filename)) {
    data$year = as.character("2011")
    data= subset(data, data$state != "California") #Removing CA from the 2011 data bc it only represents LAC and SF
  }
  if(grepl("2012", filename)) {
    data$year = as.character("2012")
  }
  if(grepl("2013", filename)) {
    data$year = as.character("2013")
  }
  if(grepl("2014", filename)) {
    data$year = as.character("2014")
  }
  if(grepl("2015", filename)) {
    data$year = as.character("2015")
  }
  if(grepl("2016", filename)) {
    data$year = as.character("2016")
  }
  if(grepl("2017", filename)) {
    data$year = as.character("2017")
  }
  if(grepl("2018", filename)) {
    data$year = as.character("2018")
  }
  if(grepl("2019", filename)) {
    data$year = as.character("2019")
  }
  if(grepl("2020", filename)) {
    data$year = as.character("2020")
  }
  if(grepl("2021", filename)) {
    data$year = as.character("2021")
  }
  
data$location = state.abb[match(data$state, state.name)]

data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
  
##Follow up with Todd about how to determine 'value' if it should be the test_2 count or the retention_calc##
  #data$value = 
  
# data <- data %>%
#   select(year, outcome, location, value)

  data= as.data.frame(data)

  list(filename, data)
})

################################################################################
            ###Put Data into Data Manager###
################################################################################

state_retention_data = lapply(data.list.retention.clean, `[[`, 2)

for (data in state_retention_data) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data')
}
