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
##Outcome = retention.of.engaged
##Filter for states without symbols so this will be for data that has no footnotes
################################################################################
data.list.retention.engaged.clean = lapply(data.list.retention, function(file){

  data=file[["data"]]
  filename = file[["filename"]]

  data=subset(data, grepl('^[A-Za-z_ -]+$', state)) #removes any states with symbols
  
  data$outcome = "retention.of.engaged"
  
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
  
  data$value = data$retention_calc
  data$value = round(data$value, digits=2)
  
 data <- data %>%
   select(year, outcome, location, value)

  data= as.data.frame(data)

  list(filename, data)
})

################################################################################
                    ###Clean State Retention Data###
##Outcome = retention
##Filter for states without symbols so this will be for data that has no footnotes
################################################################################
data.list.retention.clean = lapply(data.list.retention, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data=subset(data, grepl('^[A-Za-z_ -]+$', state)) #removes any states with symbols
  
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
  
  data$value = (data$`2cd4_percent`/100)
  data$value = round(data$value, digits=2)
  
   data <- data %>%
     select(year, outcome, location, value)
  
  data= as.data.frame(data)
  
  list(filename, data)
})
################################################################################
###Create datasets of just states with symbols for footnotes (1 for each outcome)
#outcome= retention.of.engaged
################################################################################
#outcome= retention.of.engaged
data.list.retention.engaged.foonotes = lapply(data.list.retention, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    filter(str_detect(state, "[^[:alnum:] ]"))  #this selects states with symbol
  
  data$state= gsub('[^[:alnum:] ]',"",data$state) #Then remove the symbols to create the location var
  
  data$outcome = "retention.of.engaged"
  
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
    data= subset(data, state== "Maryland")  ##Note: Hardcoding out Maryland bc of case footnote; this will be put separately#
  }
  if(grepl("2021", filename)) {
    data$year = as.character("2021")
  }
  
  data$location = state.abb[match(data$state, state.name)]
  
  data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
  
  data$value = data$retention_calc
  data$value = round(data$value, digits=2)
  
  data <- data %>%
    select(year, outcome, location, value)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

################################################################################
      ###Create datasets of just states with symbols for footnotes (1 for each outcome)
#outcome = retention
################################################################################
#outcome = retention
data.list.retention.footnotes = lapply(data.list.retention, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    filter(str_detect(state, "[^[:alnum:] ]"))  #this selects states with symbol
  
  data$state= gsub('[^[:alnum:] ]',"",data$state) #Then remove the symbols to create the location var
  
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
    data= subset(data, state== "Maryland")  ##Note: Hardcoding out Maryland bc of case footnote; this will be put separately#
  }
  if(grepl("2021", filename)) {
    data$year = as.character("2021")
  }
  
  data$location = state.abb[match(data$state, state.name)]
  
  data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
  
  data$value = (data$`2cd4_percent`/100)
  data$value = round(data$value, digits=2)
  
  data <- data %>%
    select(year, outcome, location, value)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

################################################################################
#Create datasets for just Maryland footnote about cases in 2020
#outcome= retention.of.engaged
################################################################################
#outcome= retention.of.engaged
data.list.retention.engaged.md = lapply(data.list.retention, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$state= gsub('[^[:alnum:] ]',"",data$state) #Then remove the symbols to create the location var
  
  data$outcome = "retention.of.engaged"
  
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
  
  data$value = data$retention_calc
  data$value = round(data$value, digits=2)
  
  data <- data %>%
    select(year, outcome, location, value) %>%
    filter(location == "MD" & year == "2020")  ###SELECTING ONLY MARYLAND BC OF FOOTNOTE##
  
  data= as.data.frame(data)
  
  list(filename, data)
})

################################################################################
#Create datasets for just Maryland footnote about cases in 2020
#outcome = retention
################################################################################
#outcome = retention
data.list.retention.md = lapply(data.list.retention, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    filter(str_detect(state, "[^[:alnum:] ]"))  #this selects states with symbol
  
  data$state= gsub('[^[:alnum:] ]',"",data$state) #Then remove the symbols to create the location var
  
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
  
  data$value = (data$`2cd4_percent`/100)
  data$value = round(data$value, digits=2)
  
  data <- data %>%
    select(year, outcome, location, value)
  
  data <- data %>%
    select(year, outcome, location, value) %>%
    filter(location == "MD" & year == "2020")  ###SELECTING ONLY MARYLAND BC OF FOOTNOTE##
  
  data= as.data.frame(data)
  
  list(filename, data)
})


################################################################################
            ###Put Data into Data Manager###
##Creating multiple put statements for the notes on issues with death and 
##case reporting
################################################################################

#put outcome= retention
state_retention_data = lapply(data.list.retention.clean, `[[`, 2)

for (data in state_retention_data) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data')
}

#put outcome= retention.of.engaged
state_retention_engaged_data = lapply(data.list.retention.engaged.clean, `[[`, 2)

for (data in state_retention_engaged_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data')
}

################################################################################
#Puts for details in footnotes
################################################################################

#put outcome= retention FOOTNOTES
state_retention_footnotes = lapply(data.list.retention.footnotes, `[[`, 2)

for (data in state_retention_footnotes) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data; Data should be interpreted with caution due to incomplete ascertainment of deaths that occurred during the year')
}

#put outcome= retention.of.engaged FOOTNOTES
state_retention_engaged_footnotes = lapply(data.list.retention.engaged.foonotes, `[[`, 2)

for (data in state_retention_engaged_footnotes) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data; Data should be interpreted with caution due to incomplete ascertainment of deaths that occurred during the year')
}

################################################################################
#Create separate put for MD Footnote in 2020
#Footnote is around case ascertainment
################################################################################

#put outcome= retention MARYLAND
state_retention_MD = lapply(data.list.retention.md, `[[`, 2)

for (data in state_retention_MD) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data;Data should be interpreted with caution due to incomplete reporting of case information to CDC during December 2021.')
}

#put outcome= retention.of.engaged MARYLAND
state_retention_engaged_MD = lapply(data.list.retention.engaged.md, `[[`, 2)

for (data in state_retention_engaged_MD) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.retention.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/hiv-surveillance.html',
    details = 'Monitoring Selected National HIV Prevention and Care Objectives by Using HIV Surveillance Data; Data should be interpreted with caution due to incomplete reporting of case information to CDC during December 2021.')
}
