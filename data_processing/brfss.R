#library(haven)

################################################################################
                    ###Read in BRFSS .xpt files
################################################################################

DATA.DIR.BRFSS.STATE="../../data_raw/brfss/brfss_state"

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
  list(filename=x, data=read_xpt(x))
})

DATA.DIR.BRFSS.MSA="../../data_raw/brfss/brfss_msa"

brfss_file_msa<- list.files(DATA.DIR.BRFSS.MSA, pattern = ".XPT", full.names = "TRUE")

brfss_file_msa_list <- lapply(brfss_file_msa, function(x) {
  list(filename=x, data=read_xpt(x))
})
################################################################################
###Create state mapping bc the function isnt working but maybe there's 
#another way in the locations package that I don't know
################################################################################
state.to.fips.mappings = c('1' = 'AL',
                            '2'='AK',
                           '4'='AZ',
                           '5'='AR',
                           '6'='CA',
                           '8'='CO',
                           '9'='CT',
                           '10'='DE',
                           '11'='DC',
                           '12'='FL',
                           '13'='GA',
                           '15'='HI',
                           '16'='ID',
                           '17'='IL',
                           '18'='IN',
                           '19'='IA',
                           '20'='KA',
                           '21'='KY',
                           '22'='LA',
                           '23'='ME',
                           '24'='MD',
                           '25'='MA',
                           '26'='MI',
                           '27'='MN',
                           '28'='MS',
                           '29'='MO',
                           '30'='MT',
                           '31'='NE',
                           '32'='NV',
                           '33'='NH',
                           '34'='NJ',
                           '35'='NM',
                           '36'='NY',
                           '37'='NC',
                           '38'='ND',
                           '39'='OH',
                           '40'='OK',
                           '41'='OR',
                           '42'='PA',
                           '44'='RI',
                           '45'='SC',
                           '46'='SD',
                           '47'='TN',
                           '48'='TX',
                           '49'='UT',
                           '50'='VT',
                           '51'='VA',
                           '53'='WA',
                           '54'='WV',
                           '55'='WI',
                           '56'='WY',
                           '72'= 'PR')

################################################################################
  ###Creating dataframes with brfss "population" by state and year of survey###
################################################################################
data.list.brfss.state.pop = lapply(brfss_file_state_list, function(file){
  data=file[["data"]] 
  filename = file[["filename"]]
  #Change _state to character#
  data$state_fips = as.character(data$`_STATE`)
  data= subset(data, data$state_fips != "66")  #Removing Guam#
  data= subset(data, data$state_fips != "78")  #Removing Virgin Islands#
  #Create location#
  data$location = state.to.fips.mappings[data$state_fips]
  data <- data %>%
    group_by(location) %>%
    count(location)
  
  #Create year variable#
  if(grepl("2013", filename)) {
    data$year = as.numeric("2013")
  }
  if(grepl("2014", filename)) {
    data$year = as.numeric("2014")
  }
  if(grepl("2015", filename)) {
    data$year = as.numeric("2015")
  }
  if(grepl("2016", filename)) {
    data$year = as.numeric("2016")
  }
  if(grepl("2017", filename)) {
    data$year = as.numeric("2017")
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
  }
  if(grepl("2021", filename)) {
    data$year = as.numeric("2021")
  }
  if(grepl("2022", filename)) {
    data$year = as.numeric("2022")
  }
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
         ###Clean BRFSS State### (Might need to do this by outcome x2)
################################################################################
data.list.brfss.state.clean = lapply(brfss_file_state_list, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  #Change _state to character#
  data$state_fips = as.character(data$`_STATE`)
  data= subset(data, data$state_fips != "66")  #Removing Guam#
  data= subset(data, data$state_fips != "78")  #Removing Virgin Islands#
  
  #Create year variable#
  if(grepl("2013", filename)) {
    data$year = as.numeric("2013")
  }
  if(grepl("2014", filename)) {
    data$year = as.numeric("2014")
  }
  if(grepl("2015", filename)) {
    data$year = as.numeric("2015")
  }
  if(grepl("2016", filename)) {
    data$year = as.numeric("2016")
  }
  if(grepl("2017", filename)) {
    data$year = as.numeric("2017")
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
  }
  if(grepl("2021", filename)) {
    data$year = as.numeric("2021")
  }
  if(grepl("2022", filename)) {
    data$year = as.numeric("2022")
  }
  
  #Create location#
  data$location = state.to.fips.mappings[data$state_fips]
  
  #Create population total by state
  # data <- data %>%
  # group_by(location) %>%
  #   mutate(n=add_tally(location))%>%
  #   ungroup()
  
  countspop <- table(data$location)
  data$population <-countspop[match(data$location, names(countspop))]
  
  #Create outcome#
  data$outcome = "proportion.tested"  #will need to create a second outcome for denominator#
  
  #Create value for proportion of people who have receveid an HIV test in the past year
  #Complication is date in which someone got an HIV test vs year of survey- could I just subtract the date of survey from the date of test? then make a >365 flag?
  #data= subset(data, data$HIVTSTD3 != "777777")  #Removing invalid responses###Should I just change these to NA?
  #data= subset(data, data$HIVTSTD3 != "999999") #Not sure what this is#
  
  data$HIVTSTD3 = if_else(data$HIVTSTD3== "777777", NA, data$HIVTSTD3)
  data$HIVTSTD3 = if_else(data$HIVTSTD3== "999999", NA, data$HIVTSTD3)
  
  
  data=subset(data, !is.na(data$HIVTSTD3))
  #Do calculation to determine if test was in fact in the past year:
  data$test_year = as.numeric(str_sub(data$HIVTSTD3, -4))
  data$test_month = substring(data$HIVTSTD3, 1, nchar(data$HIVTSTD3)-4)
  data$test_month = as.numeric(if_else(data$test_month == "77", NA, data$test_month))
  
#Copying from Todd's code#
data$test_month = if_else((!is.na(data$test_month) & data$test_month==77), 6, data$test_month) #Ask Todd to clarify this piece# #isn't this redundant
data$test_month = if_else(is.na(data$test_year), NA, data$test_month)

data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= data$year)
mask = !is.na(data$test_year) & data$test_year==(data$year-1)
data$tested[mask] = data$test_month[mask] / 12

#data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= (data$year-1))
  

##Zoe attempt at calculation of proportion##
data=subset(data, !is.na(data$tested)) #Remove people who were not tested##

#data = aggregate(data$tested, list(data$location), FUN=sum) #then take this and divide by the count of people in each state and then do ti again for peopleat risk


#Can you merge the populations with the tested values?
 data<- data %>%
  #rename(location = Group.1) %>%
   group_by(location) %>%
   mutate(sum_tested = sum(tested)) %>%
   ungroup()%>%
   mutate(proportion_tested = (sum_tested/population))

 data$proportion_tested = round(data$proportion_tested, digits=2)
 
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, proportion_tested)
  data= as.data.frame(data)
  
  list(filename, data) 
})

#How to merge these
proportion.list = merge(data.list.brfss.state.clean, data.list.brfss.state.pop, by=location)

################################################################################
                            ###Clean BRFSS MSA###
################################################################################


################################################################################
                            ###Put BRFSS data into manager##
################################################################################
#Need to add new source and outcomes into data processing main#