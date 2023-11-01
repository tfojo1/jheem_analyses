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
'35'='Nm',
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
         ###Clean BRFSS State### (Might need to do this by outcome x2)
################################################################################
data.list.brfss.state.clean = lapply(brfss_file_state_list, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  #Change _state to character#
  data$state_fips = as.character(data$`_STATE`)
  data= subset(data, data$state_fips != "66")  #Removing Guam#
  
  #Create year variable#
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
  if(grepl("2022", filename)) {
    data$year = as.character("2022")
  }
  
  #Create location#
  data$location = state.to.fips.mappings[data$state_fips]
  
  #Create outcome#
  data$outcome = "proportion.tested"  #will need to create a second outcome for denominator#
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                            ###Clean BRFSS MSA###
################################################################################


################################################################################
                            ###Put BRFSS data into manager##
################################################################################
#Need to add new source and outcomes into data processing main#