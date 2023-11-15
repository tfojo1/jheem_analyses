
################################################################################
                      ##Create Proportion of MSM##
              ##Note this dataset pulls from the BRFSS state code
                      ##This is a weighted measure
                ##MSM Estimation using BRFSS Data by State
################################################################################
data.list.brfss.state.msm = lapply(data.list.brfss.state.totals, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location, msm) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>%  #how do I weight this#
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})

###############################################################################
                ##Put MSM Proportion into data manager- BRFSS##
###############################################################################
##BRFSS MSM State
msm.state = lapply(data.list.brfss.state.msm, `[[`, 2)  

for (data in msm.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

###############################################################################
            ##Emory MSM Data by County for 2013#
###############################################################################
DATA.DIR.MSM="../../data_raw/emory"

emory_files <- Sys.glob(paste0(DATA.DIR.MSM, '/*.csv'))

data.list.emory.msm <- lapply(emory_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE, colClasses=c(COUNTYFP="character", STATEFP= "character")))
})

###############################################################################
                ##Clean Emory data##
###############################################################################
data.list.emory.msm.county = lapply(data.list.emory.msm, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2013"
  data$outcome= "proportion.msm"

  data$value = as.numeric(data$MSM12MTH/data$ADULTMEN)
  data$value = round(data$value, digits=2)
  
  #To create location- combine state and county FIPS
  data$state_code= str_pad(data$STATEFP, width=2, side="left", pad="0")
  data$county_code= str_pad(data$COUNTYFP, width=3, side="left", pad="0")
  data$location = paste(data$state_code, data$county_code, sep="")

  data= as.data.frame(data)
  
  list(filename, data)
})
###############
data.list.emory.msm.msa = lapply(data.list.emory.msm, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2013"
  data$outcome= "proportion.msm"
  
  data$value = as.numeric(data$MSM12MTH/data$ADULTMEN)
  data$value = round(data$value, digits=2)
  
  #To create location- combine state and county FIPS
  data$state_code= str_pad(data$STATEFP, width=2, side="left", pad="0")
  data$county_code= str_pad(data$COUNTYFP, width=3, side="left", pad="0")
  data$location = paste(data$state_code, data$county_code, sep="")
  
  data$location = sub("^", "C.", data$location)
  data$location_check = locations::is.location.valid(data$location)
  #Removing invalid MSA locations#
  data=subset(data, !is.na(data$location))
  
  data= as.data.frame(data)
  
  list(filename, data)
})

###########
##CHECKS##
##########
# check = data.list.emory.msm.clean[[1]]
# check = check[[2]]

###############################################################################
##Put MSM Proportion into data manager##
###############################################################################
# ###Emory MSM by County
# msm.emory.county = lapply(data.list.emory.msm.county, `[[`, 2)  
# 
# for (data in msm.emory.county) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'emory',
#     source = 'emory',
#     dimension.values = list(),
#     url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
#     details = 'Emory University MSM Research from American Community Survey')
# }

# ###Emory MSM by MSA
# msm.emory.msa = lapply(data.list.emory.msm.msa, `[[`, 2)  
# 
# for (data in msm.emory.msa ) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'emory',
#     source = 'emory',
#     dimension.values = list(),
#     url = 'https://prismhealth.emory.edu/estimating-the-population-sizes-of-men-who-have-sex-with-men-in-us-states-and-counties-using-data-from-the-american-community-survey/',
#     details = 'Emory University MSM Research from American Community Survey')
# }
