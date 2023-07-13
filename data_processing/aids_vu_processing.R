
library(jheem2)
library(tidyverse)
library(readxl)

##Adding AIDS Vu data###
DATA.DIR.PREP="../../data_raw/prep/aidsvu"

prep_files <- Sys.glob(paste0(DATA.DIR.PREP, '/*.xlsx'))
data.list.prep <- lapply(prep_files, function(x){
  skip=3
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip, col_types= "text"))
})




###Total PrEP Use###
data.list.prep.total = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Formatting all files will need#
    data$year = data$Year 
    data$outcome = "prep"
    
    #Location conditional formatting
    if(grepl("state", filename)) {
      data$location = data$`State Abbreviation`
      subset(data, data$location != "PR")
      
      #Create State Total
      data$value = data$`State PrEP Users`
      data$value[data$value %in% c("-1")] = NA  #data suppressed#
      data$value[data$value %in% c("-2")] = NA  #data suppressed#
      data$value[data$value %in% c("-4")] = NA  #data not available at county level#
      data$value[data$value %in% c("-8")] = NA  #data undefined#
      data$value[data$value %in% c("-9")] = NA  #data unavailable#
    }
    
    if(grepl("county", filename)) {
      data$FIPS = as.numeric(data$`GEO ID`)
      data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
      subset(data, data$State != "PR")
      
      #Create County total
      data$value = data$`County PrEP Users`
      data$value[data$value %in% c("-1")] = NA  #data suppressed#
      data$value[data$value %in% c("-2")] = NA  #data suppressed#
      data$value[data$value %in% c("-4")] = NA  #data not available at county level#
      data$value[data$value %in% c("-8")] = NA  #data undefined#
      data$value[data$value %in% c("-9")] = NA  #data unavailable#
    }
    
    list(filename, data)
    
})



###PrEP Use by Sex###

data.list.prep.sex = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$male=data$`Male PrEP Users`
  data$female =data$`Female PrEP Users` 
  
  data <- data %>%
    pivot_longer(cols=c("male", "female"),
                 names_to = "sex",
                 values_to = "value")
  
  list(filename, data)
  
})

###############################################################################
###PrEP Use by Age###
data.list.prep.age = lapply(data.list.prep, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$`under 25 years`= data$`Age LE 24 PrEP Users`
  data$`25-34 years`= data$`Age 25-34 PrEP Users`
  data$`35-44 years` = data$`Age 35-44 PrEP Users`
  data$`45-54 years`= data$`Age 45-54 PrEP Users`
  data$`55+ years` = data$`Age 55+ PrEP Users`

  
  data <- data %>%
   pivot_longer(cols=c("under 25 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
                  names_to = "age",
                  values_to = "value")
  
  
  list(filename, data)
  
})



##############################################################################

###PrEP Use by Race###
data.list.prep.race = lapply(data.list.prep.total, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$male=data$`Male PrEP Users`
  data$female =data$`Female PrEP Users` 
  
  data <- data %>%
    pivot_longer(cols=c("male", "female"),
                 names_to = "sex",
                 values_to = "value")
  
  list(filename, data)
  
})

###########################archive################################################


# 
# ####Can I make a bunch of functions (that use tidyverse) and then lapply them over lsits
# 
# f1 <- function(data) {
#   rename(male=`Male PrEP Users`)%>%
#     rename(female =`Female PrEP Users`) %>%
#     select(location, year, outcome, male, female) %>%
#     pivot_longer(cols=c("male", "female"),
#                  names_to = "sex",
#                  values_to = "value")
# }
# 
# test.clean = lapply(data.list.prep, f1{
#   data=x[["data"]]
#   })
#     
# zoe = lapply(data.list.prep, `[[`, 2) 
# zoe_1 = lapply(zoe, f1)




  