# library(jheem2)
# library(readxl)
# library(tidyverse)
<<<<<<< HEAD
library(locations)
=======
#library(locations)
>>>>>>> 156f4eab933c7643a44ea28c4c0b888ac90a7253

################################################################################
                  ###Read in MSA LHD Data###
################################################################################
DATA.DIR.LHD="../../data_raw/lhd_msa"

lhd_files <- Sys.glob(paste0(DATA.DIR.LHD, '/*.xlsx'))

data.list.lhd <- lapply(lhd_files, function(x){
  list(filename=x, data=read_excel(x))
})
################################################################################
                    ###Clean MSA LHD Data###
################################################################################
data.list.lhd.clean = lapply(data.list.lhd, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$year = as.character(data$year)
  
  data$value = round(data$value, digits=2)
  
  data$location =locations::get.cbsa.for.msa.name(data$msa)
  
  data$race = ifelse(data$demo=="black", "black",
                     ifelse(data$demo =='hispanic', "hispanic",
                            ifelse(data$demo =='other race', 'other', "")))
              
  data$age = ifelse(data$demo=="13-24 years", "13-24 years",
                    ifelse(data$demo =='25-34 years', "25-34 years",
                           ifelse(data$demo =='35-44 years', "35-44 years",
                                  ifelse(data$demo =='45-54 years', "45-54 years",
                           ifelse(data$demo =='55+ years', '55+ years', "")))))         
                 
  data$sex = ifelse(data$demo=="male", "male",
                             ifelse(data$demo =='female', 'female', ""))
  
  data$risk = ifelse(data$demo=="msm", "msm",
                     ifelse(data$demo =='idu', "idu",
                            ifelse(data$demo =='msm_idu', "msm_idu",
                            ifelse(data$demo =='heterosexual', 'heterosexual', ""))))
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
      #Creating lists by each demographic bc otherwise the put gets messed up 
      #when there is an NA value for the demo variables#
################################################################################
data.list.lhd.clean.age = lapply(data.list.lhd.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$age != "")
  data <- data %>%
    select(year, location, outcome, age, value)
  data= as.data.frame(data)
  list(filename, data) 
})

data.list.lhd.clean.sex= lapply(data.list.lhd.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$sex != "")
  data <- data %>%
    select(year, location, outcome, sex, value)
  data= as.data.frame(data)
  list(filename, data) 
})

data.list.lhd.clean.race = lapply(data.list.lhd.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$race != "")
  data <- data %>%
    select(year, location, outcome, race, value)
  data= as.data.frame(data)
  list(filename, data) 
})

data.list.lhd.clean.risk = lapply(data.list.lhd.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$risk != "")
  data <- data %>%
    select(year, location, outcome, risk, value)
  data= as.data.frame(data)
  list(filename, data) 
})

data.list.lhd.clean.total = lapply(data.list.lhd.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$age == "")
  data= subset(data, data$sex == "")
  data= subset(data, data$race == "")
  data= subset(data, data$risk == "")
  data <- data %>%
    select(year, location, outcome, value)
  data= as.data.frame(data)
  list(filename, data) 
})

##Combine all the individual lists together into one [age, risk, race, sex, total]

combined_list <- c(data.list.lhd.clean.total, data.list.lhd.clean.sex, data.list.lhd.clean.race, 
                   data.list.lhd.clean.age, data.list.lhd.clean.risk)

##Subset the combo list by location so each location has a compilation of
##lists with their demographics (and then a bunch of empty dfs for other locations)

#C.12420 - Austin
#C.12580 - Baltimore
#C.12940- Baton Rouge
#C.17460 - Cleveland
#C.18140 - Columbus
#C.19100 - Dallas
#C.47900 -DC
#C.19820 - Detroit
#C.26900 - Indianapolis
#C.35380 - New Orleans
#C.41700 - San Antonio

austin = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.12420")
  data= as.data.frame(data)
  list(filename, data) 
})
baltimore = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.12580")
  data= as.data.frame(data)
  list(filename, data) 
})
baton_rouge = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.12940")
  data= as.data.frame(data)
  list(filename, data) 
})
cleveland = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.17460")
  data= as.data.frame(data)
  list(filename, data) 
})
columbus = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.18140")
  data= as.data.frame(data)
  list(filename, data) 
})
dallas = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.19100")
  data= as.data.frame(data)
  list(filename, data) 
})
dc = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.47900")
  data= as.data.frame(data)
  list(filename, data) 
})
detroit = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.19820")
  data= as.data.frame(data)
  list(filename, data) 
})
indianapolis = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.26900")
  data= as.data.frame(data)
  list(filename, data) 
})
new_orleans = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.35380")
  data= as.data.frame(data)
  list(filename, data) 
})
san_antonio = lapply(combined_list, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= subset(data, data$location == "C.41700")
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
                   ###Add LHD to Data Manager###
            #Put by location to add details for each LHD#
################################################################################

##Put Austin
lhd_austin = lapply(austin, `[[`, 2)

for (data in lhd_austin) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://www.austintexas.gov/department/hiv-planning-council#:~:text=Austin%20Area%20HIV%20Needs%20Assessment,the%20funds%20available%20for%20servicess',
    details = 'Local Health Department Reports')
}

##Put Baltimore
lhd_baltimore = lapply(baltimore, `[[`, 2)

for (data in lhd_baltimore) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://health.maryland.gov/phpa/OIDEOR/CHSE/Pages/statistics.aspx',
    details = 'Local Health Department Reports; Linkage data for 2016-2021 represents cases diagnosed in that particular year')
}

##Put Baton Rouge
lhd_batonrouge = lapply(baton_rouge, `[[`, 2)

for (data in lhd_batonrouge) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://ldh.la.gov/news/1935',
    details = 'Local Health Department Reports')
}

##Put Cleveland
lhd_cleveland = lapply(cleveland, `[[`, 2)

for (data in lhd_cleveland) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://odh.ohio.gov/know-our-programs/hiv-aids-surveillance-program/Data-and-Statistics/',
    details = 'Local Health Department Reports')
}

##Put Columbus
lhd_columbus = lapply(columbus, `[[`, 2)

for (data in lhd_columbus) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://odh.ohio.gov/know-our-programs/hiv-aids-surveillance-program/Data-and-Statistics/',
    details = 'Local Health Department Reports')
}

##Put Dallas
lhd_dallas = lapply(dallas, `[[`, 2)

for (data in lhd_dallas) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://achievingtogethertx.org/wp-content/uploads/2020/01/Dallas-HSDA-2018-HIV-Continuum-of-Care.pdf',
    details = 'Local Health Department Reports')
}

##Put DC
lhd_dc = lapply(dc, `[[`, 2)

for (data in lhd_dc) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://health.maryland.gov/phpa/OIDEOR/CHSE/Documents/Washington-MSA-HIV-Progress-Table--2020.pdf',
    details = 'Local Health Department Reports')
}

##Put Detroit
lhd_detroit = lapply(detroit, `[[`, 2)

for (data in lhd_detroit) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://www.michigan.gov/mdhhs/keep-mi-healthy/chronicdiseases/hivsti/data-and-statistics/HIV-STI-Statewide-Annual-Analyses#current',
    details = 'Local Health Department Reports')
}

##Put Indianapolis
lhd_indianapolis = lapply(indianapolis, `[[`, 2)

for (data in lhd_indianapolis) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://www.in.gov/health/hiv-std-viral-hepatitis/files/Final-STATE-OF-INDIANA-INTEGRATED-PREVENTION-AND-CARE-PLAN-2016a.pdf',
    details = 'Local Health Department Reports')
}

#Put New Orleans
lhd_new_orleans = lapply(new_orleans, `[[`, 2)

for (data in lhd_indianapolis) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://ldh.la.gov/news/1935',
    details = 'Local Health Department Reports')
}

#Put San Antonio
lhd_san_antonio = lapply(san_antonio, `[[`, 2)

for (data in lhd_san_antonio) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'lhd',
    source = 'lhd',
    dimension.values = list(),
    url = 'https://www.fast-trackcities.org/sites/default/files/San%20Antonio%20Transitional%20Grant%20Area%20Integrated%20HIV%20Prevention%20and%20Care%20Plan%202017-2021.pdf',
    details = 'Local Health Department Reports')
}