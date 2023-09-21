# library(jheem2)
# library(readxl)
# library(tidyverse)

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
            ###Add LHD to Data Manager###
################################################################################

##Need to add details to differentiate months=1 from months =3 for linkage outcome##
##Balitmore linakge is for new diagnoses#


#Ask todd what this should look like- ex. URL


lhd_total = lapply(data.list.lhd.clean, `[[`, 2)

for (data in lhd_total) {

  data.manager$put.long.form(
    data = data,
    outcome= "prep",
    ontology.name = 'aidsvu',
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'AIDS Vu Reporting')
}