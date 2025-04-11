#Source this file into data_processing_main to remove headers from the CDC Atlas Plus data
#Note each group of files has a different length header

#diagnoses - skip 8
#deaths - skip 10
#prevalence - skip 10
#SLE -skip 11
#knowledge - skip 10

#library(jheem2)

DATA.DIR.DIAGNOSES="../../data_raw/diagnoses"
DATA.DIR.DEATHS="../../data_raw/deaths"
DATA.DIR.PREVALENCE="../../data_raw/prevalence"
DATA.DIR.SLE="../../data_raw/sle"
DATA.DIR.NATIONAL.SUPPRESSION="../../data_raw/sle/national_suppression"
DATA.DIR.KNOWLEDGE="../../data_raw/knowledge"
DATA.DIR.ATLAS.PREP="../../data_raw/prep/atlas_plus"

#---HIV Diagnoses---#
diagnoses_files <- Sys.glob(paste0(DATA.DIR.DIAGNOSES, '/*.csv'))
#creating a list with sublists of filename, data#
data.list.diagnoses <- lapply(diagnoses_files, function(x){
  skip=8
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
  })

#---HIV Deaths---#
deaths_files <- Sys.glob(paste0(DATA.DIR.DEATHS, '/*.csv'))
data.list.deaths <- lapply(deaths_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---HIV Prevalence---#
prevalence_files <- Sys.glob(paste0(DATA.DIR.PREVALENCE, '/*.csv'))
data.list.prevalence <- lapply(prevalence_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---Suppression, Linkage, Receipt of Care---#
sle_files <- Sys.glob(paste0(DATA.DIR.SLE, '/*.csv'))
data.list.sle <- lapply(sle_files, function(x){
  if(grepl("allcounty", x)) {
    skip=9
    list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
  }
  else  skip=11
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---National Level Suppression--#
national_suppression_files <- Sys.glob(paste0(DATA.DIR.NATIONAL.SUPPRESSION, '/*.csv'))
data.list.national.suppression <- lapply(national_suppression_files, function(x){
    skip=9
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---Knowledge to Care---#
knowledge_files <- Sys.glob(paste0(DATA.DIR.KNOWLEDGE, '/*.csv'))
data.list.knowledge <- lapply(knowledge_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---PrEP Data from Atlas Plus---#
atlas_prep_files <- Sys.glob(paste0(DATA.DIR.ATLAS.PREP, '/*.csv'))
#creating a list with sublists of filename, data#
data.list.atlas.prep <- lapply(atlas_prep_files, function(x){
  skip=8
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})
