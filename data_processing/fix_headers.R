#Source this file into data_processing_main to remove the long headers from the CDC Atlas Plus data
#Note each group of files has a different length header

#diagnoses - skip 8
#deaths - skip 10
#prevalence - skip 10
#SLE -skip 11
#knowledge - skip 10

library(jheem2)

DATA.DIR.DIAGNOSES="../../data_raw/diagnoses"
DATA.DIR.DEATHS="../../data_raw/deaths"
DATA.DIR.PREVALENCE="../../data_raw/prevalence"
DATA.DIR.SLE="../../data_raw/sle"
DATA.DIR.KNOWLEDGE="../../data_raw/knowledge"


diagnoses_files <- Sys.glob(paste0(DATA.DIR.DIAGNOSES, '/*.csv'))
data.list.diagnoses <- lapply(diagnoses_files, read.csv, skip=8, header=TRUE)

deaths_files <- Sys.glob(paste0(DATA.DIR.DEATHS, '/*.csv'))
data.list.deaths <- lapply(deaths_files, read.csv, skip=10, header=TRUE)

prevalence_files <- Sys.glob(paste0(DATA.DIR.PREVALENCE, '/*.csv'))
data.list.prevalence <- lapply(prevalence_files, read.csv, skip=10, header=TRUE)

sle_files <- Sys.glob(paste0(DATA.DIR.SLE, '/*.csv'))
data.list.sle <- lapply(sle_files, read.csv, skip=11, header=TRUE)

knowledge_files <- Sys.glob(paste0(DATA.DIR.KNOWLEDGE, '/*.csv'))
data.list.knowledge <- lapply(knowledge_files, read.csv, skip=10, header=TRUE)

