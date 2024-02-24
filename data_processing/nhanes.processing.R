library(haven)
library(dplyr)
################################################################################
##Read in NHANES Data
DATA.DIR.NHANES="C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/NHANES"

nhanes.files <- list.files(DATA.DIR.NHANES, pattern = ".XPT", full.names = "TRUE")

nhanes.files.data.list <- lapply(nhanes.files, function(x) {
  list(filename=x, data=read_xpt(x))
})
################################################################################

nhanes.clean = lapply(nhanes.files.data.list, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("demographics", filename)) {
    data <- data %>%
      select(SEQN, WTINT2YR, WTMEC2YR, SDDSRVYR, RIDAGEYR, RIDRETH1, RIAGENDR)
  }
  
  if(grepl("sexual_behavior_ok", filename)) {
    data <- data %>%
      select(SEQN, SXD021,  SXD031, SXQ251, SXQ270, SXQ294)
  }
  
  if(grepl("sexual_behavior_fix1", filename)) { #NEED TO ADD IN HERE SOMETHNG FOR CALCULATE SEX WITH MEN SXQ101
    data <- data %>%
      select(SEQN, SXD021,  SXD031, SXQ251, SXQ270, SXQ294)
  }
  
  if(grepl("sexual_behavior_fix2", filename)) {
    data <- data %>%
      rename(SXQ292 = SXQ296)%>%
      rename(SXQ294 = SXQ296)%>%
      select(SEQN, SXD021,  SXD031, SXQ251, SXQ270, SXQ294)
  }
  list(filename, data) 
  
})
################################################################################
##Merge datasets
demo = nhanes.files.data.list[[1]]
demo = demo [[2]]
sex = nhanes.files.data.list[[2]]
sex = sex [[2]]
total.07.08= merge(demo, sex, by="SEQN")

demo = nhanes.files.data.list[[3]]
demo = demo [[2]]
sex = nhanes.files.data.list[[4]]
sex = sex [[2]]
total.09.10= merge(demo, sex, by="SEQN")

demo = nhanes.files.data.list[[5]]
demo = demo [[2]]
sex = nhanes.files.data.list[[6]]
sex = sex [[2]]
total.11.12= merge(demo, sex, by="SEQN")

demo = nhanes.files.data.list[[7]]
demo = demo [[2]]
sex = nhanes.files.data.list[[8]]
sex = sex [[2]]
total.13.14= merge(demo, sex, by="SEQN")

demo = nhanes.files.data.list[[9]]
demo = demo [[2]]
sex = nhanes.files.data.list[[10]]
sex = sex [[2]]
total.15.16= merge(demo, sex, by="SEQN")

nhanes.1 = merge(total.07.08, total.09.10, by= "SEQN")
nhanes.2 = merge(nhanes.1, total.11.12, by= "SEQN")
nhanes.3 = merge(nhanes.2, total.13.14, by= "SEQN")
nhanes.total = merge(nhanes.3, total.15.16, by= "SEQN")


nhanes.total = rbind(total.07.08, total.09.10)



 