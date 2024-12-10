library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

###############################################################################

#####SECTION 4#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')


#Register outcomes:

data.manager$register.outcome(
  'proportion.msm', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion of MSM',
    axis.name = 'Proportion of MSM',
    units = '%',
    description = "Proportion of Men who have sex with Men"), denominator.outcome = 'population') 

data.manager$register.outcome(
  'proportion.tested.n',       
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Denominator value for proportion tested in past year',
    axis.name = 'Denominator value for proportion tested in past year',
    units = '%',
    description = "Denominator value for proportion tested in past year"))

data.manager$register.outcome(
  'proportion.tested', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion Tested in Past Year',
    axis.name = 'Proportion Tested in Past Year',
    units = '%',
    description = "Proportion of People who have received an HIV test in the last year"), denominator.outcome = 'proportion.tested.n')

#Register Sources:

#Register Ontologies:

#Codes:
source('data_processing/syphilis.manager/cached.proportion.msm.R')
source('data_processing/syphilis.manager/brfss_national_weighted_tested.R') #This is used for national level proportion.tested
source('data_processing/syphilis.manager/brfss_national_weighted_msm.R') #This is used for national level proportion.msm

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")