library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

###############################################################################

#####SECTION 2#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')


#Register outcomes:

data.manager$register.outcome(
  'congenital.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Congenital Syphilis',
    axis.name = 'Congenital Syphilis',
    units = 'cases',
    description = "Congenital Syphilis"))

data.manager$register.outcome(
  'early.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Early, non-primary, non-Secondary Syphilis',
    axis.name = 'Early, non-primary, non-Secondary Syphilis',
    units = 'cases',
    description = "Early, non-primary, non-Secondary Syphilis"))

data.manager$register.outcome(
  'ps.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Primary and Secondary Syphilis',
    axis.name = 'Primary and Secondary Syphilis',
    units = 'cases',
    description = "Primary and Secondary Syphilis"))

data.manager$register.outcome(
  'unknown.duration.or.late.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Unknown Duration or Late Syphilis',
    axis.name = 'Unknown Duration or Late Syphilis',
    units = 'cases',
    description = "Unknown Duration or Late Syphilis"))

#Register Sources:

#Register Ontologies:

#Codes:
source('data_processing/syphilis.manager/syphilis.data.R')

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")