
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 4#####

###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

#Register outcomes:
#Register Sources:
#Register Ontologies:
#Codes:
#Aggregate Outcomes:
#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section4.rdata")