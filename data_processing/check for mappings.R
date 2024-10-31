
library(jheem2)

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")

source('applications/EHE/ehe_ontology_mappings.R')
source('commoncode/check_for_needed_mappings.R')


check.for.needed.mappings(surveillance.manager, 
                          ontology(race=c('black', 'hispanic', 'other'), sex=c('heterosexual_male', 'msm', 'female'), risk=c('never_IDU', 'active_IDU', 'IDU_in_remission')),
                          list(c('race', 'ethnicity'), c('sex', 'risk')))


#For SHIELD, you need SHIELD mappings loaded (it also doesn't have risk)