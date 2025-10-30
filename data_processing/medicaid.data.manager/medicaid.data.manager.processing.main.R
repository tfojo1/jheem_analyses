
library(jheem2)
library(tidyverse)
library(locations)

data.manager = create.data.manager('medicaid.data.manager', description='medicaid.data.manager')

#Register outcomes:

data.manager$register.outcome(
    'adap.clients',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'ADAP Clients',
        axis.name = 'ADAP Clients',
        units = 'population',
        description = "AIDS Drug Assistance Program Clients"))

data.manager$register.outcome(
    'non.adap.clients',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Non-ADAP Clients',
        axis.name = 'Non-ADAP Clients',
        units = 'population',
        description = "Non-ADAP Clients"))

data.manager$register.outcome(
    'proportion.nonadap.rw.clients.on.medicaid',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of Non-ADAP Ryan White Clients on Medicaid',
        axis.name = 'Proportion of Non-ADAP Ryan White Clients on Medicaid',
        units = '%',
        description = "Proportion of Non-ADAP Ryan White Clients on Medicaid"), denominator.outcome = 'non.adap.clients') 

data.manager$register.outcome(
    'proportion.adap.rw.clients.on.medicaid',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of ADAP Ryan White Clients on Medicaid',
        axis.name = 'Proportion of ADAP Ryan White Clients on Medicaid',
        units = '%',
        description = "Proportion of ADAP Ryan White Clients on Medicaid"), denominator.outcome = 'adap.clients') 

#Register Sources:
data.manager$register.parent.source('HRSA', full.name = 'Health Resources and Services Administration', short.name= "HRSA") #parent

data.manager$register.source('ryan.white.program', parent.source= "HRSA", full.name = "Ryan White HIV/AIDS Program Annual Data Report", short.name='ryan.white.program') #child

#Register Ontologies:
data.manager$register.ontology(
    'ryan.white.pdfs',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55-64 years', '65+ years'),
        race=c('white', 'black', 'american indian alaska native', 'native hawaiian pacific islander', 'hispanic', 'asian'),
        sex=c('male','female'),
        risk = c('msm', "msm_idu", 'heterosexual', 'other', 'idu'),
        fpl = c('0-100', '101-138', '139-250', '251-400', '>400'),
        service.received = c('full pay medication support', 'insurance premium assistance', 'medication co pay/deductible', 'multiple services')
    ))

#Register locations (because these reports use Regions [groups of states])
# locations::register.locations(type='region', 
#                               locations = c("01", "02", "03", "04", "05",
#                                             "06", "07", "08", "09", "10"),
#                               location.names =c('rw.region.1', 'rw.region.2', 'rw.region.3', 
#                                                 'rw.region.4', 'rw.region.5', 'rw.region.6',
#                                                 'rw.region.7', 'rw.region.8', 'rw.region.9',
#                                                  'rw.region.10'))



# Source ------------------------------------------------------------------
source('data_processing/medicaid.data.manager/adap.and.non.adap.totals.by.region.R') #outcome = adap.clients; non.adap.clients
source('data_processing/medicaid.data.manager/adap.and.non.adap.medicaid.proportions.R') #outcome = proportion.nonadap.rw.clients.on.medicaid; proportion.adap.rw.clients.on.medicaid
source('data_processing/medicaid.data.manager/brfss.proportion.tested.medicaid.R')

# Save --------------------------------------------------------------------
save(data.manager, file="Q:/data_managers/medicaid.data.manager.rdata")
save(data.manager, file = '../../cached/medicaid.data.manager.rdata')

################################################################################################
################################################################################################

# Zoe Question: Check what this is and verify if you need if for this data manager --------
# # Source Todd's Additional Cleaning Code ----------------------------------
# source('applications/EHE/ehe_ontology_mappings.R')
# source('applications/ryan_white/ryan_white_data_ontology_mappings.R')
# source('applications/ryan_white/ryan_white_data_cleanup.R')