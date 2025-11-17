
library(jheem2)
library(tidyverse)
library(locations)
library(haven)

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

data.manager$register.outcome(
    'medicaid.total',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Medicaid Total',
        axis.name = 'Medicaid Total',
        units = 'population',
        description = "Total Count of Individuals with Medicaid"))

data.manager$register.outcome(
    'uninsured.total',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Uninsured Total',
        axis.name = 'Uninsured Total',
        units = 'population',
        description = "Total Count of Individuals who are Uninsured"))

data.manager$register.outcome(
    'proportion.tested.for.hiv.past.year.medicaid',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion on Medicaid Tested for HIV Past Year',
        axis.name = 'Proportion on Medicaid Tested for HIV Past Year',
        units = '%',
        description = "Proportion of Individuals on Medicaid who were Tested for HIV in the Past Year"), denominator.outcome = 'medicaid.total') 

data.manager$register.outcome(
    'proportion.tested.for.hiv.past.year.uninsured',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion Uninsured Tested for HIV Past Year',
        axis.name = 'Proportion Uninsured Tested for HIV Past Year',
        units = '%',
        description = "Proportion of Uninsured Individuals who were Tested for HIV in the Past Year"), denominator.outcome = 'uninsured.total')

data.manager$register.outcome(
    'oahs.clients', #was previously ambulatory.care.past.year
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Received Ambulatory Care in Past Year',
        axis.name = 'Received Ambulatory Care in Past Year',
        units = 'population',
        description = "Received Ambulatory Care in Past Year"))

data.manager$register.outcome(
    'adap.suppression', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'ADAP Viral Suppression',
        axis.name = 'ADAP Viral Suppression',
        units = '%',
        description = "ADAP Viral Suppression"), denominator.outcome = 'adap.clients')


#Register Sources:
data.manager$register.parent.source('HRSA', full.name = 'Health Resources and Services Administration', short.name= "HRSA") #parent
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS") 
data.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census")
data.manager$register.parent.source('cms', full.name = 'The Centers for Medicare & Medicaid Services', short.name= "cms")
data.manager$register.parent.source('NASTAD', full.name = 'National Alliance of State and Territorial AIDS Directors', short.name= "NASTAD") #parent

data.manager$register.source('ryan.white.program', parent.source= "HRSA", full.name = "Ryan White HIV/AIDS Program Annual Data Report", short.name='ryan.white.program') #child
data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss')
data.manager$register.source('acs', parent.source= "census", full.name = 'American Community Survey', short.name= "acs") 
data.manager$register.source('cms', parent.source= "cms", full.name = 'The Centers for Medicare & Medicaid Services', short.name= "cms") 
data.manager$register.source('nastad.adap', parent.source= "NASTAD", full.name = "ADAP Monitoring Project Annual Report", short.name='nastad.adap') #child


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

data.manager$register.ontology(
    'brfss',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
        race=c('white', 'black', 'american indian/alaska native', 'asian', 'native hawaiian/other pacific islander', 'hispanic', 'other race'),
        sex=c('male','female'),
        risk=c('msm', 'not_msm')
    ))

data.manager$register.ontology(
    'acs',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('0-18', '19-64'),
        race=c("white", "black", "hispanic", "asian/native hawaiian or pacific islander", "american indian or alaska native"),
        sex=c('male','female')
    ))

data.manager$register.ontology(
    'cms',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('0-18', '19-26', '27-44', '45-64', '65+')
    ))

#Register Locations:
locations::register.types(type = c("rw.region"), prefix = c("rw"), prefix.longform = c("ryan.white.region"))

locations::register.locations(type='rw.region',
                              locations = c('rw.region.1', 'rw.region.2', 'rw.region.3',
                                            'rw.region.4', 'rw.region.5', 'rw.region.6',
                                            'rw.region.7', 'rw.region.8', 'rw.region.9',
                                            'rw.region.10'),
                              location.names =c('rw.region.1', 'rw.region.2', 'rw.region.3',
                                                'rw.region.4', 'rw.region.5', 'rw.region.6',
                                                'rw.region.7', 'rw.region.8', 'rw.region.9',
                                                'rw.region.10'))



# Source ------------------------------------------------------------------
source('data_processing/medicaid.data.manager/adap.and.non.adap.totals.by.region.R') #outcome = adap.clients; non.adap.clients (by region)
source('data_processing/medicaid.data.manager/adap.and.non.adap.medicaid.proportions.R') #outcome = proportion.nonadap.rw.clients.on.medicaid; proportion.adap.rw.clients.on.medicaid
source('data_processing/medicaid.data.manager/brfss.proportion.tested.medicaid.R') #outcomes = medicaid.total;proportion.tested.for.hiv.past.year.medicaid; 
source('data_processing/medicaid.data.manager/brfss.proportion.tested.uninsured.R') #outcomes = uninsured.total;  proportion.tested.for.hiv.past.year.uninsured
source('data_processing/medicaid.data.manager/brfss.data.variance.values.R') #Adds variance values to uninsured.total and medicaid.total

source('data_processing/medicaid.data.manager/acs.total.medicaid.and.uninsured.R') #outcomes = medicaid.total;uninsured.total
source('data_processing/medicaid.data.manager/cms.total.medicaid.R') #outcomes = medicaid.total;uninsured.total
source('data_processing/medicaid.data.manager/ryan.white.total.and.stratified.state.only.R') #outcomes = adap.clients; non.adap.clients (by state)
source('data_processing/medicaid.data.manager/ryan.white.adap.suppression.copy.R') #outcomes = adap.suppression

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