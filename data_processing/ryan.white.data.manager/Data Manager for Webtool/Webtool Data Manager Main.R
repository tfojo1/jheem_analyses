library(jheem2)
library(tidyverse)
#This code creates a separate data manager to be used in the Ryan White webtool

data.manager = create.data.manager('Ryan White Web Data Manager', description='Ryan White Web Data Manager')

#Register outcomes:

data.manager$register.outcome(
    'non.adap.clients',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Non-ADAP Clients',
        axis.name = 'Non-ADAP Clients',
        units = 'population',
        description = "Non-ADAP Clients"))

data.manager$register.outcome(
    'adap.proportion',  #Decided to make this a ratio bc some states have more adap than non and it creates a proportion >1
    metadata = create.outcome.metadata(
        scale = 'ratio',
        display.name = 'ADAP Ratio',
        axis.name = 'ADAP Ratio',
        units = '%',
        display.as.percent = T,
        description = "ADAP Ratio"), denominator.outcome = 'non.adap.clients')

data.manager$register.outcome(
    'oahs.clients', #was previously ambulatory.care.past.year
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Received Ambulatory Care in Past Year',
        axis.name = 'Received Ambulatory Care in Past Year',
        units = 'population',
        description = "Received Ambulatory Care in Past Year"))

data.manager$register.outcome(  ##I'm not actually going to put any data for this
    'adap.clients',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'ADAP Clients',
        axis.name = 'ADAP Clients',
        units = 'population',
        description = "AIDS Drug Assistance Program Clients"))

data.manager$register.outcome(
    'oahs.suppression', #previously non.adap.viral.suppression
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Non-ADAP Viral Suppression',
        axis.name = 'Non-ADAP Viral Suppression',
        units = '%',
        description = "Non-ADAP Viral Suppression"), denominator.outcome = 'oahs.clients')

data.manager$register.outcome(
    'adap.suppression', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'ADAP Viral Suppression',
        axis.name = 'ADAP Viral Suppression',
        units = '%',
        description = "ADAP Viral Suppression"), denominator.outcome = 'adap.clients')

data.manager$register.outcome(
    'diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Diganosed Prevalence',
        axis.name = 'Diganosed Prevalence (n)',
        units = 'cases',
        description = "Diagnosed HIV Prevalence"))

data.manager$register.outcome(
    'diagnoses',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'New Diagnoses',
        axis.name = 'New Diagnoses (n)',
        units = 'cases',
        description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
    'prep', 
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Prescribed PrEP',
        axis.name = 'Number Prescribed PrEP',
        units = 'cases',
        description = "Number Prescribed PrEP"))

data.manager$register.outcome(
    'suppression', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Viral Suppression',
        axis.name = 'Proportion Virally Suppressed',
        units = '%',
        description = "HIV Viral Suppression"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(  #Adding this as a denominator value for awareness/knowledge of status- bc the cdc denominator is an estimated prevalence value of both known and unknown status#
    'total.prevalence',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Total Prevalence',
        axis.name = 'Total Prevalence',
        units = 'cases',
        description = "Estimated Prevalence of Known and Unknown Status"))

data.manager$register.outcome(
    'awareness', #changed from knowledge to awareness
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Awareness of HIV Status',
        axis.name = 'Proportion Aware of HIV Status',
        units = '%',
        description = "Awareness of HIV Status"), denominator.outcome = 'total.prevalence') #creating outcome that is pop values for awareness#

data.manager$register.outcome(
    'proportion.tested.n',           #Will have option in code to make this only people at risk or everyone#
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
data.manager$register.parent.source('HRSA', full.name = 'Health Resources and Services Administration', short.name= "HRSA") #parent
data.manager$register.parent.source('NASTAD', full.name = 'National Alliance of State and Territorial AIDS Directors', short.name= "NASTAD") #parent
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent
data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA")
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS") #parent
data.manager$register.parent.source('LHD', full.name = 'Local Health Department', short.name= "LHD") #parent


data.manager$register.source('ryan.white.program', parent.source= "HRSA", full.name = "Ryan White HIV/AIDS Program Annual Data Report", short.name='ryan.white.program') #child
data.manager$register.source('nastad.adap', parent.source= "NASTAD", full.name = "ADAP Monitoring Project Annual Report", short.name='nastad.adap') #child
data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv')
data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu')
data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss') #child
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('cdc.aggregated.proportion', parent.source= "NHSS", full.name = 'CDC Aggregated Proportion', short.name = 'cdc agg prop') #child
data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv') #child
data.manager$register.source('cdc.prep', parent.source= "IQVIA", full.name = "CDC PrEP Data", short.name='cdc.prep')
data.manager$register.source('cdc.supplemental.reports', parent.source= "NHSS", full.name = "HIV Surveillance Supplemental Report", short.name='cdc.supplemental.reports')
data.manager$register.source('prep.aidsvu.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP AIDS Vu Aggregated County', short.name = 'prep aidsvu aggd county') #For aggregated prep data from AIDS Vu
data.manager$register.source('prep.cdc.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP CDC Aggregated County', short.name = 'prep cdc aggd county') #For aggregated prep data from Atlas Plus (CDC)
data.manager$register.source('lhd', parent.source= "LHD", full.name = "Local Health Department", short.name='lhd') #child
data.manager$register.source('cdc.surveillance.reports', parent.source= "NHSS", full.name = "CDC HIV Surveillance Report", short.name='cdc.surveillance.reports') #child

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
    'cdc',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'aidsvu',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black', 'hispanic', 'white'),
        sex=c('male','female')
    ))

data.manager$register.ontology(
    'cdc.new',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55-64 years', "65+ years"),
        race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))
data.manager$register.ontology(
    'cdc.msa.reports',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
        race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'white'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
        
    ))
data.manager$register.ontology(
    'lhd',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
        race=c('black', 'hispanic', 'other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual')
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

# Pull from Ryan White Data Manager ---------------------------------------
ryan.white.data.manager = load.data.manager("Q:/data_managers/ryan.white.data.manager.rdata")

ryan.white.subset = ryan.white.data.manager$subset.data(outcomes.to.keep = c('non.adap.clients', 'adap.proportion', 'oahs.clients',  'oahs.suppression', 'adap.suppression'),
                                                                            sources.to.keep = c( 'ryan.white.program', 'nastad.adap'),
                                                                            ontologies.to.keep =c('ryan.white.pdfs'),
                                                                            retain.ontology.registrations = F)

# Pull from HIV Surveillance Manager --------------------------------------
surveillance.manager = load.data.manager("Q:/data_managers/surveillance.manager.rdata")

hiv.surveillance.subset = surveillance.manager$subset.data(outcomes.to.keep = c('suppression', 'total.prevalence', 'awareness', 'proportion.tested', 'proportion.tested.n', 'prep', 'diagnosed.prevalence', 'diagnoses'),
                                        sources.to.keep = c('cdc.hiv', 'cdc.aggregated.proportion', 'lhd', 'cdc.aggregated.county', 'brfss', 'aidsvu', 'cdc.prep', 'prep.cdc.aggregated.county', 'prep.aidsvu.aggregated.county', 'cdc.surveillance.reports'),
                                        ontologies.to.keep =c('cdc', 'lhd', 'brfss', 'aidsvu', 'cdc.msa.reports'),
                                        retain.ontology.registrations = F)

# Merge  ------------------------------------------------------------------
hiv.surveillance.subset$import.data(ryan.white.subset)
data.manager = hiv.surveillance.subset

# Save --------------------------------------------------------------------
save(data.manager, file="Q:/data_managers/ryan.white.web.data.manager.rdata")
save(data.manager, file = '../../cached/ryan.white.web.data.manager.rdata')

