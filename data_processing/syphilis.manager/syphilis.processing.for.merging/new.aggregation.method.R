source('data_processing/aggregate_county_to_msa_new.R') #This aggregates county level data to other locations

# -------------------------------------------------------------------------

 #3-25-26: Parastu wants ps.syphilis and total.syphilis for all msas:
list1_2023 <- read_excel("Q:/data_raw/syphilis.manager/all.msas/list1_2023.xlsx", 
                         sheet = "Unique MSAs")

list1_2023$msa <- paste0("C.", list1_2023$msa)

list1_2023 <- list1_2023%>%
    filter(msa != "C.10380")%>%
    filter(msa != "C.11200")%>%
    filter(msa != "C.11640")%>%
    filter(msa != "C.17410")%>%
    filter(msa != "C.25020")%>%
    filter(msa != "C.28450")%>%
    filter(msa != "C.28880")%>%
    filter(msa != "C.30500")%>%
    filter(msa != "C.32420")%>%
    filter(msa != "C.38660")%>%
    filter(msa != "C.41980")%>%
    filter(msa != "C.43640")%>%
    filter(msa != "C.47930")%>%
    filter(msa != "C.48680")

ALL.MSAS <- list1_2023$msa

# -------------------------------------------------------------------------

put.msa.data.as.new.source.NEW(outcome = 'ps.syphilis.diagnoses',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  ALL.MSAS,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'early.syphilis.diagnoses',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'unknown.duration.or.late.syphilis.diagnoses',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'hiv.diagnosed.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'cdc.prep',
                               to.source.name = 'prep.cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'aidsvu',
                               to.source.name = 'prep.aidsvu.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep.indications',
                               from.source.name = 'cdc.prep.indications',
                               to.source.name = 'prep.indications.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'total.syphilis.diagnoses',
                               from.source.name = 'cdc.summed',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  ALL.MSAS,  #Think of this as containing location 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = syphilis.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')
