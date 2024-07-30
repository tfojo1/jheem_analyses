source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

# Need to have this file locally
load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-11_C.12580.Rdata')

simplot(simset,
        outcomes = c("diagnosed.prevalence","new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "risk", 
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),  
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),  
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        facet.by = "risk",  
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2010:2025))

simplot(simset,
        facet.by = "race",  
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2010:2025))

simplot(simset,
        outcomes = c("prep.indications","prep.uptake"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2010:2025)) 

simplot(simset,
        outcomes = c("aids.diagnoses","diagnosed.prevalence","new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 1970:2025)) 

simplot(simset,
        facet.by = "risk",  
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1970:2025)) 

simplot(simset,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset,
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset,
        facet.by = "risk", 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset,
        facet.by = "age", 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
