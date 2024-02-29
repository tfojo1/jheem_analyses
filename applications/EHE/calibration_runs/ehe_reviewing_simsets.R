source('../jheem_analyses/applications/EHE/ehe_specification.R')

# load("../jheem_analyses/prelim_results/init.transmission.SIMSET_2024-02-27_C.16980.Rdata") # Chicago
# load("../jheem_analyses/prelim_results/init.transmission.SIMSET_2024-02-27_C.26420.Rdata") # Houston
load("../jheem_analyses/prelim_results/init.transmission.SIMSET_2024-02-27_C.33100.Rdata") # Miami

# is it possible to pull just one sim off of the simset? 
dimnames(simset$diagnosed.prevalence)
# simset[[1]]

simplot(simset,
        facet.by = "risk", # idu and msm-idu undershooting 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2020))

simplot(simset,
        facet.by = "race", # hispanic undershooting (and black historically)
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(simset,
        facet.by = "risk", # heterosexual/msm overshooting
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(simset,
        facet.by = "race", # hispanic undershooting (but small #s)
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(simset,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2020))

simplot(simset,
        facet.by = "risk", # pretty good 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(simset,
        facet.by = "race", # hispanic undershooting (but small #s), other overshooting
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(simset,
        facet.by = "risk", split.by = "race", # black msm overshooting; idu and msm-idu all undershooting
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(simset,
        facet.by = "risk", split.by = "race", # not bad 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(simset,
        facet.by = "age", split.by = "race", # good 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 