source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

# load("../jheem_analyses/prelim_results/full.with.covid2_simset_2025-02-09_C.16980.Rdata")
load("../jheem_analyses/prelim_results/full.with.covid2_simset_2025-02-12_C.16980.Rdata")
simset.full = simset

load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2025-02-08_C.16980.Rdata")
simset.trans = simset

load("../jheem_analyses/prelim_results/init.pop.ehe_simset_2025-02-07_C.16980.Rdata")
simset.pop = simset

cbind(head(simset.full$get.mcmc.mixing.statistic(NULL),20))
cbind(head(simset.trans$get.mcmc.mixing.statistic(NULL),20))
cbind(head(simset.pop$get.mcmc.mixing.statistic(NULL),20))

simset.full$traceplot("acute.transmissibility.rr") # worst one in full - stuck 
simset.full$traceplot("age2.msm.hiv.aging.multiplier.0") # stuck 
simset.full$traceplot("hispanic.age2.aging.multiplier.1") # stuck 

simset.trans$traceplot("other.active.idu.initial.prevalence.ratio") # worst one in trans; small movements
simset.pop$traceplot("other.non.idu.general.mortality.rate.multiplie") # worst one in pop; small movements

#full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.16980')
full.lik$compare.sims(simset.trans$last.sim(),simset.full$last.sim())


# not much movement in full, but moving in trans
simplot(simset.full$last.sim(),
        simset.full,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans$last.sim(),
        simset.trans,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 


simplot(simset.full$last.sim(),
        simset.full,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

# not much movement in full, but movement in trans 
simplot(simset.trans$last.sim(),
        simset.trans,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.full$last.sim(),
        simset.full,
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 