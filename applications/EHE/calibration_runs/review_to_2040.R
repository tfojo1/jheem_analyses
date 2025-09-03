
LOCATION = "MO"
load(paste0("prelim_results/final.ehe.state_simset_2025-06-18_",LOCATION,".RData"))

noint = get.null.intervention()
simset = noint$run(simset, start.year=2025, end.year=2040, verbose=T)

save(simset,file=paste0("prelim_results/final.ehe.state_simset_2040_",Sys.Date(),"_",LOCATION,".Rdata"))

simplot(simset$last.sim(),
        simset,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2040)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2040)) 