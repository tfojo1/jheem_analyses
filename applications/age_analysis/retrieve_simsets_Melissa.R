source('../jheem_analyses/applications/EHE/ehe_specification.R')

LOCATION = "CO"

simset = retrieve.simulation.set("ehe",LOCATION,"final.ehe.state",n.sim = 1000,intervention.code = "noint")
save(simset, file = paste0("prelim_results/simset_noint_",Sys.Date(),"_",LOCATION,".Rdata"))

simset = retrieve.simulation.set("ehe",LOCATION,"final.ehe.state",n.sim = 1000,intervention.code = "baseline")
save(simset, file = paste0("prelim_results/simset_baseline_",Sys.Date(),"_",LOCATION,".Rdata"))
