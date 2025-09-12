source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATIONS = c("CA","NY","FL","GA","TX","AL","MS","LA","IL","MO","WI")

# for(location in LOCATIONS){
# 
#     simset = retrieve.simulation.set("ehe",location,"final.ehe.state",n.sim = 1000,intervention.code = "noint")
#     save(simset,file=paste0("prelim_results/simset_noint_",Sys.Date(),"_",simset$location,".Rdata"))
#         
# }

for(location in LOCATIONS){
    
    simset = retrieve.simulation.set("ehe",location,"final.ehe.state",n.sim = 1000)
    save(simset,file=paste0("prelim_results/simset_baseline_",Sys.Date(),"_",simset$location,".Rdata"))
    
}