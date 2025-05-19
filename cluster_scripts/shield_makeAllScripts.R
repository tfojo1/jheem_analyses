source("cluster_scripts/batch_script_helpers.R")

location="C.12580"
version="shield"
calibnames=c("elTotal","psTotal","psElTotal","all.totals")

lapply(calibnames,function(x){
    calib.name=paste0("syphilis.9.pk.",x)
    master.name=paste0("shieldMaster.9.pk.",x)
    cat("*** Creating setup, run, setup_master and run_master scripts for ",calib.name," *** \n")
    
    # these are defined in batch_script_helpers.R
    SHIELD.SPEC<- "applications/SHIELD/shield_specification.R"
    SHIELD.REG <- "applications/SHIELD/shield_calib_register.R"
    
    # make.setup.scripts(location,version,calib.name,SHIELD.SPEC, SHIELD.REG,account='pkasaie1')
    # make.run.scripts(location,version,calib.name,1,SHIELD.SPEC, SHIELD.REG,account='pkasaie1', time.hours=72)
    # make.setup.master.script(master.name,location,version,calib.name,overwrite = T)
    # make.run.master.script(master.name,location,version,calib.name,1,overwrite = T)
    
    cat("*** Created all scripts for ",calib.name," *** \n")
})

cat("*** Scripts are complete *** \n")