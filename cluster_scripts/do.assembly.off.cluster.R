#' @param output.dir Directory within "files/run_results"
do.assembly.off.cluster <- function(output.dir,
                                    locations,
                                    version="ehe",
                                    calibration.code,
                                    burn.keep=150,
                                    thin.keep=0,
                                    specification.path="../jheem_analyses/applications/EHE/ehe_specification.R",
                                    register.calibration.path="../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R") {
    
    source(specification.path)
    source(register.calibration.path)
    
    for (location in locations) {
        tryCatch({print(paste0("Assembling mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
            simset = assemble.simulations.from.calibration(version = version,
                                                           location = location,
                                                           calibration.code = calibration.code)
            
            print(paste0("Burning/thinning..."))
            if (burn.keep!=0)
                simset = simset$burn(keep = burn.keep) #0.5
            if (thin.keep!=0)
                simset = simset$thin(keep = thin.keep)
            
            print(paste0("Saving..."))
            save(simset,file=paste0("../../files/run_results/",output.dir, "/",calibration.code,"_simset_",Sys.Date(),"_",location,".Rdata"))
        }, error=function(e) {print(paste0("Error: skipping location '", location, "'"))})
        
    }
    print("Done with all locations!")
}