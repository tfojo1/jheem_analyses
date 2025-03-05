#' @param output.dir Directory within "files/run_results". "run_results" may need to be created.
do.assembly.off.cluster <- function(output.dir,
                                    locations,
                                    calibration.code,
                                    version="ehe",
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

#' @details You probably need to source "../jheem2/R/FILE_MANAGER_file_manager.R" and call set.jheem.root.directory before using this.
#' @param dir Directory within "files/run_results" where the assembled simsets exist locally.
#' @param date The same format as Sys.date(), which was the date the simsets were assembled.
do.save.simsets <- function(locations,
                            version="ehe",
                            calibration.code="full.with.covid2",
                            date="2025-03-04",
                            dir="rw") {
    
    for (location in locations) {
        
        tryCatch({
            print(paste0("Saving location '", location, "'"))
            filename = paste0("../../files/",dir, "/",calibration.code,"_simset_",date,"_",location,".Rdata")
            simset = get(load(filename))
            simset$save()
        },
        # }, error=function(e) {browser()})
        error=function(e) {print(paste0("Error: skipping location '", location, "'"))})
        
    }
    print("All done!")
}