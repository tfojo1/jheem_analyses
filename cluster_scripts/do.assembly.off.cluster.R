#' @param output.dir Directory within "files/run_results"
do.assembly.off.cluster <- function(output.dir,
                                    locations,
                                    version,
                                    calibration.code,
                                    burn.keep=0.5,
                                    thin.keep=0,
                                    specification.path,
                                    register.calibration.path) {
    
    source(specification.path)
    source(register.calibration.path)
    
    for (location in locations) {
        
        print(paste0("Saving mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
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
        
    }
    print("Done with all locations!")
}