JHEEM_DIR = file.path("/scratch4/pkasaie1", Sys.getenv("USER"), "jheem/code/jheem_analyses")
setwd(JHEEM_DIR)  # Set working directory to ensure consistent paths

source('cluster_scripts/batch_script_helpers.R')

MY_LOCATIONS = c("US")
version <- "shield"
calibration.code <- 'pop.demog.shield.US'
specification.path <- file.path(JHEEM_DIR, "applications/SHIELD/shield_specification.R")
register.calibration.path <- file.path(JHEEM_DIR, "applications/SHIELD/calibration/shield_calib_register.R")
overwrite <- TRUE

###### Setup #####
make.setup.scripts(MY_LOCATIONS,
                   version=version,
                   calibration.code=calibration.code,
                   specification.path=specification.path,
                   register.calibration.path=register.calibration.path)


make.setup.master.script(paste0("example_setup_script_",version,"_",calibration.code),
                         locations=MY_LOCATIONS,
                         version=version,
                         calibration.code=calibration.code,
                         overwrite = overwrite)


###### Run #####

chains <- 4

make.run.scripts(MY_LOCATIONS,
                 version=version,
                 calibration.code=calibration.code,
                 specification.path=specification.path,
                 register.calibration.path=register.calibration.path,
                 chains=chains)
make.run.master.script(paste0("example_run_script_",version,"_",calibration.code),
                       locations=MY_LOCATIONS,
                       version=version,
                       calibration.code=calibration.code,
                       chains=chains,
                       overwrite = overwrite)


###### Assemble #####

burn.keep <-  0.6
thin.keep <- 0.2

make.combined.assemble.script(paste0("example_assemble_script_",version,"_",calibration.code),
                              locations=MY_LOCATIONS,
                              version=version,
                              calibration.code=calibration.code,
                              burn.keep = burn.keep,
                              thin.keep = thin.keep,
                              specification.path=specification.path,
                              register.calibration.path=register.calibration.path,
                              overwrite = overwrite)

