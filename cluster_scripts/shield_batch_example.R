JHEEM_DIR = file.path("/scr4-pkasaie1/", Sys.getenv("USER"), "jheem/code/jheem_analyses/")
setwd(JHEEM_DIR)  # Set working directory to ensure consistent paths

source('cluster_scripts/batch_script_helpers.R')

MY_LOCATIONS = c("C.12580")
version <- "shield"
calibration.code <- 'pop.demog.shield'
specification.path <- file.path(JHEEM_DIR, "applications/SHIELD/shield_specification.R")
register.calibration.path <- file.path(JHEEM_DIR, "applications/SHIELD/shield_calib_register.R")
overwrite <- TRUE 

###### Setup #####
# make a script that will setup the calibration 
make.setup.scripts(MY_LOCATIONS,
                   version=version,
                   calibration.code=calibration.code,
                   specification.path=specification.path,
                   register.calibration.path=register.calibration.path) 

# makes the master scripts that calls individual scripts for every location specified above 
make.setup.master.script(paste0("example_setup_script_",version,"_",calibration.code),
                         locations=MY_LOCATIONS,
                         version=version,
                         calibration.code=calibration.code,
                         overwrite = overwrite)


###### Run #####
# how many parallel chains? 
chains <- 1

# make a script that will run the calibration
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
# make a script that will assemble the calibration
burn.keep <-  0 # get rid of first 1-XX%

#this will take place on top of thinining that is set up in the calib-register 
thin.keep <- 0 #e.g., 0.2=keep 1 out of every 5

make.combined.assemble.script(paste0("example_assemble_script_",version,"_",calibration.code),
                              locations=MY_LOCATIONS,
                              version=version,
                              calibration.code=calibration.code,
                              burn.keep = burn.keep,
                              thin.keep = thin.keep,
                              specification.path=specification.path,
                              register.calibration.path=register.calibration.path,
                              overwrite = overwrite)

